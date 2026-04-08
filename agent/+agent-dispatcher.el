;;; +agent-dispatcher.el -*- lexical-binding: t; -*-

;; Permission forwarding, progress polling, and agent spawning for
;; agent-shell dispatch workflows.

(require 'cl-lib)
(require 'map)


(declare-function agent-shell--start "agent-shell")
(declare-function agent-shell-anthropic-make-claude-code-config "agent-shell")
(defvar agent-shell--state)
(defvar shell-maker--busy)

;; ── Dispatcher structs ──────────────────────────────────────────────

(cl-defstruct (agent-shell-dispatch-state
               (:constructor agent-shell-dispatch-state-make)
               (:copier nil))
  "Active dispatch session state."
  dispatcher-buffer tasks statuses)

(cl-defstruct (agent-shell-dispatch-reported-status
               (:constructor agent-shell-dispatch-reported-status-make)
               (:copier nil))
  "Raw status report from an agent via MCP."
  status detail updated started)

(cl-defstruct (agent-shell-dispatch-resolved-status
               (:constructor agent-shell-dispatch-resolved-status-make)
               (:copier nil))
  "Computed effective status after inspecting agent buffer state."
  effective elapsed detail started)

;; -- Permission forwarding from background agents to dispatcher buffer --

(defvar agent-shell-dispatch--primary-buffer nil
  "Buffer name of the primary (dispatcher) shell for permission rendering.")

(defvar agent-shell-dispatch--state nil
  "State for the dispatch progress polling timer.")

(defun agent-shell-dispatch-forward-permission (permission)
  "Forward PERMISSION from a background agent via the messaging protocol."
  (when-let* ((target agent-shell-dispatch--primary-buffer))
    (agent-shell-dispatch-msg-send
     (agent-shell-dispatch-msg-permission-make
      :agent-buffer (buffer-name)
      :timestamp (current-time)
      :tool-call (map-elt permission :tool-call)
      :options (map-elt permission :options)
      :respond (map-elt permission :respond))
     target)
    t))

;; -- Dispatch task graph and progress rendering --

(defun agent-shell-dispatch--clear-state ()
  "Clear dispatch state. Used as teardown hook."
  (setq agent-shell-dispatch--state nil))

(defun agent-shell-dispatch--format-elapsed (start-time)
  "Format elapsed time since START-TIME as a human-readable string."
  (let ((elapsed (floor (float-time (time-subtract nil start-time)))))
    (cond
     ((< elapsed 60) (format "%ds" elapsed))
     ((< elapsed 3600) (format "%dm%02ds" (/ elapsed 60) (% elapsed 60)))
     (t (format "%dh%02dm" (/ elapsed 3600) (% (/ elapsed 60) 60))))))

(defun agent-shell-dispatch-report (task-id status &optional detail)
  "Report STATUS for TASK-ID. Called by agents via MCP.
STATUS is a string: \"working\", \"done\", \"error\".
DETAIL is an optional description of current activity."
  (when-let* ((state agent-shell-dispatch--state)
              (statuses (agent-shell-dispatch-state-statuses state)))
    (let ((existing (gethash task-id statuses)))
      (puthash task-id
               (agent-shell-dispatch-reported-status-make
                :status (intern status)
                :detail detail
                :updated (current-time)
                :started (or (and existing (agent-shell-dispatch-reported-status-started existing))
                             (current-time)))
               statuses))))



(defun agent-shell-dispatch--resolve-status (task statuses)
  "Determine effective status for TASK given STATUSES hash.
Returns a agent-shell-dispatch-resolved-status struct."
  (let* ((id (plist-get task :id))
         (agent-buf (plist-get task :agent))
         (buf (get-buffer agent-buf))
         (alive (and buf (get-buffer-process buf)))
         (busy (and buf (buffer-local-value 'shell-maker--busy buf)))
         (perm (member agent-buf agent-shell-dispatch-msg--pending-permission-agents))
         (reported (gethash id statuses))
         (rep-status (and reported (agent-shell-dispatch-reported-status-status reported)))
         (rep-detail (and reported (agent-shell-dispatch-reported-status-detail reported)))
         (started (and reported (agent-shell-dispatch-reported-status-started reported)))
         (effective (cond
                     ((eq rep-status 'done) 'done)
                     ((eq rep-status 'error) 'error)
                     (perm 'permission)
                     ((and alive busy) 'working)
                     ((eq rep-status 'working) 'working)
                     ((and alive (not busy) started) 'done)
                     (alive 'waiting)
                     (t 'dead))))
    ;; Record start time on first working state
    (when (and (eq effective 'working) (not started))
      (puthash id (agent-shell-dispatch-reported-status-make
                   :status 'working :detail rep-detail
                   :updated (current-time) :started (current-time))
               statuses)
      (setq started (current-time)))
    (agent-shell-dispatch-resolved-status-make
     :effective effective
     :elapsed (if started (agent-shell-dispatch--format-elapsed started) nil)
     :detail (and rep-detail (memq effective '(working permission)) rep-detail)
     :started started)))


(defun agent-shell-dispatch--build-status-map ()
  "Build a status-map hash from current dispatch state.
Returns a hash of id → `agent-shell-dispatch-render-task-status', or nil."
  (when-let* ((state agent-shell-dispatch--state)
              (tasks (agent-shell-dispatch-state-tasks state))
              (statuses (agent-shell-dispatch-state-statuses state)))
    (let ((sm (make-hash-table :test 'equal)))
      (dolist (task tasks)
        (let* ((resolved (agent-shell-dispatch--resolve-status task statuses))
               (id (plist-get task :id)))
          (puthash id (agent-shell-dispatch-render-task-status-make
                       :status (agent-shell-dispatch-resolved-status-effective resolved)
                       :elapsed (agent-shell-dispatch-resolved-status-elapsed resolved)
                       :detail (agent-shell-dispatch-resolved-status-detail resolved))
                   sm)))
      sm)))

(defun agent-shell-dispatch-start (dispatcher-buffer tasks &optional _interval)
  "Start the dispatch task graph in the agent-shell header.
DISPATCHER-BUFFER is the dispatcher's agent-shell buffer name.
TASKS is a list of plists: ((:id ID :name NAME :agent AGENT-BUF) ...)."
  (agent-shell-dispatch-render-teardown)
  (setq agent-shell-dispatch-msg--pending-permission-agents nil)
  ;; Normalize :agent — default to dispatcher buffer if missing or not a string
  (let* ((normalized (mapcar (lambda (task)
                               (let ((agent (plist-get task :agent)))
                                 (if (stringp agent) task
                                   (plist-put (copy-sequence task) :agent dispatcher-buffer))))
                             tasks))
         (task-defs (mapcar (lambda (task)
                              (agent-shell-dispatch-render-task-make
                               :id (plist-get task :id)
                               :name (plist-get task :name)
                               :depends-on (plist-get task :depends-on)))
                            normalized)))
    (setq agent-shell-dispatch--state
          (agent-shell-dispatch-state-make
           :dispatcher-buffer dispatcher-buffer
           :tasks normalized
           :statuses (make-hash-table :test 'equal)))
    ;; Set up render module
    (add-hook 'agent-shell-dispatch-render-teardown-hook
              #'agent-shell-dispatch--clear-state)
    (agent-shell-dispatch-render-set-tasks task-defs)
    (setq agent-shell-dispatch-render-buffer dispatcher-buffer
          agent-shell-dispatch-render-status-function #'agent-shell-dispatch--build-status-map
          agent-shell-dispatch-render-header-function #'agent-shell--update-header-and-mode-line
          agent-shell-dispatch-render-reset-function (lambda ()
                                                       (when (boundp 'agent-shell--header-cache)
                                                         (setq agent-shell--header-cache nil))
                                                       (agent-shell--update-header-and-mode-line))
          agent-shell-dispatch-render-busy-p-function (lambda () shell-maker--busy)
          agent-shell-dispatch-render-advice-target 'agent-shell--update-header-and-mode-line)
    ;; Enable render mode in dispatcher buffer
    (with-current-buffer (get-buffer dispatcher-buffer)
      (agent-shell-dispatch-render-mode 1))))


(defun agent-shell-dispatch-stop ()
  "Stop rendering. State is preserved for `agent-shell-dispatch-render-mode' toggle."
  (when agent-shell-dispatch-render-mode
    (agent-shell-dispatch-render-mode -1)))

;; Backward-compat aliases for old skill API
(defun agent-shell-dispatch-start-progress-polling (dispatcher-buffer agents &optional interval)
  "Backward-compat wrapper. Converts alist to task plist format."
  (agent-shell-dispatch-start dispatcher-buffer
                              (cl-loop for agent in agents
                                       for i from 1
                                       collect (list :id (format "task-%d" i)
                                                     :name (cdr agent)
                                                     :agent (car agent)))
                              interval))

(defun agent-shell-dispatch-kill-agents ()
  "Kill all dispatched agent buffers.
Also stops dispatch polling. Returns the number of agents killed."
  (let ((tasks (and agent-shell-dispatch--state (agent-shell-dispatch-state-tasks agent-shell-dispatch--state)))
        (self (buffer-name)))
    (agent-shell-dispatch-stop)
    (cl-loop for task in tasks
             for buf-name = (plist-get task :agent)
             unless (or (null buf-name) (equal buf-name self))
             do (when-let* ((buf (get-buffer buf-name))
                            (proc (get-buffer-process buf)))
                  (set-process-query-on-exit-flag proc nil)
                  (delete-process proc))
             (when-let* ((buf (get-buffer buf-name)))
               (kill-buffer buf))
             and count t)))

;; -- Start function for spawned agents --

(defun agent-shell-dispatch-start-agent (config _arg &optional buffer-name)
  "Start a new Claude agent-shell for dispatch.
No window popup, no session prompt. Copies the session mode from the
primary (dispatcher) buffer. Permissions are rendered in the dispatcher buffer.
BUFFER-NAME, if provided, is incorporated into the buffer label."
  (let* ((cfg (copy-alist config))
         (mode-id (or (when-let* ((primary agent-shell-dispatch--primary-buffer)
                                  (pbuf (get-buffer primary)))
                        (with-current-buffer pbuf
                          (map-nested-elt agent-shell--state '(:session :mode-id))))
                      "default"))
         (buf nil))
    (setf (map-elt cfg :default-session-mode-id)
          (lambda () mode-id))
    (setf (map-elt cfg :buffer-name)
          (if buffer-name
              (format "[agent:%s]" buffer-name)
            "[agent]"))
    (setq buf (agent-shell--start :config cfg
                                  :no-focus t
                                  :new-session t
                                  :session-strategy 'new))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq-local agent-shell-permission-responder-function
                    #'agent-shell-dispatch-forward-permission)))
    buf))
