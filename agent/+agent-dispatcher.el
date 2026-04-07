;;; +agent-dispatcher.el -*- lexical-binding: t; -*-

;; Permission forwarding, progress polling, and agent spawning for
;; meta-agent-shell dispatch workflows.

(require 'cl-lib)
(require 'map)
(require 'svg)

(declare-function agent-shell--make-permission-button "agent-shell")
(declare-function agent-shell--start "agent-shell")
(declare-function agent-shell-anthropic-make-claude-code-config "agent-shell")
(declare-function agent-shell-ui-delete-fragment "agent-shell")
(declare-function agent-shell-ui-make-fragment-model "agent-shell")
(declare-function agent-shell-ui-update-fragment "agent-shell")
(defvar agent-shell--state)
(defvar shell-maker--busy)

;; -- Permission forwarding from background agents to dispatcher buffer --

(defvar +meta-agent-shell--primary-buffer nil
  "Buffer name of the primary (dispatcher) shell for permission rendering.")

(defvar +meta-agent-shell--pending-permission-agents nil
  "List of agent buffer names with unresolved permission dialogs.")

(defvar +meta-agent-shell--dispatch-state nil
  "State for the dispatch progress polling timer.")

(defun +meta-agent-shell--cleanup-permission (perm-id target-buf agent-buf option-id)
  "Remove the permission dialog identified by PERM-ID from TARGET-BUF."
  (when-let* ((buf (get-buffer target-buf)))
    (with-current-buffer buf
      (save-excursion
        (let ((inhibit-read-only t))
          (goto-char (point-min))
          (when-let* ((match (text-property-search-forward
                              '+meta-agent-shell-perm-id perm-id t)))
            (let ((start (prop-match-beginning match))
                  (end (prop-match-end match)))
              (delete-region start end)
              (goto-char start)
              (insert (propertize
                       (format "    ✓ %s — %s\n" agent-buf option-id)
                       'font-lock-face 'font-lock-comment-face
                       'read-only t)))))))))

(defun +meta-agent-shell--insert-before-prompt (buf text)
  "Insert TEXT into BUF before the prompt, or at point-max if busy."
  (with-current-buffer buf
    (save-excursion
      (let ((inhibit-read-only t))
        (if shell-maker--busy
            (progn (goto-char (point-max)) (insert text))
          (if-let* ((proc (get-buffer-process (current-buffer))))
              (progn
                (goto-char (process-mark proc))
                (forward-line 0)
                (insert text "\n"))
            (goto-char (point-max))
            (insert text)))))))

(defun +meta-agent-shell-forward-permission (permission)
  "Render PERMISSION from a background agent in the dispatcher buffer.
Uses agent-shell's native button maker for full navigation compatibility."
  (when-let* ((tool-call (map-elt permission :tool-call))
              (options (map-elt permission :options))
              (respond (map-elt permission :respond))
              (title (or (map-elt tool-call :title) "unknown"))
              (kind (or (map-elt tool-call :kind) "unknown"))
              (agent-buf (buffer-name))
              (target (and +meta-agent-shell--primary-buffer
                           (get-buffer +meta-agent-shell--primary-buffer))))
    (cl-pushnew agent-buf +meta-agent-shell--pending-permission-agents :test #'equal)
    (let* ((perm-id (format "perm-%s-%s" agent-buf (random)))
           (keymap (make-sparse-keymap))
           (buttons
            (mapconcat
             (lambda (opt)
               (let* ((option-id (map-elt opt :option-id))
                      (action (lambda ()
                                (interactive)
                                (funcall respond option-id)
                                (setq +meta-agent-shell--pending-permission-agents
                                      (delete agent-buf +meta-agent-shell--pending-permission-agents))
                                (+meta-agent-shell--cleanup-permission
                                 perm-id
                                 (buffer-name target)
                                 agent-buf
                                 option-id))))
                 (when-let* ((char-str (map-elt opt :char)))
                   (define-key keymap (kbd char-str) action))
                 (agent-shell--make-permission-button
                  :text (map-elt opt :label)
                  :help (map-elt opt :label)
                  :action action
                  :keymap keymap
                  :char (map-elt opt :char)
                  :option (or (map-elt opt :option)
                              (map-elt opt :label))
                  :navigatable t)))
             options
             " "))
           (text (format "\n╭─\n\n    %s %s %s\n\n\n    %s\n\n\n    %s\n\n\n╰─\n"
                         (propertize "⚠" 'font-lock-face 'warning)
                         (propertize (format "Permission: %s" agent-buf)
                                     'font-lock-face 'bold)
                         (propertize "⚠" 'font-lock-face 'warning)
                         (propertize (format "%s (%s)" title kind)
                                     'font-lock-face 'comint-highlight-input)
                         buttons)))
      (put-text-property 0 (length text) 'keymap keymap text)
      (put-text-property 0 (length text) '+meta-agent-shell-perm-id perm-id text)
      (+meta-agent-shell--insert-before-prompt target text))
    t))

;; -- Dispatch task graph and progress rendering --

(defvar +dispatch--spinner-frames
  '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏")
  "Braille spinner animation frames.")

(defvar +dispatch--spinner-index 0
  "Current spinner frame index.")

(defun +dispatch--format-elapsed (start-time)
  "Format elapsed time since START-TIME as a human-readable string."
  (let ((elapsed (floor (float-time (time-subtract nil start-time)))))
    (cond
     ((< elapsed 60) (format "%ds" elapsed))
     ((< elapsed 3600) (format "%dm%02ds" (/ elapsed 60) (% elapsed 60)))
     (t (format "%dh%02dm" (/ elapsed 3600) (% (/ elapsed 60) 60))))))

(defun +dispatch-report (task-id status &optional detail)
  "Report STATUS for TASK-ID. Called by agents via MCP.
STATUS is a string: \"working\", \"done\", \"error\".
DETAIL is an optional description of current activity."
  (when-let* ((state +meta-agent-shell--dispatch-state)
              (statuses (plist-get state :statuses)))
    (let ((existing (gethash task-id statuses)))
      (puthash task-id
               (list :status (intern status)
                     :detail detail
                     :updated (current-time)
                     :started (or (plist-get existing :started) (current-time)))
               statuses))))

(defvar +dispatch--svg-colors
  '((done       :bg "#2a4a2a" :fg "#b6e63e" :icon "✓")
    (working    :bg "#4a3a1a" :fg "#fd971f" :icon "⠹")
    (permission :bg "#4a1a2a" :fg "#f92672" :icon "🔒")
    (waiting    :bg "#2a2a2a" :fg "#75715e" :icon "◦")
    (error      :bg "#4a1a1a" :fg "#f92672" :icon "✗")
    (dead       :bg "#1a1a1a" :fg "#555555" :icon "?"))
  "Color scheme for dispatch SVG task graph.")

(defun +dispatch--build-svg (tasks-info)
  "Build an SVG image from TASKS-INFO, a list of plists.
Each entry: (:name NAME :status STATUS :elapsed STR :detail STR-OR-NIL)."
  (let* ((row-h 38) (detail-h 16) (pad 6) (w 440) (font "Iosevka Nerd Font")
         (total-h (+ pad (cl-loop for info in tasks-info
                                  sum (+ row-h (if (plist-get info :detail) detail-h 0) pad))))
         (svg (svg-create w total-h))
         (y pad))
    (svg-rectangle svg 0 0 w total-h :fill "#1a1a2e" :rx 8)
    (dolist (info tasks-info)
      (let* ((status (plist-get info :status))
             (name (plist-get info :name))
             (elapsed (or (plist-get info :elapsed) ""))
             (detail (plist-get info :detail))
             (colors (cdr (assq status +dispatch--svg-colors)))
             (bg (plist-get colors :bg))
             (fg (plist-get colors :fg))
             (icon (plist-get colors :icon))
             (box-h (+ row-h (if detail detail-h 0))))
        (svg-rectangle svg pad y (- w (* 2 pad)) box-h :fill bg :rx 5)
        (svg-text svg (format "%s  %s" icon name)
                  :x (+ pad 10) :y (+ y 24)
                  :fill fg :font-size 14 :font-family font)
        (when (> (length elapsed) 0)
          (svg-text svg elapsed
                    :x (- w pad 12) :y (+ y 24)
                    :fill "#75715e" :font-size 12 :font-family font
                    :text-anchor "end"))
        (when detail
          (svg-text svg (format "└ %s" detail)
                    :x (+ pad 26) :y (+ y 24 detail-h)
                    :fill "#75715e" :font-size 11 :font-family font))
        (setq y (+ y box-h pad))))
    svg))

(defun +dispatch--resolve-status (task statuses)
  "Determine effective status for TASK given STATUSES hash.
Returns (STATUS ELAPSED DETAIL STARTED) list."
  (let* ((id (plist-get task :id))
         (agent-buf (plist-get task :agent))
         (buf (get-buffer agent-buf))
         (alive (and buf (get-buffer-process buf)))
         (busy (and buf (buffer-local-value 'shell-maker--busy buf)))
         (perm (member agent-buf +meta-agent-shell--pending-permission-agents))
         (reported (gethash id statuses))
         (rep-status (plist-get reported :status))
         (rep-detail (plist-get reported :detail))
         (started (plist-get reported :started))
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
      (puthash id (list :status 'working :detail rep-detail
                        :updated (current-time) :started (current-time))
               statuses)
      (setq started (current-time)))
    (list effective
          (if started (+dispatch--format-elapsed started) nil)
          (and rep-detail (memq effective '(working permission)) rep-detail)
          started)))

(defun +dispatch--render ()
  "Render the dispatch task graph as an SVG in a progress fragment.
Combines agent-reported status with shell-maker--busy fallback."
  (when-let* ((state +meta-agent-shell--dispatch-state)
              (dispatcher (plist-get state :dispatcher-buffer))
              (tasks (plist-get state :tasks))
              (statuses (plist-get state :statuses))
              ((get-buffer dispatcher)))
    ;; Cycle spinner
    (cl-incf +dispatch--spinner-index)
    (let ((spinner (nth (% +dispatch--spinner-index (length +dispatch--spinner-frames))
                        +dispatch--spinner-frames)))
      ;; Update spinner icon in color scheme
      (plist-put (cdr (assq 'working +dispatch--svg-colors)) :icon spinner))
    ;; Resolve statuses and build render data
    (let* ((total (length tasks))
           (ready-count 0) (busy-count 0)
           (render-data
            (cl-loop for task in tasks
                     for (effective elapsed detail _started)
                       = (+dispatch--resolve-status task statuses)
                     when (eq effective 'done) do (cl-incf ready-count)
                     when (memq effective '(working permission)) do (cl-incf busy-count)
                     collect (list :name (plist-get task :name)
                                   :status effective
                                   :elapsed elapsed
                                   :detail detail)))
           (svg (+dispatch--build-svg render-data))
           (img (svg-image svg :scale 1.0))
           (body (propertize "dispatch-graph" 'display img)))
      (with-current-buffer dispatcher
        (agent-shell-ui-update-fragment
         (agent-shell-ui-make-fragment-model
          :namespace-id "dispatch-progress"
          :block-id "status"
          :label-left (propertize " Dispatch" 'font-lock-face 'font-lock-keyword-face)
          :label-right (propertize
                        (cond
                         ((= ready-count total) (format "[%d/%d ✓ complete]" ready-count total))
                         ((> busy-count 0) (format "[%d/%d done, %d active]" ready-count total busy-count))
                         (t (format "[%d/%d done]" ready-count total)))
                        'font-lock-face
                        (if (= ready-count total) 'success 'font-lock-comment-face))
          :body body)
         :expanded t)))))

(defun +dispatch-start (dispatcher-buffer tasks &optional interval)
  "Start the dispatch task graph.
DISPATCHER-BUFFER is the dispatcher's agent-shell buffer name.
TASKS is a list of plists: ((:id ID :name NAME :agent AGENT-BUF) ...).
INTERVAL is seconds between render updates (default 2)."
  (+dispatch-stop)
  (setq +meta-agent-shell--pending-permission-agents nil)
  (with-current-buffer dispatcher-buffer
    (ignore-errors
      (agent-shell-ui-delete-fragment
       :namespace-id "dispatch-progress"
       :block-id "status")))
  (let ((interval (or interval 2)))
    (setq +meta-agent-shell--dispatch-state
          (list :dispatcher-buffer dispatcher-buffer
                :tasks tasks
                :statuses (make-hash-table :test 'equal)
                :timer (run-with-timer 1 interval #'+dispatch--render)))))

(defun +dispatch-stop ()
  "Stop the dispatch render timer."
  (when-let* ((state +meta-agent-shell--dispatch-state)
              (timer (plist-get state :timer)))
    (cancel-timer timer))
  (setq +meta-agent-shell--dispatch-state nil))

;; Backward-compat aliases for old skill API
(defun +meta-agent-shell-start-progress-polling (dispatcher-buffer agents &optional interval)
  "Backward-compat wrapper. Converts alist to task plist format."
  (+dispatch-start dispatcher-buffer
                   (cl-loop for agent in agents
                            for i from 1
                            collect (list :id (format "task-%d" i)
                                         :name (cdr agent)
                                         :agent (car agent)))
                   interval))
(defalias '+meta-agent-shell-stop-progress-polling #'+dispatch-stop)

(defun +meta-agent-shell-kill-agents ()
  "Kill all agent-shell sessions except the current buffer.
Returns the number of agents killed."
  (let ((self (buffer-name))
        (killed 0))
    (dolist (session (meta-agent-shell-list-sessions))
      (let ((buf-name (plist-get session :buffer)))
        (unless (equal buf-name self)
          (when-let* ((buf (get-buffer buf-name))
                      (proc (get-buffer-process buf)))
            (set-process-query-on-exit-flag proc nil)
            (delete-process proc))
          (when (get-buffer buf-name)
            (kill-buffer buf-name))
          (cl-incf killed))))
    killed))

;; -- Start function for spawned agents --

(defun +meta-agent-shell-start (_arg &optional _buffer-name)
  "Start a new Claude agent-shell for meta-agent-shell.
No window popup, no session prompt. Copies the session mode from the
primary (dispatcher) buffer. Permissions are rendered in the dispatcher buffer."
  (let* ((config (copy-alist (agent-shell-anthropic-make-claude-code-config)))
         (mode-id (or (when-let* ((primary +meta-agent-shell--primary-buffer)
                                  (pbuf (get-buffer primary)))
                        (with-current-buffer pbuf
                          (map-nested-elt agent-shell--state '(:session :mode-id))))
                      "default"))
         (buf nil))
    (setf (map-elt config :default-session-mode-id)
          (lambda () mode-id))
    (setq buf (agent-shell--start :config config
                                  :no-focus t
                                  :new-session t
                                  :session-strategy 'new))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq-local agent-shell-permission-responder-function
                    #'+meta-agent-shell-forward-permission)))
    buf))
