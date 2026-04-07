;;; +agent-dispatcher.el -*- lexical-binding: t; -*-

;; Permission forwarding, progress polling, and agent spawning for
;; meta-agent-shell dispatch workflows.

(require 'cl-lib)
(require 'map)

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

;; -- Dispatch progress polling --

(defun +meta-agent-shell--poll-progress ()
  "Check agent statuses and update the dispatch progress fragment.
Uses shell-maker--busy directly to detect active output."
  (when-let* ((state +meta-agent-shell--dispatch-state)
              (dispatcher (plist-get state :dispatcher-buffer))
              (agents (plist-get state :agents))
              ((get-buffer dispatcher)))
    (let ((seen-busy (plist-get state :seen-busy)))
      (cl-loop with total = (length agents)
               for agent in agents
               for buf-name = (car agent)
               for task = (cdr agent)
               for buf = (get-buffer buf-name)
               for alive = (and buf (get-buffer-process buf))
               for busy = (and buf (buffer-local-value 'shell-maker--busy buf))
               for perm = (member buf-name +meta-agent-shell--pending-permission-agents)
               when (and alive busy)
                 do (cl-pushnew buf-name seen-busy :test #'equal)
                 and count t into busy-count
                 and collect (format "  ⏳ %s — %s" task buf-name) into lines
               else when perm
                 count t into busy-count
                 and collect (format "  🔒 %s — %s (permission)" task buf-name) into lines
               else when (and alive (not busy) (member buf-name seen-busy))
                 count t into ready-count
                 and collect (format "  ✓ %s — %s" task buf-name) into lines
               else when (and alive (not busy))
                 collect (format "  ⏸ %s — %s (waiting)" task buf-name) into lines
               else
                 collect (format "  ✗ %s — %s (dead)" task buf-name) into lines
               finally do
               (plist-put state :seen-busy seen-busy)
               (with-current-buffer dispatcher
                 (agent-shell-ui-update-fragment
                  (agent-shell-ui-make-fragment-model
                   :namespace-id "dispatch-progress"
                   :block-id "status"
                   :label-left (propertize " Dispatch" 'font-lock-face 'font-lock-keyword-face)
                   :label-right (propertize
                                 (cond
                                  ((= (or ready-count 0) total)
                                   (format "[%d/%d ✓ ALL COMPLETE]" (or ready-count 0) total))
                                  ((> (or busy-count 0) 0)
                                   (format "[%d/%d done, %d working]"
                                           (or ready-count 0) total (or busy-count 0)))
                                  (t
                                   (format "[%d/%d done]" (or ready-count 0) total)))
                                 'font-lock-face
                                 (if (= (or ready-count 0) total)
                                     'success
                                   'font-lock-comment-face))
                   :body (string-join lines "\n"))
                  :expanded t))))))

(defun +meta-agent-shell-start-progress-polling (dispatcher-buffer agents &optional interval)
  "Start polling agent progress.
Deletes any stale fragment, creates a fresh one, then starts the timer.
DISPATCHER-BUFFER is where the fragment renders.
AGENTS is an alist of (buffer-name . task-description).
INTERVAL is seconds between polls (default 3)."
  (+meta-agent-shell-stop-progress-polling)
  (setq +meta-agent-shell--pending-permission-agents nil)
  (with-current-buffer dispatcher-buffer
    (ignore-errors
      (agent-shell-ui-delete-fragment
       :namespace-id "dispatch-progress"
       :block-id "status")))
  (let ((interval (or interval 3)))
    (setq +meta-agent-shell--dispatch-state
          (list :dispatcher-buffer dispatcher-buffer
                :agents agents
                :seen-busy nil
                :timer (run-with-timer 1 interval #'+meta-agent-shell--poll-progress)))))

(defun +meta-agent-shell-stop-progress-polling ()
  "Stop polling and clean up the timer."
  (when-let* ((state +meta-agent-shell--dispatch-state)
              (timer (plist-get state :timer)))
    (cancel-timer timer))
  (setq +meta-agent-shell--dispatch-state nil))

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
