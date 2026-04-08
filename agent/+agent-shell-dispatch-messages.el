;;; +agent-shell-dispatch-messages.el --- Messaging protocol for dispatch -*- lexical-binding: t; -*-

;;; Commentary:

;; Typed messaging between subagents and the dispatcher buffer.
;; Uses cl-defgeneric for method dispatch on message struct types.

;;; Code:

(require 'cl-lib)
(require 'map)

(declare-function agent-shell--make-permission-button "agent-shell")
(declare-function agent-shell-diff "agent-shell-diff")
(declare-function agent-shell-queue-request "agent-shell")
(defvar shell-maker--busy)

;; ── Base struct ─────────────────────────────────────────────────────

(cl-defstruct (agent-shell-dispatch-msg
               (:constructor nil)
               (:copier nil))
  "Base message from a subagent."
  agent-buffer
  timestamp)

;; ── Message types ───────────────────────────────────────────────────

(cl-defstruct (agent-shell-dispatch-msg-permission
               (:include agent-shell-dispatch-msg)
               (:constructor agent-shell-dispatch-msg-permission-make)
               (:copier nil))
  "Permission request from a subagent."
  tool-call options respond)

(cl-defstruct (agent-shell-dispatch-msg-input-needed
               (:include agent-shell-dispatch-msg)
               (:constructor agent-shell-dispatch-msg-input-needed-make)
               (:copier nil))
  "Subagent needs input from the dispatcher."
  question context)

(cl-defstruct (agent-shell-dispatch-msg-batch-progress
               (:include agent-shell-dispatch-msg)
               (:constructor agent-shell-dispatch-msg-batch-progress-make)
               (:copier nil))
  "Milestone progress on a batch of work."
  phase completed total)

(cl-defstruct (agent-shell-dispatch-msg-batch-completed
               (:include agent-shell-dispatch-msg)
               (:constructor agent-shell-dispatch-msg-batch-completed-make)
               (:copier nil))
  "Batch of work completed."
  summary)

(cl-defstruct (agent-shell-dispatch-msg-task-progress
               (:include agent-shell-dispatch-msg)
               (:constructor agent-shell-dispatch-msg-task-progress-make)
               (:copier nil))
  "Milestone progress on the overall task."
  phase)

(cl-defstruct (agent-shell-dispatch-msg-task-completed
               (:include agent-shell-dispatch-msg)
               (:constructor agent-shell-dispatch-msg-task-completed-make)
               (:copier nil))
  "Overall task completed."
  summary)

(cl-defstruct (agent-shell-dispatch-msg-error
               (:include agent-shell-dispatch-msg)
               (:constructor agent-shell-dispatch-msg-error-make)
               (:copier nil))
  "Error report from a subagent."
  description context)

;; ── Protocol ────────────────────────────────────────────────────────

(cl-defgeneric agent-shell-dispatch-msg-render (msg)
  "Format MSG body as a propertized string.
Does not include the agent-name frame — that is added by `msg-send'.")

(cl-defgeneric agent-shell-dispatch-msg-handle (msg target-buf)
  "Handle side effects of MSG after rendering in TARGET-BUF.
Default is no-op."
  nil)

(cl-defgeneric agent-shell-dispatch-msg-send (msg target-buf)
  "Send MSG to TARGET-BUF: render, frame, insert, handle."
  (when-let* ((buf (get-buffer target-buf)))
    (let* ((body (agent-shell-dispatch-msg-render msg))
           (agent (agent-shell-dispatch-msg-agent-buffer msg))
           (text (agent-shell-dispatch-msg--frame agent body)))
      (agent-shell-dispatch-msg--insert-before-prompt buf text)
      (agent-shell-dispatch-msg-handle msg target-buf))))

;; ── Shared rendering helpers ────────────────────────────────────────

(defun agent-shell-dispatch-msg--frame (agent-name body)
  "Wrap BODY in a frame with AGENT-NAME header."
  (let ((header (propertize (format "╭─ %s " agent-name)
                            'font-lock-face 'font-lock-comment-face))
        (rule (propertize (make-string 30 ?─)
                          'font-lock-face 'font-lock-comment-face))
        (footer (propertize "╰─" 'font-lock-face 'font-lock-comment-face))
        (bar (propertize "│ " 'font-lock-face 'font-lock-comment-face)))
    (concat "\n" header rule "\n"
            (mapconcat (lambda (line) (concat bar line))
                       (split-string body "\n" t)
                       "\n")
            "\n" footer "\n")))

(defun agent-shell-dispatch-msg--insert-before-prompt (buf text)
  "Insert TEXT into BUF before the prompt, or at point-max if busy."
  (with-current-buffer buf
    (save-excursion
      (let ((inhibit-read-only t))
        (cond
         (shell-maker--busy
          (goto-char (point-max))
          (insert text))
         ((when-let* ((proc (get-buffer-process (current-buffer))))
            (goto-char (process-mark proc))
            (forward-line 0)
            (insert text "\n")
            t))
         (t (goto-char (point-max))
            (insert text)))))))

;; ── Render methods ──────────────────────────────────────────────────

(cl-defmethod agent-shell-dispatch-msg-render
  ((msg agent-shell-dispatch-msg-input-needed))
  "Render input-needed as question with context."
  (let ((q (agent-shell-dispatch-msg-input-needed-question msg))
        (ctx (agent-shell-dispatch-msg-input-needed-context msg)))
    (concat (propertize "❓ " 'font-lock-face 'warning)
            (propertize q 'font-lock-face 'bold)
            (when ctx (concat "\n\n" ctx)))))

(cl-defmethod agent-shell-dispatch-msg-render
  ((msg agent-shell-dispatch-msg-batch-progress))
  "Render batch progress as milestone line."
  (let ((phase (agent-shell-dispatch-msg-batch-progress-phase msg))
        (done (agent-shell-dispatch-msg-batch-progress-completed msg))
        (total (agent-shell-dispatch-msg-batch-progress-total msg)))
    (concat (propertize "📋 " 'font-lock-face 'success)
            (propertize (format "%s (%d/%d)" phase done total)
                        'font-lock-face 'font-lock-function-name-face))))

(cl-defmethod agent-shell-dispatch-msg-render
  ((msg agent-shell-dispatch-msg-batch-completed))
  "Render batch completed as summary."
  (concat (propertize "✅ " 'font-lock-face 'success)
          (propertize "Batch complete: " 'font-lock-face 'bold)
          (agent-shell-dispatch-msg-batch-completed-summary msg)))

(cl-defmethod agent-shell-dispatch-msg-render
  ((msg agent-shell-dispatch-msg-task-progress))
  "Render task progress as phase milestone."
  (concat (propertize "📋 " 'font-lock-face 'success)
          (agent-shell-dispatch-msg-task-progress-phase msg)))

(cl-defmethod agent-shell-dispatch-msg-render
  ((msg agent-shell-dispatch-msg-task-completed))
  "Render task completed as summary."
  (concat (propertize "✅ " 'font-lock-face 'success)
          (propertize "Task complete: " 'font-lock-face 'bold)
          (agent-shell-dispatch-msg-task-completed-summary msg)))

(cl-defmethod agent-shell-dispatch-msg-render
  ((msg agent-shell-dispatch-msg-error))
  "Render error with description and context."
  (let ((desc (agent-shell-dispatch-msg-error-description msg))
        (ctx (agent-shell-dispatch-msg-error-context msg)))
    (concat (propertize "❌ " 'font-lock-face 'error)
            (propertize desc 'font-lock-face 'error)
            (when ctx (concat "\n\n" ctx)))))

(provide '+agent-shell-dispatch-messages)
;;; +agent-shell-dispatch-messages.el ends here
