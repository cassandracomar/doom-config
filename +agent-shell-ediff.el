;;; +agent-shell-ediff.el --- Replace agent-shell-diff with ediff -*- lexical-binding: t; -*-
;; Uses ediff for side-by-side comparison instead of diff-mode.

(require 'ediff)
(eval-when-compile
  (require 'cl-lib))

(defvar agent-shell-diff--on-exit)

(cl-defun +agent-shell-ediff (&key old new on-exit on-accept on-reject title file)
  "Ediff-based replacement for `agent-shell-diff'.
Creates a side-by-side ediff session from OLD and NEW strings.
On quit, prompts to accept or reject changes and fires the
appropriate callback.

Arguments match `agent-shell-diff':
  :OLD       - Original string content
  :NEW       - Modified string content
  :ON-EXIT   - Function called when buffer is killed unexpectedly
  :ON-ACCEPT - Command to accept all changes
  :ON-REJECT - Command to reject all changes
  :TITLE     - Optional title for buffer names
  :FILE      - File path (used for mode detection)"
  (let* ((name (or title (and file (file-name-nondirectory file)) "unknown"))
         (buf-a (generate-new-buffer (format "*old: %s*" name)))
         (buf-b (generate-new-buffer (format "*new: %s*" name)))
         (mode (and file (assoc-default file auto-mode-alist #'string-match)))
         (saved-winconf (current-window-configuration))
         (calling-buffer (current-buffer))
         ctl-buf startup-hook-fn before-setup-hook-fn)

    ;; Fill buffers with content and set mode for syntax highlighting
    (dolist (spec (list (cons buf-a old) (cons buf-b new)))
      (with-current-buffer (car spec)
        (insert (cdr spec))
        (when mode (ignore-errors (funcall mode)))
        (setq buffer-read-only t)))

    ;; Self-removing hooks (following claude-code-ide pattern)
    (setq before-setup-hook-fn
          (lambda ()
            ;; Delete side windows to prevent "Cannot split side window" errors
            (dolist (window (window-list))
              (when (window-parameter window 'window-side)
                (delete-window window)))
            (remove-hook 'ediff-before-setup-hook before-setup-hook-fn)))

    (setq startup-hook-fn
          (lambda ()
            (setq ctl-buf ediff-control-buffer)
            (with-current-buffer ediff-control-buffer
              ;; For agent-shell-diff-kill-buffer compatibility
              (setq-local agent-shell-diff--on-exit on-exit)

              ;; Suppress janitor asking about our temp buffers
              (setq-local ediff-keep-variants t)

              ;; Quit hook chain: prompt → kill temps → ediff cleanup → restore winconf
              (setq-local ediff-quit-hook
                          (list
                           ;; 1. Prompt accept/reject, schedule callback
                           (lambda ()
                             (let ((choice (condition-case nil
                                               (if (y-or-n-p "Accept changes?")
                                                   'accept 'reject)
                                             (quit 'ignore))))
                               ;; Clear on-exit so kill-buffer-hook doesn't double-fire
                               (setq agent-shell-diff--on-exit nil)
                               (run-with-idle-timer
                                0.1 nil
                                (lambda ()
                                  (pcase choice
                                    ('accept (when on-accept (funcall on-accept)))
                                    ('reject (when on-reject (funcall on-reject)))
                                    (_ (message "Ignored")))))))
                           ;; 2. Kill temp buffers before ediff-cleanup-mess displays them
                           (lambda ()
                             (when (buffer-live-p buf-a) (kill-buffer buf-a))
                             (when (buffer-live-p buf-b) (kill-buffer buf-b)))
                           ;; 3. Standard ediff cleanup (kills aux buffers + control buffer)
                           #'ediff-cleanup-mess
                           ;; 4. Restore window configuration
                           (lambda ()
                             (when saved-winconf
                               (ignore-errors
                                 (set-window-configuration saved-winconf))))))

              ;; External/unexpected kill: fire on-exit and clean up ediff artifacts
              (add-hook 'kill-buffer-hook
                        (lambda ()
                          (when (and agent-shell-diff--on-exit
                                     (buffer-live-p calling-buffer))
                            (with-current-buffer calling-buffer
                              (funcall agent-shell-diff--on-exit)))
                          (dolist (buf (list buf-a buf-b))
                            (when (buffer-live-p buf) (kill-buffer buf)))
                          (dolist (var '(ediff-diff-buffer ediff-fine-diff-buffer
                                         ediff-custom-diff-buffer ediff-error-buffer))
                            (when (and (boundp var) (symbol-value var)
                                       (buffer-live-p (symbol-value var)))
                              (kill-buffer (symbol-value var)))))
                        nil t)

              ;; Override q to skip ediff's "Quit?" prompt — straight to "Accept?"
              (define-key ediff-mode-map "q"
                (lambda ()
                  (interactive)
                  (ediff-barf-if-not-control-buffer)
                  (setq this-command 'ediff-quit)
                  (ediff-really-quit nil)))
              (evil-normalize-keymaps))

            (ignore-errors (ediff-next-difference))
            (remove-hook 'ediff-startup-hook startup-hook-fn)))

    ;; Register hooks
    (add-hook 'ediff-before-setup-hook before-setup-hook-fn)
    (add-hook 'ediff-startup-hook startup-hook-fn)

    ;; Start ediff
    (condition-case err
        (let ((old-setup-fn ediff-window-setup-function)
              (old-split-fn ediff-split-window-function)
              (ediff-control-buffer-suffix (format "<%s>" name)))
          (unwind-protect
              (progn
                (setq ediff-window-setup-function #'ediff-setup-windows-plain
                      ediff-split-window-function #'split-window-horizontally)
                (ediff-buffers buf-a buf-b))
            (setq ediff-window-setup-function old-setup-fn
                  ediff-split-window-function old-split-fn)))
      (error
       (when (buffer-live-p buf-a) (kill-buffer buf-a))
       (when (buffer-live-p buf-b) (kill-buffer buf-b))
       (remove-hook 'ediff-before-setup-hook before-setup-hook-fn)
       (remove-hook 'ediff-startup-hook startup-hook-fn)
       (signal (car err) (cdr err))))

    ctl-buf))

(advice-add 'agent-shell-diff :override #'+agent-shell-ediff)

(provide '+agent-shell-ediff)
;;; +agent-shell-ediff.el ends here
