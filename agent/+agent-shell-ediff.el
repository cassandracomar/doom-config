;;; +agent-shell-ediff.el --- Replace agent-shell-diff with ediff -*- lexical-binding: t; -*-
;; Uses ediff for side-by-side comparison instead of diff-mode.

(require 'ediff)
(eval-when-compile
  (require 'cl-lib))

(defvar agent-shell-diff--on-exit)

(defvar +agent-shell-ediff--scroll-timer nil
  "Timer for ediff scroll sync polling.")

(defun +agent-shell-ediff--sync-line (from-buf from-start to-buf to-win)
  "Set TO-WIN's start to the same line as FROM-START in FROM-BUF."
  (set-window-start to-win
    (with-current-buffer to-buf
      (save-excursion
        (goto-char (point-min))
        (forward-line (1- (with-current-buffer from-buf
                            (line-number-at-pos from-start t))))
        (point)))))

(defun +agent-shell-ediff--start-scroll-sync (buf-a buf-b)
  "Start a timer that syncs scroll position between BUF-A and BUF-B."
  (+agent-shell-ediff--stop-scroll-sync)
  (let ((last-a nil) (last-b nil))
    (setq +agent-shell-ediff--scroll-timer
          (run-with-timer 0.05 0.05
            (lambda ()
              (if (not (and (buffer-live-p buf-a) (buffer-live-p buf-b)))
                  (+agent-shell-ediff--stop-scroll-sync)
                (when-let* ((win-a (get-buffer-window buf-a))
                            (win-b (get-buffer-window buf-b))
                            (start-a (window-start win-a))
                            (start-b (window-start win-b)))
                  (cond
                   ((and last-a (/= start-a last-a))
                    (+agent-shell-ediff--sync-line buf-a start-a buf-b win-b))
                   ((and last-b (/= start-b last-b))
                    (+agent-shell-ediff--sync-line buf-b start-b buf-a win-a)))
                  (setq last-a (window-start win-a)
                        last-b (window-start win-b)))))))))

(defun +agent-shell-ediff--stop-scroll-sync ()
  "Stop the ediff scroll sync timer."
  (when (timerp +agent-shell-ediff--scroll-timer)
    (cancel-timer +agent-shell-ediff--scroll-timer)
    (setq +agent-shell-ediff--scroll-timer nil)))

(defvar +agent-shell-ediff--bg-only-faces
  '(ediff-current-diff-A ediff-current-diff-B
    ediff-fine-diff-A ediff-fine-diff-B
    ediff-even-diff-A ediff-even-diff-B
    ediff-odd-diff-A ediff-odd-diff-B)
  "Ediff faces to reduce to background-only in agent-shell sessions.")

(defun +agent-shell-ediff--strip-overlay-fg (&rest _)
  "Replace ediff overlay faces with background-only anonymous specs.
This preserves font-lock syntax foreground colors while still
showing diff-region background highlighting."
  (dolist (buf (list ediff-buffer-A ediff-buffer-B))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (cl-loop for ov in (overlays-in (point-min) (point-max))
                 for face = (overlay-get ov 'face)
                 when (and (symbolp face)
                           (memq face +agent-shell-ediff--bg-only-faces))
                 do (when-let* ((bg (face-background face nil t)))
                      (overlay-put ov 'face (list :background bg :extend t))
                      (overlay-put ov 'priority (or (overlay-get ov 'priority) 10))))))))

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
         ;; Try to build full-file buffers: read file from disk for
         ;; the old content, replace the old hunk with the new hunk
         ;; to produce the new content.  Fall back to the raw
         ;; old/new hunks if the file can't be read or the hunk
         ;; can't be located.
         (full-old (when (and file (file-readable-p file))
                     (with-temp-buffer
                       (insert-file-contents file)
                       (buffer-string))))
         (full-new (when full-old
                     (with-temp-buffer
                       (insert full-old)
                       (goto-char (point-min))
                       (when (search-forward old nil t)
                         (replace-match new t t)
                         (buffer-string)))))
         (old-content (or full-old old))
         (new-content (or full-new new))
         (buf-a (generate-new-buffer (format "*old: %s*" name)))
         (buf-b (generate-new-buffer (format "*new: %s*" name)))
         (mode (and file (assoc-default file auto-mode-alist #'string-match)))
         (saved-winconf (current-window-configuration))
         (calling-buffer (current-buffer))
         ctl-buf startup-hook-fn before-setup-hook-fn)

    ;; Fill buffers with content and set mode for syntax highlighting
    (cl-loop for (buf . content) in (list (cons buf-a old-content)
                                          (cons buf-b new-content))
             do (with-current-buffer buf
                  (insert content)
                  (when file (setq-local buffer-file-name file))
                  (when mode (ignore-errors (funcall mode)))
                  (font-lock-ensure)
                  (set-buffer-modified-p nil)
                  (setq buffer-read-only t)))

    ;; Self-removing hooks (following claude-code-ide pattern)
    (setq before-setup-hook-fn
          (lambda ()
            ;; Delete side windows to prevent "Cannot split side window" errors
            (cl-loop for window in (window-list)
                     when (window-parameter window 'window-side)
                     do (delete-window window))
            (remove-hook 'ediff-before-setup-hook before-setup-hook-fn)))

    (setq startup-hook-fn
          (lambda ()
            (setq ctl-buf ediff-control-buffer)
            (with-current-buffer ediff-control-buffer
              ;; For agent-shell-diff-kill-buffer compatibility
              (setq-local agent-shell-diff--on-exit on-exit)

              ;; Suppress janitor asking about our temp buffers
              (setq-local ediff-keep-variants t)

              ;; Sync scrolling between the two buffers via polling timer.
              ;; (window-scroll-functions doesn't survive redisplay.)
              (+agent-shell-ediff--start-scroll-sync
               ediff-buffer-A ediff-buffer-B)

              ;; Strip fg from ediff overlays so syntax highlighting
              ;; foreground is preserved.  Re-apply on diff navigation.
              (+agent-shell-ediff--strip-overlay-fg)
              (add-hook 'ediff-select-difference-hook
                        #'+agent-shell-ediff--strip-overlay-fg nil t)

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
                                    ('reject
                                     ;; Route through on-exit which sends a proper
                                     ;; reject_once permission response (not just
                                     ;; :cancelled).  Suppress its y-or-n-p since
                                     ;; we already have the user's answer.
                                     (if on-exit
                                         (cl-letf (((symbol-function 'y-or-n-p)
                                                    (lambda (&rest _) nil)))
                                           (funcall on-exit))
                                       (when on-reject (funcall on-reject))))
                                    (_ (message "Ignored")))))))
                           ;; 2. Stop scroll sync and kill temp buffers
                           (lambda ()
                             (+agent-shell-ediff--stop-scroll-sync)
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
                          (cl-loop for buf in (list buf-a buf-b)
                                   when (buffer-live-p buf) do (kill-buffer buf))
                          (cl-loop for var in '(ediff-diff-buffer ediff-fine-diff-buffer
                                                ediff-custom-diff-buffer ediff-error-buffer)
                                   for buf = (and (boundp var) (symbol-value var))
                                   when (and buf (buffer-live-p buf)) do (kill-buffer buf)))
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
