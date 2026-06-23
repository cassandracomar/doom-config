;;; +emcp.el --- fixes to make emcp work -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;  local fixes to make emcp work
;;
;;; Code:

(require 'cl-lib)
(require 'emcp-confirm)
(require 'seq)

(defvar +emcp-confirm-current-buffer nil
  "Most recent EMCP confirmation buffer.")

(defvar-local +emcp-confirm-transient-display-text nil
  "Text inserted above the `emcp-confirm' transient menu.")

(defun +emcp-confirm-buffer ()
  "Return the active EMCP confirmation buffer."
  (or (and (derived-mode-p 'emcp-confirm-mode)
           (current-buffer))
      (and (buffer-live-p +emcp-confirm-current-buffer)
           +emcp-confirm-current-buffer)
      (seq-find (lambda (buffer)
                  (and (buffer-live-p buffer)
                       (with-current-buffer buffer
                         (and (derived-mode-p 'emcp-confirm-mode)
                              emcp-confirm--pending))))
                (buffer-list))))

(defun +emcp-confirm-dispatch (result)
  "Choose RESULT in the active `emcp-confirm-mode' buffer."
  (if-let* ((buffer (+emcp-confirm-buffer)))
      (with-current-buffer buffer
        (emcp-confirm--dispatch result))
    (user-error "No active EMCP confirmation buffer")))

(defun +emcp-confirm-yes ()
  "Accept the current EMCP confirmation once."
  (interactive)
  (+emcp-confirm-dispatch 'yes-once))

(defun +emcp-confirm-no ()
  "Reject the current EMCP confirmation once."
  (interactive)
  (+emcp-confirm-dispatch 'no-once))

(defun +emcp-confirm-always-accept ()
  "Accept all matching EMCP confirmations in this session."
  (interactive)
  (+emcp-confirm-dispatch 'mode-accept))

(defun +emcp-confirm-always-reject ()
  "Reject all matching EMCP confirmations in this session."
  (interactive)
  (+emcp-confirm-dispatch 'mode-reject))

(defun +emcp-confirm-copy-code ()
  "Copy the current eval confirmation body to the kill ring."
  (interactive)
  (if-let* ((buffer (+emcp-confirm-buffer))
            (code (with-current-buffer buffer
                    (plist-get (plist-get emcp-confirm--pending :context)
                               :code))))
      (progn (kill-new code)
             (message "EMCP: code copied to kill-ring"))
    (user-error "Current EMCP tool does not support copying code")))

(defun +emcp-confirm-dismiss ()
  "Dismiss the active EMCP confirmation buffer."
  (interactive)
  (if-let* ((buffer (+emcp-confirm-buffer)))
      (kill-buffer buffer)
    (user-error "No active EMCP confirmation buffer")))

(defun +emcp-confirm-display-text (session title body)
  "Return the text to display above the EMCP confirm transient."
  (concat
   (format "%s wants to %s:\n\n"
           (propertize (emcp--session-label session) 'face 'emcp-confirm-agent)
           (propertize title 'face 'emcp-confirm-title))
   (emcp-confirm--format-body body)))

(defun +emcp-confirm-transient-insert-body ()
  "Insert the pending EMCP confirmation body into the transient buffer."
  (when (and (boundp 'transient--prefix)
             transient--prefix
             (eq (oref transient--prefix command) 'emcp-confirm))
    (when-let* ((buffer (+emcp-confirm-buffer))
                (text (with-current-buffer buffer
                        +emcp-confirm-transient-display-text)))
      (insert text)
      (unless (bolp) (insert "\n"))
      (insert "\n"))))

(cl-defun +emcp-confirm-prompt (server session &key title body context groups
                                       on-dismiss callback)
  "Open a transient for an EMCP confirmation without showing its buffer."
  (let ((buffer (generate-new-buffer emcp-confirm-buffer-name)))
    (with-current-buffer buffer
      (emcp-confirm-mode)
      (let ((inhibit-read-only t))
        (insert (emcp-confirm--render session title body groups)))
      (goto-char (point-min))
      (use-local-map (emcp-confirm--build-keymap groups))
      (setq emcp-confirm--pending
            (list :server server :session session :context context
                  :on-dismiss on-dismiss :callback callback)
            +emcp-confirm-transient-display-text
            (+emcp-confirm-display-text session title body))
      (add-hook 'kill-buffer-hook #'emcp-confirm--on-kill nil t))
    (setq +emcp-confirm-current-buffer buffer)
    (with-current-buffer buffer
      (emcp-confirm))
    buffer))

(define-keys-and-transient! emcp-confirm-mode-map emcp-confirm
  "EMCP confirm commands."
  :block "Accept?"
  :desc "Yes"                                  :n "y" #'+emcp-confirm-yes
  :desc "No"                                   :n "n" #'+emcp-confirm-no

  :block "Session mode"
  :desc "Always accept"                        :n "a" #'+emcp-confirm-always-accept
  :desc "Always reject"                        :n "r" #'+emcp-confirm-always-reject

  :block "Other"
  :desc "Copy code to kill-ring"               :n "w" #'+emcp-confirm-copy-code
  :desc "Dismiss"                              :n "q" #'+emcp-confirm-dismiss)

(when (fboundp '+emcp-confirm-transient-after-prompt)
  (advice-remove 'emcp-confirm-prompt #'+emcp-confirm-transient-after-prompt))
(advice-remove 'emcp-confirm-prompt #'+emcp-confirm-prompt)
(advice-add 'emcp-confirm-prompt :override #'+emcp-confirm-prompt)
(remove-hook 'transient-setup-buffer-hook #'+emcp-confirm-transient-insert-body)
(add-hook 'transient-setup-buffer-hook #'+emcp-confirm-transient-insert-body)

(defun +emcp-screenshot-frame-png (orig frame)
  "Capture FRAME via ORIG, returning nil instead of failing the tool."
  (when (memq (framep frame) '(x pgtk ns w32 haiku android))
    (condition-case err
        (funcall orig frame)
      (error
       (message "EMCP screenshot skipped frame %S: %s"
                (frame-parameter frame 'name)
                (error-message-string err))
       nil))))

(with-eval-after-load 'emcp-screenshot
  (advice-remove 'emcp-screenshot-frame-png #'+emcp-screenshot-frame-png)
  (advice-add 'emcp-screenshot-frame-png :around #'+emcp-screenshot-frame-png))

(provide '+emcp)
;;; +emcp.el ends here
