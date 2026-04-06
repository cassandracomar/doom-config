;;; +agent-shell-interrupt-fix.el --- Queue input submitted while shell is busy -*- lexical-binding: t; -*-

;; After `agent-shell-interrupt', the shell stays busy until the ACP
;; cancel response arrives (~1-2s).  Any input submitted during that
;; window is silently discarded by `shell-maker--clear-input-for-execution'.
;;
;; Additionally, the first prompt sent after a cancel often gets an
;; immediate empty response from the agent (it hasn't fully settled).
;;
;; Fix:
;; 1. Queue rejected input and replay when finish-output fires.
;; 2. After a cancel, track submitted input.  If finish-output fires
;;    suspiciously fast (< 3s), auto-resubmit once.

(eval-when-compile
  (require 'cl-lib))

(defvar-local +agent-shell--queued-input nil
  "Input that was submitted while shell-maker was busy.
Replayed automatically when `shell-maker-finish-output' fires.")

(defvar-local +agent-shell--post-cancel nil
  "Non-nil after an interrupt cancel, until a prompt completes normally.")

(defvar-local +agent-shell--last-submit nil
  "Cons of (INPUT . TIME) from the most recent post-cancel submission.
Used to detect immediate empty responses and auto-resubmit.")

(define-advice shell-maker--clear-input-for-execution
    (:around (orig-fn &rest args) queue-when-busy)
  "When input is rejected because the shell is busy, queue it for replay.
When in post-cancel state and input is accepted, track it for auto-resubmit."
  (let ((result (apply orig-fn args)))
    (when (derived-mode-p 'agent-shell-mode)
      (let ((input (plist-get args :input)))
        (cond
         ;; Input rejected (busy) — queue it
         ((and (not result) input
               (not (string-empty-p (string-trim input))))
          (setq +agent-shell--queued-input input)
          (message "Input queued — will send when ready"))
         ;; Input accepted in post-cancel state — track for resubmit
         ((and result +agent-shell--post-cancel input
               (not (string-empty-p (string-trim input))))
          (setq +agent-shell--last-submit (cons input (float-time)))))))
    result))

(define-advice shell-maker-finish-output
    (:after (&rest _) replay-queued-input)
  "After the shell becomes ready, replay queued input or auto-resubmit.
If in post-cancel state and the agent responded too fast (< 3s),
the prompt was likely dropped — resubmit it automatically."
  (when (derived-mode-p 'agent-shell-mode)
    (let ((buf (current-buffer)))
      (cond
       ;; Queued input from busy rejection — replay it
       (+agent-shell--queued-input
        (let ((input +agent-shell--queued-input))
          (setq +agent-shell--queued-input nil)
          (run-with-idle-timer
           0.2 nil
           (lambda ()
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (when (and (derived-mode-p 'agent-shell-mode)
                            (not shell-maker--busy))
                   (shell-maker-submit :input input))))))))
       ;; Post-cancel fast response — auto-resubmit
       ((and +agent-shell--last-submit
             (< (- (float-time) (cdr +agent-shell--last-submit)) 1.0))
        (let ((input (car +agent-shell--last-submit)))
          (setq +agent-shell--last-submit nil
                +agent-shell--post-cancel nil)
          (run-with-idle-timer
           0.5 nil
           (lambda ()
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (when (and (derived-mode-p 'agent-shell-mode)
                            (not shell-maker--busy))
                   (message "Auto-resubmitting after post-cancel drop")
                   ;; Delete the failed prompt's visual artifact
                   ;; (input echo + empty response + prompt)
                   (let ((inhibit-read-only t))
                     (when (and (markerp comint-last-input-start)
                                (marker-position comint-last-input-start))
                       (delete-region comint-last-input-start (point-max))))
                   (shell-maker-submit :input input))))))))
       ;; Post-cancel normal response (took > 3s) — clear state
       (+agent-shell--last-submit
        (setq +agent-shell--post-cancel nil
              +agent-shell--last-submit nil))
       ;; No tracked submit — leave post-cancel state alone
       ;; (cancel response's finish-output arrives here before
       ;; the user has typed anything)
       ))))

(define-advice agent-shell-interrupt
    (:after (&optional _force) post-interrupt-fixup)
  "Clear queued input, set post-cancel state, move to end of buffer."
  (when (derived-mode-p 'agent-shell-mode)
    (setq +agent-shell--queued-input nil
          +agent-shell--post-cancel t
          +agent-shell--last-submit nil)
    (goto-char (point-max))))

(provide '+agent-shell-interrupt-fix)
;;; +agent-shell-interrupt-fix.el ends here
