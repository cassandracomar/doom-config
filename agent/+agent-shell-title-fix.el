;;; +agent-shell-title-fix.el --- Tolerate :title-less session alists -*- lexical-binding: t; -*-

;; `agent-shell--initiate-new-session' builds the session alist with
;; :id, :mode-id, :modes, :model-id, :models — but no :title.  On the
;; first prompt submission, `agent-shell--send-command' calls
;; `agent-shell--set-session-title', which does
;;   (map-put! (map-elt agent-shell--state :session) :title title)
;; and `map-put!' on a list signals `map-not-inplace' when the key is
;; absent.  The sibling `agent-shell--set-session-from-response' (resume
;; path) does include :title, so this only bites fresh sessions.
;;
;; Override to append the :title pair in place when missing.

;;; Commentary:
;;; Code:

(declare-function agent-shell--emit-event "agent-shell")

(defun +agent-shell--set-session-title (title)
  "Set the current session's title to TITLE and emit `session-title-changed'.
Replacement for `agent-shell--set-session-title' that appends :title
when the session alist lacks the key, instead of erroring."
  (when (and (stringp title)
             (not (string-empty-p title))
             (not (equal (map-nested-elt agent-shell--state '(:session :title)) title)))
    (let ((session (map-elt agent-shell--state :session)))
      (if (assq :title session)
          (map-put! session :title title)
        (nconc session (list (cons :title title)))))
    (agent-shell--emit-event :event 'session-title-changed
                             :data (list (cons :title title)))))

(advice-add 'agent-shell--set-session-title :override #'+agent-shell--set-session-title)

(provide '+agent-shell-title-fix)
;;; +agent-shell-title-fix.el ends here
