;;; +agent-shell-view-on-y.el --- Rebind y to View diff in permission prompts -*- lexical-binding: t; -*-

;; When a permission prompt includes a diff (i.e., an Edit tool), rebind
;; y to open the diff viewer instead of immediately accepting.  The
;; Allow button is removed from the button row since the user can accept
;; from within the diff viewer.  Non-edit permissions (Bash, etc.) are
;; unaffected.

(eval-when-compile
  (require 'cl-lib))

(define-advice agent-shell--make-tool-call-permission-text
    (:override (&rest args) view-on-y-for-edits)
  "When diff is available, bind y to view and remove the Allow button."
  (cl-destructuring-bind (&key acp-request client state) args
    (let* ((tool-call-id (map-nested-elt acp-request '(params toolCall toolCallId)))
           (diff (map-nested-elt state `(:tool-calls ,tool-call-id :diff)))
           (all-actions (agent-shell--make-permission-actions
                         (map-nested-elt acp-request '(params options))))
           ;; When diff exists, filter allow_once from displayed buttons
           (actions (if diff
                        (seq-remove (lambda (a) (equal (map-elt a :kind) "allow_once"))
                                    all-actions)
                      all-actions))
           (view-char (if diff "y" "v"))
           (shell-buffer (map-elt state :buffer))
           (keymap (let ((map (make-sparse-keymap)))
                     (dolist (action actions)
                       (when-let ((char (map-elt action :char)))
                         (define-key map (kbd char)
                                     (lambda ()
                                       (interactive)
                                       (agent-shell--send-permission-response
                                        :client client
                                        :request-id (map-elt acp-request 'id)
                                        :option-id (map-elt action :option-id)
                                        :state state
                                        :tool-call-id tool-call-id
                                        :message-text (map-elt action :option))
                                       (when (equal (map-elt action :kind) "reject_once")
                                         (with-current-buffer shell-buffer
                                           (agent-shell-interrupt t)))))))
                     (when diff
                       (define-key map view-char
                                   (agent-shell--make-diff-viewing-function
                                    :diff diff
                                    ;; Pass all-actions so accept/reject works in the diff viewer
                                    :actions all-actions
                                    :client client
                                    :request-id (map-elt acp-request 'id)
                                    :state state
                                    :tool-call-id tool-call-id)))
                     (define-key map (kbd "C-c C-c")
                                 (lambda ()
                                   (interactive)
                                   (with-current-buffer shell-buffer
                                     (agent-shell-interrupt t))))
                     map))
           (title (agent-shell--permission-title :acp-request acp-request))
           (diff-button (when diff
                          (agent-shell--make-permission-button
                           :text (format "View (%s)" view-char)
                           :help (format "Press %s to view diff" view-char)
                           :action (agent-shell--make-diff-viewing-function
                                    :diff diff
                                    :actions all-actions
                                    :client client
                                    :request-id (map-elt acp-request 'id)
                                    :state state
                                    :tool-call-id tool-call-id)
                           :keymap keymap
                           :navigatable t
                           :char view-char
                           :option "view diff"))))
      (format "╭─

    %s %s %s%s


    %s%s


╰─"
              (propertize agent-shell-permission-icon
                          'font-lock-face 'warning)
              (propertize "Tool Permission" 'font-lock-face 'bold)
              (propertize agent-shell-permission-icon
                          'font-lock-face 'warning)
              (if title
                  (propertize
                   (format "\n\n\n    %s" title)
                   'font-lock-face 'comint-highlight-input)
                "")
              (if diff-button
                  (concat diff-button " ")
                "")
              (mapconcat (lambda (action)
                           (agent-shell--make-permission-button
                            :text (map-elt action :label)
                            :help (map-elt action :label)
                            :action (lambda ()
                                      (interactive)
                                      (agent-shell--send-permission-response
                                       :client client
                                       :request-id (map-elt acp-request 'id)
                                       :option-id (map-elt action :option-id)
                                       :state state
                                       :tool-call-id tool-call-id
                                       :message-text (format "Selected: %s" (map-elt action :option)))
                                      (when (equal (map-elt action :kind) "reject_once")
                                        (with-current-buffer shell-buffer
                                          (agent-shell-interrupt t))))
                            :keymap keymap
                            :char (map-elt action :char)
                            :option (map-elt action :option)
                            :navigatable t))
                         actions
                         " ")))))

(provide '+agent-shell-view-on-y)
;;; +agent-shell-view-on-y.el ends here
