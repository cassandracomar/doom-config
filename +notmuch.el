;;; +notmuch.el --- configure notmuch -*- lexical-binding: t; -*-

(map! :map notmuch-search-mode-map
      :desc "Archive the currently selected thread or region. #pim" "A" #'notmuch-search-archive-thread
      :desc "Filter or LIMIT the current search results. #pim" "/" #'notmuch-search-filter ; alias for l
      :desc "Reply to the entire current thread. #pim" "r" #'notmuch-search-reply-to-thread-sender
      :desc "Reply-all to the entire current thread. #pim" "R" #'notmuch-search-reply-to-thread ; reply to all
      :desc "Refresh current buffer or all notmuch buffers if prefixed. #pim" "g" #'notmuch-multi-refresh-buffer
      :desc "Mark as deleted the currently selected thread. #pim" "d" #'notmuch-multi-search-delete-thread
      :desc "Mark as deleted the currently selected thread. #pim" "D" #'notmuch-multi-search-delete-all
      :desc "Mark as expirable the currently selected thread. #pim" "e" #'notmuch-multi-search-expire-thread
      :desc "Mark as expirable the currently selected thread. #pim" "E" #'notmuch-multi-search-expire-all
      :desc "Mark as spam the currently selected thread. #pim" "s" #'notmuch-multi-search-spam-thread
      :desc "Mark as spam the currently selected thread. #pim" "S" #'notmuch-multi-search-spam-all
      :desc "Mark as flagged the currently selected thread. #pim" "!" #'notmuch-multi-search-flag-thread

      :map notmuch-tree-mode-map
      :desc "Archive the currently selected thread or region. #pim" "A" #'notmuch-tree-archive-thread
      :desc "Filter or LIMIT the current search results. #pim" "/" #'notmuch-tree-filter ; alias for l
      :desc "Reply to the sender of the current message. #pim" "r" #'notmuch-tree-reply-sender
      :desc "Reply-all of the current message. #pim" "R" #'notmuch-tree-reply ; reply to all
      :desc "Mark as deleted the currently selected message. #pim" "d" #'notmuch-multi-tree-delete-message
      :desc "Mark as deleted the currently selected thread. #pim" "D" #'notmuch-multi-tree-delete-thread
      :desc "Mark as expirable the currently selected message. #pim" "e" #'notmuch-multi-tree-expire-message
      :desc "Mark as expirable the currently selected thread. #pim" "E" #'notmuch-multi-tree-expire-thread
      :desc "Mark as spam the currently selected message. #pim" "s" #'notmuch-multi-tree-spam-message
      :desc "Mark as spam the currently selected thread. #pim" "S" #'notmuch-multi-tree-spam-thread
      :desc "Mark as flagged the currently selected thread. #pim" "F" #'notmuch-multi-tree-flag-thread
      :desc "Mark as flagged the currently selected message. #pim" "!" #'notmuch-multi-tree-flag-message
      :desc "Refresh current buffer or all notmuch buffers if prefixed. #pim" "g" #'notmuch-multi-refresh-buffer

      :map notmuch-show-mode-map
      :desc "Tag as flagged or untag is prefixed. #pim" "!" #'notmuch-multi-show-flag-message
      :desc "Archive the current message. #pim" "a" #'notmuch-show-archive-message
      :desc "Archive each message in thread. #pim" "A" #'notmuch-show-archive-thread
      :desc "Reply-to of the current message. #pim" "r" #'notmuch-show-reply-sender
      :desc "Reply-all of the current message. #pim" "R" #'notmuch-show-reply
      :desc "Tag as deleted or untag is prefixed. #pim" "C-t d"  #'notmuch-multi-show-delete-message
      :desc "Tag as deleted or untag is prefixed. #pim" "C-t e"  #'notmuch-multi-show-expire-message
      :desc "Tag as spam or untag is prefixed. #pim" "C-t s" #'notmuch-multi-show-spam-message

      :map notmuch-message-mode-map :ni
      :desc "complete addresses" "TAB" #'completion-at-point
      :desc "complete addresses" "<tab>" #'completion-at-point)

(setq notmuch-hello-sections '(notmuch-multi-hello-insert-accounts-searches
                               notmuch-hello-insert-alltags)
      notmuch-show-logo nil
      notmuch-column-control t
      notmuch-hello-auto-refresh t
      notmuch-hello-recent-searches-max 20
      notmuch-hello-thousands-separator ""
      notmuch-show-all-tags-list t
      notmuch-always-prompt-for-sender t
      notmuch-archive-tags '("-inbox" "-unread" "+archived")
      notmuch-tag-formats '(("unread" (propertize tag 'face 'notmuch-tag-unread) "✉")
                            ("flagged" (propertize tag 'face 'notmuch-tag-flagged) "🚩")
                            ("inbox" (propertize tag 'face 'notmuch-tag-flagged) "inbox")
                            ("delete" (notmuch-apply-face tag 'notmuch-tag-added) "delete")
                            ("archived" (notmuch-apply-face tag 'notmuch-tag-added) "archive")
                            ("sent" (notmuch-apply-face tag 'notmuch-tag-added) "send")
                            ("expire" (notmuch-apply-face tag 'notmuch-tag-added) "expire")
                            ("attachment" (notmuch-apply-face tag 'notmuch-tag-added) "📎")
                            ("important" (propertize tag 'face 'notmuch-tag-flagged) "❗")
                            ("passed" (propertize tag 'face 'notmuch-tag-flagged) "pass")
                            ("replied" (propertize tag 'face 'notmuch-tag-flagged) "↵")
                            ("spam" (propertize tag 'face 'notmuch-tag-flagged) "🐟")
                            ("signed" (propertize tag 'face 'notmuch-tag-flagged) "🔒"))
      notmuch-thread-symbols '((prefix . " ")
                               (top . "─")
                               (top-tee . "┬")
                               (vertical . "│")
                               (vertical-tee . "├")
                               (bottom . "└")
                               ;; (arrow . "►")
                               (arrow . "─►"))
      pi-notmuch-saved-searches `((:name "Inbox"
                                   :query "tag:inbox"
                                   :sort-order newest-first
                                   :search-type tree
                                   :key ,(kbd "i"))
                                  (:name "Unread Inbox"
                                   :query "tag:unread and tag:inbox"
                                   :sort-order newest-first
                                   :search-type tree
                                   :key ,(kbd "u"))
                                  (:name "Unread"
                                   :query "tag:unread"
                                   :sort-order newest-first
                                   :search-type tree
                                   :key ,(kbd "U"))
                                  (:name "All"
                                   :query "*"
                                   :sort-order newest-first
                                   :search-type tree
                                   :key ,(kbd "a"))
                                  (:name "Archived"
                                   :query "tag:archived"
                                   :sort-order newest-first
                                   :search-type tree
                                   :key ,(kbd "A"))
                                  (:name "Important"
                                   :query "tag:important"
                                   :sort-order newest-first
                                   :search-type tree
                                   :key ,(kbd "I"))
                                  (:name "Starred"
                                   :query "tag:flagged"
                                   :sort-order newest-first
                                   :search-type tree
                                   :key ,(kbd "s"))))
(notmuch-multi-accounts-saved-searches-set `((:account (:name "mountclare.net" :query "tag:mountclare.net" :key-prefix "m")
                                              :searches ,(append pi-notmuch-saved-searches
                                                                 `((:name "Unclassified"
                                                                    :query "folder:cass@mountclare.net/Inbox AND tag:read AND NOT tag:expire"
                                                                    :sort-order newest-first
                                                                    :search-type tree
                                                                    :key ,(kbd "x")))))
                                             (:account (:name "nie.rs" :query "tag:nie.rs" :key-prefix "n")
                                              :searches ,(append pi-notmuch-saved-searches
                                                                 `((:name "Unclassified"
                                                                    :query "folder:cass@nie.rs/Inbox AND tag:read AND NOT tag:expire"
                                                                    :sort-order newest-first
                                                                    :search-type tree
                                                                    :key ,(kbd "y")))))
                                             (:account (:name "drwholdings.com" :query "tag:drwholdings.com" :key-prefix "o")
                                              :searches ,(append pi-notmuch-saved-searches
                                                                 `((:name "Unclassified"
                                                                    :query "folder:drwholdings.com/Inbox AND tag:read AND NOT tag:expire"
                                                                    :sort-order newest-first
                                                                    :search-type tree
                                                                    :key ,(kbd "z")))))))

(set-face-attribute 'notmuch-tag-unread nil :inherit 'warning)
(set-face-attribute 'notmuch-search-matching-authors nil :inherit 'notmuch-tree-match-author-face)
(set-face-attribute 'notmuch-search-non-matching-authors nil :inherit 'notmuch-tree-match-author-face)
(set-face-attribute 'notmuch-tree-no-match-author-face nil :inherit 'notmuch-tree-match-author-face)
(set-face-attribute 'notmuch-tree-no-match-date-face nil :inherit 'notmuch-tree-match-date-face)
(set-face-attribute 'notmuch-tree-no-match-tag-face nil :inherit 'notmuch-tree-match-tag-face)

(defun company-grab (regexp &optional expression limit)
  (when (looking-back regexp limit)
    (or (match-string-no-properties (or expression 0)) "")))

(defun +notmuch-company-cape (command &optional arg &rest _ignore)
  "`company-mode' completion back-end for `notmuch'."
  (interactive (list 'interactive))
  (let ((case-fold-search t)
	(completion-ignore-case t))
    (cl-case command
      (prefix (and (or (derived-mode-p 'message-mode)
		       (derived-mode-p 'org-msg-edit-mode))
		   (looking-back
		    (concat notmuch-address-completion-headers-regexp ".*")
		    (line-beginning-position))
                   (setq notmuch-company-last-prefix
			 (company-grab "[:,][ \t]*\\(.*\\)" 1 (point-at-bol)))))
      (candidates (cond
		   ((notmuch-address--harvest-ready)
		    ;; Update harvested addressed from time to time
		    (notmuch-address-harvest-trigger)
		    (notmuch-address-matching arg))
		   (t
		    (cons :async
			  (lambda (callback)
			    ;; First run quick asynchronous harvest
			    ;; based on what the user entered so far
			    (notmuch-address-harvest
			     arg nil
			     (lambda (_proc _event)
			       (funcall callback (notmuch-address-matching arg))
			       ;; Then start the (potentially long-running)
			       ;; full asynchronous harvest if necessary
			       (notmuch-address-harvest-trigger))))))))
      (match (if (string-match notmuch-company-last-prefix arg)
		 (match-end 0)
	       0))
      (post-completion
       (run-hook-with-args 'notmuch-address-post-completion-functions arg))
      (no-cache t))))

(advice-add #'notmuch-company :override #'+notmuch-company-cape)

(add-hook! 'notmuch-message-mode-hook (setq-local completion-at-point-functions (list (cape-company-to-capf 'notmuch-company))))
