;;; +notmuch.el --- configure notmuch -*- lexical-binding: t; -*-

(after! consult-notmuch
  (require 'embark)
  (setq consult-notmuch-export-function #'notmuch-tree)
  (add-to-list 'embark-exporters-alist '(notmuch-result . consult-notmuch-export)))

(defun +notmuch-find-tag ()
  (interactive)
  (let* ((tag (notmuch-select-tag-with-completion "notmuch tag: "))
         (ftag (format "tag:%s" tag))
         (current (notmuch-tree-get-query))
         (new (if current (format "%s and %s" ftag current) ftag)))
    (notmuch-tree new)))

(defun +consult-notmuch-tree ()
  (interactive)
  (let ((current (notmuch-tree-get-query)))
    (consult-notmuch-tree (when current (format "%s and " current)))))

(defun +notmuch-toggle-tag-on-selection (tag mode &optional next-function)
  (save-excursion
    (pcase-let* ((`(,start ,end &rest _) (evil-visual-range))
                 (next (or next-function (intern (format "notmuch-%s-next-message" mode)))))
      (evil-exit-visual-state)
      (with-restriction start end
        (goto-char (point-min))
        (while (<= (point) (point-max))
          (evil-collection-notmuch-toggle-tag tag mode next)))))
  (evil-visual-select start end))

(defun +notmuch-tree-toggle-delete ()
  (interactive)
  (+notmuch-toggle-tag-on-selection "delete" "tree"))

(defun +notmuch-tree-toggle-unread ()
  (interactive)
  (+notmuch-toggle-tag-on-selection "unread" "tree"))

(defun +notmuch-tree-toggle-flagged ()
  (interactive)
  (+notmuch-toggle-tag-on-selection "flagged" "tree"))

(defun +notmuch-search-toggle-delete ()
  (interactive)
  (+notmuch-toggle-tag-on-selection "delete" "search"))

(defun +notmuch-search-toggle-unread ()
  (interactive)
  (+notmuch-toggle-tag-on-selection "unread" "search"))

(defun +notmuch-search-toggle-flagged ()
  (interactive)
  (+notmuch-toggle-tag-on-selection "flagged" "search"))

(map! :map 'notmuch-tree-mode-map
      :mv "d" #'+notmuch-tree-toggle-delete
      :mv "!" #'+notmuch-tree-toggle-unread
      :mv "=" #'+notmuch-tree-toggle-flagged

      :map 'notmuch-search-mode-map
      :mv "d" #'+notmuch-search-toggle-delete
      :mv "!" #'+notmuch-search-toggle-unread
      :mv "=" #'+notmuch-search-toggle-flagged)

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
      :desc "complete addresses" "<tab>" #'completion-at-point

      :map notmuch-common-keymap
      :desc "search for a tag" :n "s" #'+notmuch-find-tag
      :desc "search for a query" :n "S" #'consult-notmuch-tree
      :desc "search for a query without preview" :n "t" #'notmuch-tree

      :map notmuch-tree-mode-map
      :desc "search for a tag" :n "s" #'+notmuch-find-tag
      :desc "search for a query" :n "S" #'+consult-notmuch-tree
      :desc "search for a query without preview" :n "t" #'notmuch-tree)

(setq notmuch-hello-sections '(notmuch-multi-hello-insert-accounts-searches
                               notmuch-hello-insert-search
                               notmuch-hello-insert-recent-searches
                               notmuch-hello-insert-footer)
      notmuch-show-logo nil
      notmuch-column-control t
      notmuch-hello-auto-refresh t
      notmuch-hello-recent-searches-max 20
      notmuch-hello-thousands-separator ""
      notmuch-show-all-tags-list t
      notmuch-always-prompt-for-sender t
      notmuch-archive-tags '("-inbox" "-unread" "+archived")
      notmuch-multi-delete-tag "trash"
      notmuch-tag-formats '(("unread" (propertize tag 'face 'notmuch-tag-unread) "✉")
                            ("flagged" (propertize tag 'face 'notmuch-tag-flagged) "🚩")
                            ("inbox" (propertize tag 'face 'notmuch-tag-flagged) "📬")
                            ("delete" (notmuch-apply-face tag 'notmuch-tag-added) "❌")
                            ("archived" (notmuch-apply-face tag 'notmuch-tag-added) "💾")
                            ("sent" (notmuch-apply-face tag 'notmuch-tag-added) "📨")
                            ("expire" (notmuch-apply-face tag 'notmuch-tag-added) "🖍")
                            ("attachment" (notmuch-apply-face tag 'notmuch-tag-added) "📎")
                            ("important" (propertize tag 'face 'notmuch-tag-flagged) "❗")
                            ("passed" (propertize tag 'face 'notmuch-tag-flagged) "⚽")
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
      pi-notmuch-saved-searches `((:name "Unread Inbox"
                                   :query "tag:unread and tag:inbox and -tag:trash and -tag:deleted and -tag:spam"
                                   :sort-order newest-first
                                   :search-type tree
                                   :key ,(kbd "u"))
                                  (:name "Inbox"
                                   :query "tag:inbox and -tag:trash and -tag:deleted and -tag:spam"
                                   :sort-order newest-first
                                   :search-type tree
                                   :key ,(kbd "i"))
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
                                   :key ,(kbd "s")))
      notmuch-tree-outline-enabled t)

(notmuch-multi-accounts-saved-searches-set `((:account (:name "mountclare.net" :query "tag:mountclare.net" :key-prefix "m")
                                              :searches ,pi-notmuch-saved-searches)
                                             (:account (:name "nie.rs" :query "tag:nie.rs" :key-prefix "n")
                                              :searches ,pi-notmuch-saved-searches)
                                             (:account (:name "drwholdings.com" :query "tag:drwholdings.com" :key-prefix "o")
                                              :searches ,pi-notmuch-saved-searches)))

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
(setq-hook! 'notmuch-message-mode-hook
  completion-at-point-functions (list (cape-company-to-capf 'notmuch-company)))
