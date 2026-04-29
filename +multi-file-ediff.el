;;; +multi-file-ediff.el --- Multi-file ediff with sidebar -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Treemacs-style left sidebar listing files in a changeset.
;; Selecting a file opens an ediff session for it.  The sidebar
;; persists across file switches and is destroyed when the session ends.
;;
;; Generic via cl-generic protocol.  Initial adapter: magit (single
;; commit or rev-range).
;;
;;; Code:

(require 'cl-lib)
(require 'ediff)
(require 'ediff-init)
(require 'magit)

(defgroup multi-file-ediff nil
  "Multi-file ediff with sidebar."
  :group 'tools)

(defcustom multi-file-ediff-sidebar-width 32
  "Width of the multi-file-ediff sidebar window."
  :type 'integer
  :group 'multi-file-ediff)

(defface multi-file-ediff-current-file
  '((t :inherit magit-section-highlight :weight bold))
  "Face for the currently displayed file in the sidebar."
  :group 'multi-file-ediff)

(defface multi-file-ediff-visited-file
  '((t :inherit shadow))
  "Face for files already visited in this session."
  :group 'multi-file-ediff)


;;;; Source protocol

(cl-defgeneric multi-file-ediff--prepare (source)
  "Pre-flight before listing files (e.g. fetch refs)."
  (ignore source))

(cl-defgeneric multi-file-ediff--title (source)
  "Return a title string for SOURCE.")

(cl-defgeneric multi-file-ediff--list-files (source)
  "Return a list of (PATH . META) for SOURCE.")

(cl-defgeneric multi-file-ediff--make-buffers (source path)
  "Return (OLD-BUFFER . NEW-BUFFER) for PATH from SOURCE.")


;;;; Session group

(cl-defstruct multi-file-ediff-group
  source
  title
  files
  current-file
  visited
  sidebar-buffer
  ctl-buffer
  saved-winconf
  switching-files-p
  tracked-buffers)

(defvar multi-file-ediff--current-session nil
  "The active session group, or nil.")


;;;; Sidebar mode

(defvar multi-file-ediff-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "RET")    #'multi-file-ediff-select-file)
    (define-key m (kbd "SPC")    #'multi-file-ediff-select-file)
    (define-key m (kbd "j")      #'multi-file-ediff-next-file)
    (define-key m (kbd "k")      #'multi-file-ediff-previous-file)
    (define-key m (kbd "<down>") #'multi-file-ediff-next-file)
    (define-key m (kbd "<up>")   #'multi-file-ediff-previous-file)
    (define-key m (kbd "g")      #'multi-file-ediff-refresh)
    (define-key m (kbd "q")      #'multi-file-ediff-end-session)
    m)
  "Keymap for `multi-file-ediff-mode'.")

(define-derived-mode multi-file-ediff-mode special-mode "Multi-File Ediff"
  "Major mode for the multi-file-ediff sidebar."
  (setq-local truncate-lines t)
  (setq-local cursor-type nil)
  (hl-line-mode 1)
  (add-hook 'kill-buffer-hook
            #'multi-file-ediff--sidebar-killed-hook nil t))

(when (fboundp 'evil-set-initial-state)
  (evil-set-initial-state 'multi-file-ediff-mode 'normal))

(with-eval-after-load 'evil
  (evil-define-key* 'normal multi-file-ediff-mode-map
    (kbd "RET")    #'multi-file-ediff-select-file
    (kbd "SPC")    #'multi-file-ediff-select-file
    (kbd "j")      #'multi-file-ediff-next-file
    (kbd "k")      #'multi-file-ediff-previous-file
    (kbd "<down>") #'multi-file-ediff-next-file
    (kbd "<up>")   #'multi-file-ediff-previous-file
    (kbd "g")      #'multi-file-ediff-refresh
    (kbd "q")      #'multi-file-ediff-end-session))


;;;; Sidebar rendering

(defun multi-file-ediff--render-sidebar (group)
  "Render GROUP's file list."
  (let ((buf (multi-file-ediff-group-sidebar-buffer group)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (current (multi-file-ediff-group-current-file group))
              (visited (multi-file-ediff-group-visited group))
              (saved-line (line-number-at-pos)))
          (erase-buffer)
          (setq header-line-format
                (multi-file-ediff-group-title group))
          (dolist (entry (multi-file-ediff-group-files group))
            (let* ((path (car entry))
                   (face (cond ((equal path current)
                                'multi-file-ediff-current-file)
                               ((gethash path visited)
                                'multi-file-ediff-visited-file)
                               (t 'default))))
              (insert (propertize path
                                  'face face
                                  'multi-file-ediff-path path))
              (insert "\n")))
          (goto-char (point-min))
          (forward-line (1- saved-line)))))))


;;;; Window display

(defun multi-file-ediff--show-sidebar (group)
  "Display GROUP's sidebar in the left side window."
  (let ((win (display-buffer-in-side-window
              (multi-file-ediff-group-sidebar-buffer group)
              `((side . left) (slot . 0)
                (window-width . ,multi-file-ediff-sidebar-width)
                (preserve-size . (t . nil))))))
    (when win
      (set-window-parameter win 'multi-file-ediff-sidebar t)
      (set-window-parameter win 'no-delete-other-windows t)
      (set-window-dedicated-p win t))
    win))

(defun multi-file-ediff--our-sidebar-window-p (win)
  (window-parameter win 'multi-file-ediff-sidebar))


;;;; Lifecycle

(defun multi-file-ediff-start (source)
  "Start a multi-file ediff session driven by SOURCE."
  (when multi-file-ediff--current-session
    (unless (y-or-n-p
             (format "End existing review of %s? "
                     (multi-file-ediff-group-title
                      multi-file-ediff--current-session)))
      (user-error "Aborted: existing session is still active"))
    (multi-file-ediff--end-session multi-file-ediff--current-session))
  (multi-file-ediff--prepare source)
  (let* ((title (multi-file-ediff--title source))
         (files (multi-file-ediff--list-files source))
         (winconf (current-window-configuration))
         buf group)
    (when (null files)
      (user-error "No files in %s" title))
    (setq buf (generate-new-buffer (format "*multi-file-ediff: %s*" title)))
    (setq group (make-multi-file-ediff-group
                 :source source
                 :title title
                 :files files
                 :sidebar-buffer buf
                 :visited (make-hash-table :test #'equal)
                 :saved-winconf winconf))
    (with-current-buffer buf
      (multi-file-ediff-mode)
      (setq-local multi-file-ediff--buffer-group group))
    (setq multi-file-ediff--current-session group)
    (multi-file-ediff--render-sidebar group)
    (multi-file-ediff--show-sidebar group)
    (multi-file-ediff--ediff-file group (caar files))
    group))

(defun multi-file-ediff--kill-ediff-aux-buffers (ctl)
  "Kill ediff aux buffers (diff/error/fine-diff/etc.) associated with CTL."
  (when (and ctl (buffer-live-p ctl))
    (with-current-buffer ctl
      (dolist (var '(ediff-diff-buffer
                     ediff-custom-diff-buffer
                     ediff-fine-diff-buffer
                     ediff-tmp-buffer
                     ediff-error-buffer
                     ediff-msg-buffer
                     ediff-debug-buffer))
        (when-let ((buf (and (boundp var) (symbol-value var))))
          (when (buffer-live-p buf)
            (let ((kill-buffer-query-functions nil))
              (ignore-errors (kill-buffer buf)))))))))

(defun multi-file-ediff--end-session (&optional group)
  "Tear down GROUP (defaults to the current session)."
  (let ((group (or group multi-file-ediff--current-session)))
    (when group
      ;; Mark as switching so the per-session ediff-quit-hook skips re-showing
      ;; the sidebar (we're about to kill it anyway).
      (setf (multi-file-ediff-group-switching-files-p group) t)
      (let ((ctl (multi-file-ediff-group-ctl-buffer group)))
        (when (and ctl (buffer-live-p ctl))
          (with-current-buffer ctl
            (ignore-errors (ediff-really-quit nil)))
          ;; Belt-and-suspenders: ediff-cleanup-mess should have killed these,
          ;; but if it didn't (e.g. because ediff-quit-hook was clobbered), do
          ;; it ourselves.
          (multi-file-ediff--kill-ediff-aux-buffers ctl)
          (when (buffer-live-p ctl)
            (let ((kill-buffer-query-functions nil))
              (ignore-errors (kill-buffer ctl))))))
      (setf (multi-file-ediff-group-switching-files-p group) nil)
      (multi-file-ediff--kill-tracked-buffers group)
      (let ((buf (multi-file-ediff-group-sidebar-buffer group)))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (remove-hook 'kill-buffer-hook
                         #'multi-file-ediff--sidebar-killed-hook t))
          (kill-buffer buf)))
      (when (eq group multi-file-ediff--current-session)
        (setq multi-file-ediff--current-session nil))
      (when-let ((wc (multi-file-ediff-group-saved-winconf group)))
        (ignore-errors (set-window-configuration wc))))))

(defun multi-file-ediff--sidebar-killed-hook ()
  "Called when the sidebar buffer is killed externally."
  (when (and multi-file-ediff--current-session
             (eq (multi-file-ediff-group-sidebar-buffer
                  multi-file-ediff--current-session)
                 (current-buffer)))
    (multi-file-ediff--end-session multi-file-ediff--current-session)))


;;;; Per-file ediff

(defun multi-file-ediff--track-buffer (group buf)
  "Track BUF in GROUP for cleanup on session end."
  (when (and (buffer-live-p buf)
             (not (memq buf (multi-file-ediff-group-tracked-buffers group))))
    (push buf (multi-file-ediff-group-tracked-buffers group))))

(defun multi-file-ediff--kill-tracked-buffers (group)
  "Kill all buffers tracked by GROUP and clear the list."
  (dolist (buf (multi-file-ediff-group-tracked-buffers group))
    (when (buffer-live-p buf)
      (let ((kill-buffer-query-functions nil))
        (ignore-errors (kill-buffer buf)))))
  (setf (multi-file-ediff-group-tracked-buffers group) nil))

(defun multi-file-ediff--ediff-file (group path)
  "Start ediff for PATH belonging to GROUP."
  ;; Kill any leftover buffers from the previous file before starting.
  (multi-file-ediff--kill-tracked-buffers group)
  (let* ((source (multi-file-ediff-group-source group))
         (bufs (multi-file-ediff--make-buffers source path))
         (old-buf (car bufs))
         (new-buf (cdr bufs))
         startup-hook-fn before-setup-hook-fn)
    (multi-file-ediff--track-buffer group old-buf)
    (multi-file-ediff--track-buffer group new-buf)
    (setf (multi-file-ediff-group-current-file group) path)
    (puthash path t (multi-file-ediff-group-visited group))
    (multi-file-ediff--render-sidebar group)
    (setq before-setup-hook-fn
          (lambda ()
            (cl-loop for win in (window-list)
                     when (and (window-parameter win 'window-side)
                               (not (multi-file-ediff--our-sidebar-window-p win)))
                     do (delete-window win))
            (remove-hook 'ediff-before-setup-hook before-setup-hook-fn)))
    (setq startup-hook-fn
          (lambda ()
            (setf (multi-file-ediff-group-ctl-buffer group)
                  ediff-control-buffer)
            (with-current-buffer ediff-control-buffer
              (setq-local ediff-keep-variants t)
              (local-set-key (kbd "q") #'multi-file-ediff-end-session)
              (when (fboundp 'evil-local-set-key)
                (evil-local-set-key 'normal (kbd "q")
                                    #'multi-file-ediff-end-session))
              (setq-local ediff-quit-hook
                          (list
                           (lambda ()
                             (when (buffer-live-p old-buf) (kill-buffer old-buf))
                             (when (buffer-live-p new-buf) (kill-buffer new-buf)))
                           #'ediff-cleanup-mess
                           (lambda ()
                             (setf (multi-file-ediff-group-ctl-buffer group) nil)
                             (unless (multi-file-ediff-group-switching-files-p group)
                               (multi-file-ediff--show-sidebar group))))))
            (multi-file-ediff--show-sidebar group)
            (remove-hook 'ediff-startup-hook startup-hook-fn)))
    (add-hook 'ediff-before-setup-hook before-setup-hook-fn)
    (add-hook 'ediff-startup-hook startup-hook-fn)
    (condition-case err
        (let ((ediff-window-setup-function 'ediff-setup-windows-plain)
              (ediff-split-window-function 'split-window-horizontally))
          (ediff-buffers old-buf new-buf))
      (error
       (remove-hook 'ediff-before-setup-hook before-setup-hook-fn)
       (remove-hook 'ediff-startup-hook startup-hook-fn)
       (when (buffer-live-p old-buf) (kill-buffer old-buf))
       (when (buffer-live-p new-buf) (kill-buffer new-buf))
       (signal (car err) (cdr err))))))


;;;; Sidebar commands

(defun multi-file-ediff--path-at-point ()
  (get-text-property (line-beginning-position) 'multi-file-ediff-path))

(defun multi-file-ediff-select-file ()
  "Ediff the file on the current sidebar line."
  (interactive)
  (let ((path (multi-file-ediff--path-at-point))
        (group multi-file-ediff--current-session))
    (cond
     ((not path) (user-error "No file at point"))
     ((not group) (user-error "No active multi-file-ediff session"))
     ((equal path (multi-file-ediff-group-current-file group))
      (message "Already showing %s" path))
     (t
      (let ((ctl (multi-file-ediff-group-ctl-buffer group)))
        (when (and ctl (buffer-live-p ctl))
          (setf (multi-file-ediff-group-switching-files-p group) t)
          (unwind-protect
              (with-current-buffer ctl
                (ignore-errors (ediff-really-quit nil)))
            (setf (multi-file-ediff-group-switching-files-p group) nil))))
      (multi-file-ediff--ediff-file group path)))))

(defun multi-file-ediff-next-file ()
  "Move to next file in the sidebar."
  (interactive)
  (forward-line 1))

(defun multi-file-ediff-previous-file ()
  "Move to previous file in the sidebar."
  (interactive)
  (forward-line -1))

(defun multi-file-ediff-refresh ()
  "Re-list files for the current session."
  (interactive)
  (let ((group multi-file-ediff--current-session))
    (when group
      (setf (multi-file-ediff-group-files group)
            (multi-file-ediff--list-files
             (multi-file-ediff-group-source group)))
      (multi-file-ediff--render-sidebar group))))

(defun multi-file-ediff-end-session ()
  "End the multi-file-ediff session entirely."
  (interactive)
  (multi-file-ediff--end-session))


;;;; Magit adapter

(cl-defstruct (multi-file-ediff-magit-source
               (:constructor multi-file-ediff-magit-source-make))
  base-ref
  head-ref
  default-directory
  display-title)

(cl-defmethod multi-file-ediff--title
  ((source multi-file-ediff-magit-source))
  (multi-file-ediff-magit-source-display-title source))

(cl-defmethod multi-file-ediff--prepare
  ((source multi-file-ediff-magit-source))
  "Try to fetch from origin if either ref is not yet local."
  (let* ((default-directory
          (multi-file-ediff-magit-source-default-directory source))
         (base (multi-file-ediff-magit-source-base-ref source))
         (head (multi-file-ediff-magit-source-head-ref source)))
    (unless (and base head (magit-rev-verify base) (magit-rev-verify head))
      (ignore-errors (magit-call-git "fetch" "origin")))))

(cl-defmethod multi-file-ediff--list-files
  ((source multi-file-ediff-magit-source))
  (let ((default-directory (multi-file-ediff-magit-source-default-directory source)))
    (mapcar (lambda (p) (cons p nil))
            (magit-git-items
             "diff-tree" "--no-commit-id" "--name-only" "-r" "-z"
             (multi-file-ediff-magit-source-base-ref source)
             (multi-file-ediff-magit-source-head-ref source)))))

(cl-defmethod multi-file-ediff--make-buffers
  ((source multi-file-ediff-magit-source) path)
  (let ((default-directory (multi-file-ediff-magit-source-default-directory source))
        (base (multi-file-ediff-magit-source-base-ref source))
        (head (multi-file-ediff-magit-source-head-ref source)))
    (cons (multi-file-ediff--magit-file-buffer base path "old")
          (multi-file-ediff--magit-file-buffer head path "new"))))

(defun multi-file-ediff--magit-file-buffer (rev path side)
  "Return a buffer with PATH at REV, or an empty stand-in if missing.
SIDE is \"old\" or \"new\" — used for the empty-buffer name."
  (or (ignore-errors (magit-find-file-noselect rev path))
      (multi-file-ediff--empty-buffer path side)))

(defun multi-file-ediff--empty-buffer (path side)
  "Create an empty stand-in buffer for PATH on SIDE."
  (let ((buf (generate-new-buffer (format "*[empty] %s: %s*" side path))))
    (with-current-buffer buf
      (when-let ((mode (assoc-default path auto-mode-alist #'string-match)))
        (ignore-errors (funcall mode)))
      (setq buffer-read-only t))
    buf))


;;;; Magit adapters: index, working tree

(cl-defstruct (multi-file-ediff-magit-staged-source
               (:constructor multi-file-ediff-magit-staged-source-make))
  default-directory
  display-title)

(cl-defmethod multi-file-ediff--title
  ((source multi-file-ediff-magit-staged-source))
  (multi-file-ediff-magit-staged-source-display-title source))

(cl-defmethod multi-file-ediff--list-files
  ((source multi-file-ediff-magit-staged-source))
  (let ((default-directory
         (multi-file-ediff-magit-staged-source-default-directory source)))
    (mapcar (lambda (p) (cons p nil)) (magit-staged-files))))

(cl-defmethod multi-file-ediff--make-buffers
  ((source multi-file-ediff-magit-staged-source) path)
  (let ((default-directory
         (multi-file-ediff-magit-staged-source-default-directory source)))
    (cons (multi-file-ediff--magit-file-buffer "HEAD" path "old")
          (or (ignore-errors (magit-find-file-index-noselect path))
              (multi-file-ediff--empty-buffer path "index")))))

(cl-defstruct (multi-file-ediff-magit-unstaged-source
               (:constructor multi-file-ediff-magit-unstaged-source-make))
  default-directory
  display-title)

(cl-defmethod multi-file-ediff--title
  ((source multi-file-ediff-magit-unstaged-source))
  (multi-file-ediff-magit-unstaged-source-display-title source))

(cl-defmethod multi-file-ediff--list-files
  ((source multi-file-ediff-magit-unstaged-source))
  (let ((default-directory
         (multi-file-ediff-magit-unstaged-source-default-directory source)))
    (mapcar (lambda (p) (cons p nil)) (magit-unstaged-files))))

(cl-defmethod multi-file-ediff--make-buffers
  ((source multi-file-ediff-magit-unstaged-source) path)
  (let ((default-directory
         (multi-file-ediff-magit-unstaged-source-default-directory source)))
    (cons (or (ignore-errors (magit-find-file-index-noselect path))
              (multi-file-ediff--empty-buffer path "index"))
          (find-file-noselect (expand-file-name path)))))

(cl-defstruct (multi-file-ediff-magit-working-tree-source
               (:constructor multi-file-ediff-magit-working-tree-source-make))
  default-directory
  display-title)

(cl-defmethod multi-file-ediff--title
  ((source multi-file-ediff-magit-working-tree-source))
  (multi-file-ediff-magit-working-tree-source-display-title source))

(cl-defmethod multi-file-ediff--list-files
  ((source multi-file-ediff-magit-working-tree-source))
  (let ((default-directory
         (multi-file-ediff-magit-working-tree-source-default-directory source)))
    (mapcar (lambda (p) (cons p nil)) (magit-modified-files))))

(cl-defmethod multi-file-ediff--make-buffers
  ((source multi-file-ediff-magit-working-tree-source) path)
  (let ((default-directory
         (multi-file-ediff-magit-working-tree-source-default-directory source)))
    (cons (multi-file-ediff--magit-file-buffer "HEAD" path "old")
          (find-file-noselect (expand-file-name path)))))


;;;; Magit entry points

;;;###autoload
(defun multi-file-ediff-magit-commit (rev)
  "Open multi-file ediff for the changes in commit REV."
  (interactive (list (magit-read-branch-or-commit "Commit")))
  (let* ((dir (or (magit-toplevel) default-directory))
         (sha (let ((default-directory dir)) (magit-rev-parse rev)))
         (source (multi-file-ediff-magit-source-make
                  :base-ref (concat rev "^")
                  :head-ref rev
                  :default-directory dir
                  :display-title (format "commit %s" (substring sha 0 7)))))
    (multi-file-ediff-start source)))

;;;###autoload
(defun multi-file-ediff-magit-range (base head)
  "Open multi-file ediff for BASE..HEAD."
  (interactive
   (list (magit-read-branch-or-commit "Base")
         (magit-read-branch-or-commit "Head")))
  (let ((source (multi-file-ediff-magit-source-make
                 :base-ref base
                 :head-ref head
                 :default-directory (or (magit-toplevel) default-directory)
                 :display-title (format "%s..%s" base head))))
    (multi-file-ediff-start source)))

;;;###autoload
(defun multi-file-ediff-magit-show-staged ()
  "Open multi-file ediff for staged changes (HEAD vs INDEX)."
  (interactive)
  (let* ((dir (or (magit-toplevel) default-directory))
         (source (multi-file-ediff-magit-staged-source-make
                  :default-directory dir
                  :display-title "staged (HEAD..INDEX)")))
    (multi-file-ediff-start source)))

;;;###autoload
(defun multi-file-ediff-magit-show-unstaged ()
  "Open multi-file ediff for unstaged changes (INDEX vs working tree)."
  (interactive)
  (let* ((dir (or (magit-toplevel) default-directory))
         (source (multi-file-ediff-magit-unstaged-source-make
                  :default-directory dir
                  :display-title "unstaged (INDEX..WORKTREE)")))
    (multi-file-ediff-start source)))

;;;###autoload
(defun multi-file-ediff-magit-show-working-tree ()
  "Open multi-file ediff for working tree vs HEAD."
  (interactive)
  (let* ((dir (or (magit-toplevel) default-directory))
         (source (multi-file-ediff-magit-working-tree-source-make
                  :default-directory dir
                  :display-title "working tree (HEAD..WORKTREE)")))
    (multi-file-ediff-start source)))


;;;; Advice variants for `magit-ediff-show-*' / `magit-ediff-compare'

(defun multi-file-ediff-magit--advice-show-commit (rev &rest _)
  "Drop-in replacement for `magit-ediff-show-commit' that uses multi-file ediff."
  (interactive (list (magit-read-branch-or-commit "Commit")))
  (multi-file-ediff-magit-commit rev))

(defun multi-file-ediff-magit--advice-compare (reva revb &rest _)
  "Drop-in replacement for `magit-ediff-compare' that uses multi-file ediff."
  (interactive
   (let ((rs (magit-ediff-compare--read-revisions)))
     (list (car rs) (cdr rs))))
  (multi-file-ediff-magit-range reva revb))

(defun multi-file-ediff-magit--advice-show-staged (&rest _)
  "Drop-in replacement for `magit-ediff-show-staged' that uses multi-file ediff."
  (interactive)
  (multi-file-ediff-magit-show-staged))

(defun multi-file-ediff-magit--advice-show-unstaged (&rest _)
  "Drop-in replacement for `magit-ediff-show-unstaged' that uses multi-file ediff."
  (interactive)
  (multi-file-ediff-magit-show-unstaged))

(defun multi-file-ediff-magit--advice-show-working-tree (&rest _)
  "Drop-in replacement for `magit-ediff-show-working-tree' that uses multi-file ediff."
  (interactive)
  (multi-file-ediff-magit-show-working-tree))

(defun multi-file-ediff-magit--advice-dwim (orig &rest args)
  "Around-advice for `magit-ediff-dwim': route the multi-file cases through
our entry points so we don't get the single-file prompt for PRs / ranges."
  (let* ((section (magit-current-section))
         (range (and section (magit-diff--dwim))))
    (cond
     ((and (consp range) (eq (car range) 'commit))
      (multi-file-ediff-magit-commit (cdr range)))
     ((stringp range)
      (pcase-let ((`(,a ,b) (magit-ediff-compare--read-revisions range)))
        (multi-file-ediff-magit-range a b)))
     (t (apply orig args)))))


;;;###autoload
(defun multi-file-ediff-magit-setup ()
  "Install :override advice on magit-ediff entry points.
Routes the standard magit-ediff transient through multi-file-ediff."
  (interactive)
  (with-eval-after-load 'magit-ediff
    (advice-add 'magit-ediff-show-commit       :override #'multi-file-ediff-magit--advice-show-commit)
    (advice-add 'magit-ediff-compare           :override #'multi-file-ediff-magit--advice-compare)
    (advice-add 'magit-ediff-show-staged       :override #'multi-file-ediff-magit--advice-show-staged)
    (advice-add 'magit-ediff-show-unstaged     :override #'multi-file-ediff-magit--advice-show-unstaged)
    (advice-add 'magit-ediff-show-working-tree :override #'multi-file-ediff-magit--advice-show-working-tree)
    (advice-add 'magit-ediff-dwim              :around   #'multi-file-ediff-magit--advice-dwim)))

(defun multi-file-ediff-magit-teardown ()
  "Remove the advice installed by `multi-file-ediff-magit-setup'."
  (interactive)
  (advice-remove 'magit-ediff-show-commit       #'multi-file-ediff-magit--advice-show-commit)
  (advice-remove 'magit-ediff-compare           #'multi-file-ediff-magit--advice-compare)
  (advice-remove 'magit-ediff-show-staged       #'multi-file-ediff-magit--advice-show-staged)
  (advice-remove 'magit-ediff-show-unstaged     #'multi-file-ediff-magit--advice-show-unstaged)
  (advice-remove 'magit-ediff-show-working-tree #'multi-file-ediff-magit--advice-show-working-tree)
  (advice-remove 'magit-ediff-dwim              #'multi-file-ediff-magit--advice-dwim))


;;;; Position translation (new-line ↔ diff-position)

(defun multi-file-ediff--narrow-to-file (raw-diff path)
  "Return (BEG . END) bounds of PATH's section in RAW-DIFF, or nil."
  (with-temp-buffer
    (insert raw-diff)
    (goto-char (point-min))
    (let ((file-re (format "^diff --git \\(?:a/\\)?%s \\(?:b/\\)?%s$"
                           (regexp-quote path) (regexp-quote path))))
      (when (re-search-forward file-re nil t)
        (let ((beg (line-beginning-position)))
          (forward-line 1)
          (let ((end (or (and (re-search-forward "^diff --git " nil t)
                              (line-beginning-position))
                         (point-max))))
            (cons beg end)))))))

(defun multi-file-ediff--walk-file-diff (raw-diff path fn)
  "Walk PATH's hunks in RAW-DIFF, calling FN for each line.
FN is called as (FN POSITION NEW-LINE LINE-TYPE LINE-TEXT) where:
  POSITION  - 1-indexed diff position
  NEW-LINE  - new-side line number (or nil for `-` lines)
  LINE-TYPE - one of \"ADDED\", \"REMOVED\", \"UNCHANGED\", \"HUNK\", \"NOEOL\"
  LINE-TEXT - the diff line content without the leading marker"
  (when-let ((bounds (multi-file-ediff--narrow-to-file raw-diff path)))
    (with-temp-buffer
      (insert raw-diff)
      (let ((current-new-line nil)
            (position 0))
        (goto-char (car bounds))
        (forward-line 1)
        ;; Skip file header lines until first hunk
        (while (and (< (point) (cdr bounds))
                    (not (looking-at "^@@ ")))
          (forward-line 1))
        (while (< (point) (cdr bounds))
          (cl-incf position)
          (cond
           ((looking-at "^@@ -[0-9]+\\(?:,[0-9]+\\)? \\+\\([0-9]+\\)")
            (setq current-new-line (string-to-number (match-string 1)))
            (funcall fn position nil "HUNK"
                     (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position))))
           ((looking-at "^\\+\\(.*\\)")
            (funcall fn position current-new-line "ADDED" (match-string 1))
            (when current-new-line (cl-incf current-new-line)))
           ((looking-at "^ \\(.*\\)")
            (funcall fn position current-new-line "UNCHANGED" (match-string 1))
            (when current-new-line (cl-incf current-new-line)))
           ((looking-at "^-\\(.*\\)")
            (funcall fn position nil "REMOVED" (match-string 1)))
           ((looking-at "^\\\\")
            (funcall fn position nil "NOEOL"
                     (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position)))))
          (forward-line 1))))))

(defun multi-file-ediff--new-line-to-diff-position (raw-diff path new-line)
  "Return the (POSITION . LINE-TYPE) for NEW-LINE in PATH, or nil if not in a hunk."
  (let (result)
    (multi-file-ediff--walk-file-diff
     raw-diff path
     (lambda (pos n-line type _text)
       (when (and (not result) n-line (= n-line new-line))
         (setq result (cons pos type)))))
    result))

(defun multi-file-ediff--diff-position-to-new-line (raw-diff path position)
  "Return the new-line for POSITION in PATH, or nil if not on the new side."
  (let (result)
    (multi-file-ediff--walk-file-diff
     raw-diff path
     (lambda (pos n-line _type _text)
       (when (and (not result) (= pos position) n-line)
         (setq result n-line))))
    result))


;;;; code-review adapter

(cl-defstruct (multi-file-ediff-code-review-source
               (:constructor multi-file-ediff-code-review-source-make))
  pullreq           ; code-review-github-repo (or sibling)
  cr-buffer         ; the code-review buffer (kept alive for cooperation)
  base-ref          ; merge-base SHA
  head-ref          ; head SHA
  default-directory
  display-title)

(cl-defmethod multi-file-ediff--title
  ((source multi-file-ediff-code-review-source))
  (multi-file-ediff-code-review-source-display-title source))

(defun multi-file-ediff--files-from-raw-diff (raw-diff)
  "Return the list of file paths in RAW-DIFF (a unified diff string)."
  (when (stringp raw-diff)
    (let (files)
      (with-temp-buffer
        (insert raw-diff)
        (goto-char (point-min))
        (while (re-search-forward "^diff --git \\(?:a/\\)?\\([^ \t\n]+\\) " nil t)
          (push (match-string 1) files)))
      (nreverse files))))

(cl-defmethod multi-file-ediff--prepare
  ((source multi-file-ediff-code-review-source))
  "Ensure the PR's base and head refs are available locally."
  (let* ((default-directory
          (multi-file-ediff-code-review-source-default-directory source))
         (base (multi-file-ediff-code-review-source-base-ref source))
         (head (multi-file-ediff-code-review-source-head-ref source))
         (pr (multi-file-ediff-code-review-source-pullreq source))
         (number (and pr (oref pr number))))
    (unless (and base head
                 (magit-rev-verify base)
                 (magit-rev-verify head))
      (when number
        (let ((spec (format "+refs/pull/%s/head:refs/pullreqs/%s"
                            number number)))
          (ignore-errors
            (magit-call-git "fetch" "origin" spec))))
      (ignore-errors (magit-call-git "fetch" "origin")))))

(cl-defmethod multi-file-ediff--list-files
  ((source multi-file-ediff-code-review-source))
  (let* ((default-directory
          (multi-file-ediff-code-review-source-default-directory source))
         (base (multi-file-ediff-code-review-source-base-ref source))
         (head (multi-file-ediff-code-review-source-head-ref source))
         (from-git (and base head
                        (ignore-errors (magit-changed-files base head)))))
    (mapcar (lambda (p) (cons p nil))
            (or from-git
                (let ((cr-buf (multi-file-ediff-code-review-source-cr-buffer source)))
                  (when (buffer-live-p cr-buf)
                    (multi-file-ediff--files-from-raw-diff
                     (with-current-buffer cr-buf
                       (code-review-db--pullreq-raw-diff)))))))))

(defvar-local multi-file-ediff--source nil
  "Source struct for the ediff buffer.")
(defvar-local multi-file-ediff--path nil
  "File path being ediff'd in this buffer.")
(defvar-local multi-file-ediff--side nil
  "`old or `new — which side of the ediff this buffer is.")

(cl-defmethod multi-file-ediff--make-buffers
  ((source multi-file-ediff-code-review-source) path)
  (let* ((default-directory
          (multi-file-ediff-code-review-source-default-directory source))
         (base (multi-file-ediff-code-review-source-base-ref source))
         (head (multi-file-ediff-code-review-source-head-ref source))
         (old-buf (multi-file-ediff--magit-file-buffer base path "old"))
         (new-buf (multi-file-ediff--magit-file-buffer head path "new")))
    (with-current-buffer old-buf
      (setq-local multi-file-ediff--source source)
      (setq-local multi-file-ediff--path path)
      (setq-local multi-file-ediff--side 'old))
    (with-current-buffer new-buf
      (setq-local multi-file-ediff--source source)
      (setq-local multi-file-ediff--path path)
      (setq-local multi-file-ediff--side 'new)
      (multi-file-ediff-comment-mode 1)
      (condition-case err
          (multi-file-ediff--render-comments-in-buffer)
        (error
         (message "multi-file-ediff: comment overlay render failed: %S" err))))
    (cons old-buf new-buf)))


;;;; Comment minor mode (in new-side ediff buffer)

(defvar multi-file-ediff-comment-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-e") #'multi-file-ediff-add-comment)
    m)
  "Keymap for `multi-file-ediff-comment-mode'.")

(define-minor-mode multi-file-ediff-comment-mode
  "Minor mode for adding code-review comments from a multi-file-ediff buffer."
  :lighter " MFE-CR"
  :keymap multi-file-ediff-comment-mode-map)

(defun multi-file-ediff-add-comment ()
  "Start a code-review comment at the current line."
  (interactive)
  (unless (and multi-file-ediff--source
               (multi-file-ediff-code-review-source-p multi-file-ediff--source))
    (user-error "Not in a code-review multi-file-ediff buffer"))
  (unless (eq multi-file-ediff--side 'new)
    (user-error "Comments can only be added on the new side"))
  (let* ((source multi-file-ediff--source)
         (path multi-file-ediff--path)
         (line (line-number-at-pos))
         (cr-buf (multi-file-ediff-code-review-source-cr-buffer source))
         (raw-diff (with-current-buffer cr-buf
                     (code-review-db--pullreq-raw-diff)))
         (pos+type (multi-file-ediff--new-line-to-diff-position
                    raw-diff path line)))
    (unless pos+type
      (user-error "Line %d of %s is not within a changed hunk" line path))
    (multi-file-ediff--open-comment-edit-buffer
     source path line (car pos+type) (cdr pos+type) (current-buffer))))


;;;; Comment edit buffer

(defvar-local multi-file-ediff--edit-context nil
  "Plist with edit state: :source :path :line :position :line-type :ediff-buffer :placeholder.")

(defvar multi-file-ediff-comment-edit-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-c") #'multi-file-ediff-commit-comment)
    (define-key m (kbd "C-c C-k") #'multi-file-ediff-cancel-comment)
    m)
  "Keymap for `multi-file-ediff-comment-edit-mode'.")

(define-derived-mode multi-file-ediff-comment-edit-mode text-mode "MFE-CommentEdit"
  "Major mode for editing a code-review comment from multi-file-ediff."
  (setq-local header-line-format
              "C-c C-c: commit  |  C-c C-k: cancel"))

(defun multi-file-ediff--placeholder-overlay (ediff-buf line)
  "Add a placeholder overlay on LINE in EDIFF-BUF."
  (with-current-buffer ediff-buf
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- line))
      (let ((ov (make-overlay (line-beginning-position)
                              (line-beginning-position))))
        (overlay-put ov 'before-string
                     (propertize "✎ pending comment\n" 'face 'warning))
        (overlay-put ov 'multi-file-ediff-placeholder t)
        ov))))

(defun multi-file-ediff--open-comment-edit-buffer (source path line position line-type ediff-buf)
  "Pop up an edit buffer for a comment at PATH:LINE."
  (let* ((edit-buf (generate-new-buffer
                    (format "*MFE-Comment %s:%d*" path line)))
         (placeholder (multi-file-ediff--placeholder-overlay ediff-buf line)))
    (with-current-buffer edit-buf
      (multi-file-ediff-comment-edit-mode)
      (setq-local multi-file-ediff--edit-context
                  (list :source source :path path :line line
                        :position position :line-type line-type
                        :ediff-buffer ediff-buf :placeholder placeholder)))
    ;; Track for session cleanup
    (when multi-file-ediff--current-session
      (multi-file-ediff--track-buffer multi-file-ediff--current-session edit-buf))
    (pop-to-buffer edit-buf
                   '((display-buffer-below-selected)
                     (window-height . 10)))))

(defun multi-file-ediff-commit-comment ()
  "Commit the comment in the edit buffer."
  (interactive)
  (let* ((ctx multi-file-ediff--edit-context)
         (body (string-trim (buffer-substring-no-properties
                             (point-min) (point-max))))
         (edit-buf (current-buffer)))
    (when (string-empty-p body)
      (user-error "Empty comment; use C-c C-k to cancel"))
    (condition-case err
        (multi-file-ediff--store-local-comment ctx body)
      (error
       (message "multi-file-ediff: failed to store comment: %S" err)
       (signal (car err) (cdr err))))
    (let ((ph (plist-get ctx :placeholder)))
      (when (overlayp ph) (delete-overlay ph)))
    (when (buffer-live-p (plist-get ctx :ediff-buffer))
      (with-current-buffer (plist-get ctx :ediff-buffer)
        (condition-case err
            (multi-file-ediff--render-comments-in-buffer)
          (error
           (message "multi-file-ediff: render failed: %S" err)))))
    (when-let ((win (get-buffer-window edit-buf)))
      (delete-window win))
    (when (buffer-live-p edit-buf)
      (let ((kill-buffer-query-functions nil))
        (kill-buffer edit-buf)))))

(defun multi-file-ediff-cancel-comment ()
  "Cancel the comment edit."
  (interactive)
  (let ((ph (plist-get multi-file-ediff--edit-context :placeholder)))
    (when (overlayp ph) (delete-overlay ph)))
  (let ((edit-buf (current-buffer)))
    (when-let ((win (get-buffer-window edit-buf)))
      (delete-window win))
    (kill-buffer edit-buf)))


;;;; Storage: write into code-review's local-comment store

(defun multi-file-ediff--store-local-comment (ctx body)
  "Store BODY as a local comment per CTX in code-review's data."
  (let* ((source (plist-get ctx :source))
         (cr-buf (multi-file-ediff-code-review-source-cr-buffer source))
         (path (plist-get ctx :path))
         (position (plist-get ctx :position))
         (line-type (plist-get ctx :line-type))
         (obj (code-review-local-comment-section
               :state "LOCAL COMMENT"
               :author (code-review-utils--git-get-user)
               :path path
               :position position
               :line-type line-type
               :send? nil
               :msg body
               :internalId (uuidgen-4))))
    (with-current-buffer cr-buf
      (code-review-comment-handler-commit obj ""))))


;;;; Overlay rendering: show stored comments inline in ediff buffer

(defcustom multi-file-ediff-comment-wrap-column 70
  "Column at which to wrap comment body text in overlays."
  :type 'integer
  :group 'multi-file-ediff)

(defcustom multi-file-ediff-comment-render-html t
  "If non-nil, render comment HTML bodies via `shr' for richer overlays."
  :type 'boolean
  :group 'multi-file-ediff)

(defface multi-file-ediff-comment-overlay-face
  '((((background light))
     :background "#f5efe8" :foreground unspecified :extend t)
    (((background dark))
     :background "#2c2630" :foreground unspecified :extend t))
  "Face for the boxed inline comment overlays."
  :group 'multi-file-ediff)

(defface multi-file-ediff-comment-heading-face
  '((t :inherit multi-file-ediff-comment-overlay-face :weight bold))
  "Face for the heading row of comment overlays."
  :group 'multi-file-ediff)

(defcustom multi-file-ediff-comment-bg-alpha 0.05
  "Alpha for blending `default' fg into bg to tint comment overlays.
0 = exactly the buffer bg (invisible).  Higher = more contrast."
  :type 'number
  :group 'multi-file-ediff)

(defun multi-file-ediff--update-comment-faces (&rest _)
  "Re-derive comment overlay faces from the current theme's `default' colors.
Picks a background slightly lifted toward the foreground so the boxes
sit just above the buffer bg."
  (let* ((bg (or (face-background 'default nil 'default) "#000000"))
         (fg (or (face-foreground 'default nil 'default) "#ffffff"))
         (tinted (if (fboundp 'doom-blend)
                     (doom-blend fg bg multi-file-ediff-comment-bg-alpha)
                   bg)))
    (face-spec-set 'multi-file-ediff-comment-overlay-face
                   `((t :background ,tinted :foreground unspecified :extend t)))
    (face-spec-set 'multi-file-ediff-comment-heading-face
                   '((t :inherit multi-file-ediff-comment-overlay-face
                        :weight bold :box nil)))))

(multi-file-ediff--update-comment-faces)
(add-hook 'doom-load-theme-hook #'multi-file-ediff--update-comment-faces)

(defun multi-file-ediff--comment-author (c)
  (cond ((eieio-object-p c) (and (slot-boundp c 'author) (oref c author)))
        ((listp c) (or (alist-get 'author c)
                       (alist-get 'login (alist-get 'author c))))))

(defun multi-file-ediff--strip-html (s)
  "Strip HTML tags from S and decode common entities."
  (when (stringp s)
    (let ((stripped (replace-regexp-in-string "<[^>]+>" "" s)))
      (replace-regexp-in-string
       "&amp;" "&"
       (replace-regexp-in-string
        "&lt;" "<"
        (replace-regexp-in-string
         "&gt;" ">"
         (replace-regexp-in-string "&quot;" "\"" stripped)))))))

(defun multi-file-ediff--strip-shr-table-padding (text)
  "Remove shr's pixel-align `display' specs from TEXT.
shr renders tables with `(display (space :align-to N))' that break
monospace alignment.  We replace those with a single space."
  (when (stringp text)
    (let ((s (copy-sequence text)) (i 0))
      (while (< i (length s))
        (let ((d (get-text-property i 'display s)))
          (when (and (consp d)
                     (eq (car-safe d) 'space)
                     (plist-get (cdr d) :align-to))
            (remove-list-of-text-properties i (1+ i) '(display) s)))
        (cl-incf i))
      s)))

(defun multi-file-ediff--render-html (html)
  "Render HTML to propertized plain text via `shr'."
  (when (stringp html)
    (require 'shr)
    (with-temp-buffer
      (insert html)
      (let ((shr-use-fonts nil)
            (shr-use-colors t)
            (shr-width (max 4 (- multi-file-ediff-comment-wrap-column 4))))
        (shr-render-region (point-min) (point-max)))
      (multi-file-ediff--strip-shr-table-padding
       (string-trim-right (buffer-string))))))

(defun multi-file-ediff--comment-body (c)
  (let ((raw (cond ((eieio-object-p c) (and (slot-boundp c 'msg) (oref c msg)))
                   ((listp c) (or (alist-get 'bodyText c)
                                  (alist-get 'body c))))))
    (cond
     ((not raw) "")
     ((and multi-file-ediff-comment-render-html
           (string-match-p "<[^>]+>" raw))
      (multi-file-ediff--render-html raw))
     (t (multi-file-ediff--strip-html raw)))))

(defun multi-file-ediff--wrap-lines (text width)
  "Return TEXT as a list of lines fitting WIDTH columns.
If TEXT already contains newlines (e.g. shr-rendered output), split on them
and trim trailing whitespace; otherwise word-wrap via `fill-region'."
  (cond
   ((null text) (list ""))
   ((string-match-p "\n" text)
    (mapcar #'string-trim-right (split-string text "\n")))
   (t
    (with-temp-buffer
      (insert text)
      (let ((fill-column width)
            (sentence-end-double-space nil))
        (fill-region (point-min) (point-max)))
      (mapcar #'string-trim-right
              (split-string (buffer-string) "\n"))))))

(defun multi-file-ediff--rule-string (n)
  (apply #'concat (make-list n "─")))

(defconst multi-file-ediff--emoji-regex
  "[\u2600-\u27BF\U0001F300-\U0001FAFF]"
  "Matches emoji code-points whose color-font rendering exceeds string-width.")

(defun multi-file-ediff--count-emoji (text)
  (let ((c 0) (pos 0))
    (while (string-match multi-file-ediff--emoji-regex text pos)
      (cl-incf c)
      (setq pos (match-end 0)))
    c))

(defun multi-file-ediff--visual-width (text)
  "Visual width of TEXT, treating each emoji as +0.5 cols (rounded down to 0)
over `string-width'.  Most color-emoji fonts render emoji at ~2.5 cols."
  (string-width text))

(defun multi-file-ediff--pad-to (text inner)
  "Right-pad TEXT to INNER visual columns.
Each emoji char (which fonts often render at ~2.5 cols vs string-width's 2)
contributes a `(space :width 0.5)' spacer to compensate.  GUI only — in
tty falls back to integer alignment."
  (let* ((emojis (multi-file-ediff--count-emoji text))
         (regular (max 0 (- inner (string-width text)))))
    (if (zerop emojis)
        (concat text (make-string regular ?\s))
      (let* ((whole-cols (/ emojis 2))
             (has-half (cl-oddp emojis))
             (final-regular (max 0 (- regular whole-cols
                                      (if has-half 1 0))))
             (half-spacer (and has-half
                               (propertize " " 'display
                                           '(space :width 0.5)))))
        (concat text
                (make-string final-regular ?\s)
                (or half-spacer ""))))))

(defun multi-file-ediff--build-comment-block (author body width face heading-face)
  "Render a single comment as a boxed block of WIDTH columns."
  (let* ((inner (max 4 (- width 4)))
         (heading (format "💬 @%s" (or author "?")))
         (heading-padded (multi-file-ediff--pad-to heading inner))
         (body-lines (multi-file-ediff--wrap-lines (or body "") inner))
         (top (concat "╭─" (multi-file-ediff--rule-string inner) "─╮"))
         (bot (concat "╰─" (multi-file-ediff--rule-string inner) "─╯"))
         (rows (cons (propertize top 'face face)
                     (append
                      (list (concat (propertize "│ " 'face face)
                                    (propertize heading-padded 'face heading-face)
                                    (propertize " │" 'face face)))
                      (mapcar (lambda (line)
                                (propertize
                                 (concat "│ "
                                         (multi-file-ediff--pad-to line inner)
                                         " │")
                                 'face face))
                              body-lines)
                      (list (propertize bot 'face face))))))
    (mapconcat #'identity rows "\n")))

(defun multi-file-ediff--comment-overlay-text (comments)
  "Build the boxed overlay string for COMMENTS at a single line."
  (let ((width multi-file-ediff-comment-wrap-column))
    (concat
     (mapconcat
      (lambda (c)
        (multi-file-ediff--build-comment-block
         (multi-file-ediff--comment-author c)
         (multi-file-ediff--comment-body c)
         width
         'multi-file-ediff-comment-overlay-face
         'multi-file-ediff-comment-heading-face))
      comments
      "\n")
     "\n")))

(defun multi-file-ediff--comments-for-path (cr-buf path)
  "Return a list of (NEW-LINE . COMMENT-LIST) pairs for PATH."
  (let* ((raw-diff (with-current-buffer cr-buf
                     (code-review-db--pullreq-raw-diff)))
         (raw-comments (with-current-buffer cr-buf
                         (code-review-db--pullreq-raw-comments)))
         (groups (and raw-comments (code-review-utils-make-group raw-comments)))
         (path-prefix (concat path ":"))
         result)
    (when groups
      (dolist (entry groups)
        (let ((key (car entry))
              (comments (cdr entry)))
          (when (string-prefix-p path-prefix key)
            (let* ((pos (string-to-number
                         (substring key (length path-prefix))))
                   (new-line (multi-file-ediff--diff-position-to-new-line
                              raw-diff path pos)))
              (when new-line
                (push (cons new-line comments) result)))))))
    result))

(defun multi-file-ediff--render-comments-in-buffer ()
  "Render comment overlays in the current new-side ediff buffer."
  (when (and (eq multi-file-ediff--side 'new)
             (multi-file-ediff-code-review-source-p multi-file-ediff--source))
    ;; Clear existing overlays we previously rendered.
    (remove-overlays (point-min) (point-max)
                     'multi-file-ediff-comment-overlay t)
    (let* ((source multi-file-ediff--source)
           (cr-buf (multi-file-ediff-code-review-source-cr-buffer source))
           (path multi-file-ediff--path)
           (entries (multi-file-ediff--comments-for-path cr-buf path)))
      (save-excursion
        (dolist (entry entries)
          (let ((line (car entry))
                (comments (cdr entry)))
            (goto-char (point-min))
            (forward-line (1- line))
            (let ((ov (make-overlay (line-beginning-position)
                                    (line-beginning-position))))
              (overlay-put ov 'before-string
                           (multi-file-ediff--comment-overlay-text comments))
              (overlay-put ov 'multi-file-ediff-comment-overlay t))))))))

(defun multi-file-ediff--forge-worktree-for (owner repo)
  "Return the local worktree directory forge knows for OWNER/REPO, or nil."
  (require 'forge)
  (caar (forge-sql [:select [worktree]
                             :from repository
                             :where (= owner $s1)
                             :and   (= name  $s2)
                             :limit 1]
                   owner repo)))

;;;###autoload
(defun multi-file-ediff-code-review ()
  "Start multi-file ediff for the PR loaded in the current code-review buffer."
  (interactive)
  (unless (eq major-mode 'code-review-mode)
    (user-error "Not in a code-review buffer"))
  (let* ((cr-buf (current-buffer))
         (pr (code-review-db-get-pullreq))
         (_ (unless pr
              (user-error "Could not load PR data; try `M-x code-review-reload'")))
         (infos (code-review-db--pullreq-raw-infos))
         (base-ref-name (alist-get 'baseRefName infos))
         (head-ref-oid (alist-get 'headRefOid infos))
         (number (oref pr number))
         ;; The code-review buffer's default-directory is whatever buffer the
         ;; user opened it from — often the wrong repo.  Look up the PR's
         ;; actual local clone in forge's database.
         (dir (or (multi-file-ediff--forge-worktree-for
                   (oref pr owner) (oref pr repo))
                  default-directory)))
    ;; Fetch first: needed for fresh clones where neither the PR head nor
    ;; the base remote-tracking branch is yet in the local repo.
    (let ((default-directory dir))
      (unless (and head-ref-oid (magit-rev-verify head-ref-oid))
        (let ((spec (format "+refs/pull/%s/head:refs/pullreqs/%s"
                            number number)))
          (ignore-errors (magit-call-git "fetch" "origin" spec))))
      (let ((remote (magit-get-current-remote)))
        (unless (and remote
                     (magit-rev-verify (concat remote "/" base-ref-name)))
          (ignore-errors (magit-call-git "fetch" "origin")))))
    ;; Now resolve refs.
    (let* ((default-directory dir)
           (forge-ref (let ((ref (format "refs/pullreqs/%s" number)))
                        (and (magit-rev-verify ref) ref)))
           (head-ref (cond ((and head-ref-oid (magit-rev-verify head-ref-oid))
                            head-ref-oid)
                           (forge-ref forge-ref)
                           (t nil)))
           (remote (magit-get-current-remote))
           (base-ref (or (and remote
                              (let ((rev (concat remote "/" base-ref-name)))
                                (and (magit-rev-verify rev) rev)))
                         (and (magit-rev-verify base-ref-name) base-ref-name)))
           (base-sha (and base-ref head-ref
                          (magit-git-string "merge-base" base-ref head-ref))))
      (unless head-ref
        (user-error "Head SHA still not in local repo after fetch (PR #%s)"
                    number))
      (unless base-ref
        (user-error "Base ref %s still not in local repo after fetch"
                    base-ref-name))
      (let ((source (multi-file-ediff-code-review-source-make
                     :pullreq pr
                     :cr-buffer cr-buf
                     :base-ref (or base-sha base-ref)
                     :head-ref head-ref
                     :default-directory dir
                     :display-title (format "PR #%s: %s"
                                            number
                                            (or (oref pr title) "")))))
        (multi-file-ediff-start source)))))


;;;; winum integration

(defun multi-file-ediff--winum-assign ()
  "Assign 0 to the multi-file-ediff sidebar window."
  (when (eq major-mode 'multi-file-ediff-mode) 0))

(with-eval-after-load 'winum
  (add-to-list 'winum-assign-functions
               #'multi-file-ediff--winum-assign))

(provide '+multi-file-ediff)
;;; +multi-file-ediff.el ends here
