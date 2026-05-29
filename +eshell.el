;;; ~/.doom.d/+eshell.el -*- lexical-binding: t; -*-

(declare-function eshell-find-single-file "config")

(setopt eshell-history-size 9999999)

(defvar eshell-history-ring nil)
(defun eshell-append-history ()
  "Call `eshell-write-history' with the `append' parameter set to `t'."
  (when eshell-history-ring
    (let ((newest-cmd-ring (make-ring 1)))
      (ring-insert newest-cmd-ring (car (ring-elements eshell-history-ring)))
      (let ((eshell-history-ring newest-cmd-ring))
        (eshell-write-history eshell-history-file-name t)))))

(defvar +eshell-shared-history-ring nil
  "Single history ring shared across all eshell buffers in this session.")

(defun +eshell-init-shared-history ()
  "Point this eshell buffer's history ring at the session-shared ring.
On the first eshell, adopts the freshly-loaded buffer-local ring as the
shared one.  Subsequent eshells then use the same ring object, so a
command run in buffer A is immediately visible to history navigation in
buffer B without a disk round-trip."
  (unless +eshell-shared-history-ring
    (setq +eshell-shared-history-ring eshell-history-ring))
  (setq-local eshell-history-ring +eshell-shared-history-ring))

(map! :leader "o E" #'+eshell/new)
(defun +eshell/new ()
  (interactive)
  (eshell t))

(after! eshell
  (require '+completions)
  (setq su-mode t)
  (setq su-auto-save-mode t)
  (setq eshell-save-history-on-exit nil)
  (require 'em-glob)

  ;; cache file-name forever
  (setq remote-file-name-inhibit-cache nil)

  ;; make sure vc stuff is not making tramp slower
  (setq vc-ignore-dir-regexp
        (format "%s\\|%s"
                vc-ignore-dir-regexp
                tramp-file-name-regexp))

  (setq tramp-verbose 1)

  ;; projectile has the fun side-effect of wanting to calculate the
  ;; project name, which makes tramp oh-so-much-slower.
  (setq projectile-mode-line "Projectile")

  (add-to-list 'eshell-modules-list 'eshell-rebind)
  (add-to-list 'eshell-modules-list 'eshell-tramp)
  (setq eshell-cmpl-dir-ignore "\\`\\(CVS\\)/\\'")

  (setq eshell-scroll-to-bottom-on-output t)
  (setq eshell-error-if-no-glob nil)
  (add-hook 'eshell-mode-hook #'+eshell-init-shared-history 90)
  (add-hook 'eshell-pre-command-hook #'eshell-append-history)
  (add-hook 'eshell-post-command-hook #'envrc--update)
  (remove-hook 'eshell-mode-hook #'hide-mode-line-mode)
  (setq vterm-kill-buffer-on-exit t)
  ;; remove confirmation for process buffers
  (setq kill-buffer-query-functions
        (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

  (setq eshell-exit-hook nil)
  (setq eshell-destroy-buffer-when-process-dies t)

  (setq corfu-auto-prefix 0
        corfu-auto-delay 0.0
        corfu-auto t)
  (setq company-minimum-prefix-length 0
        company-idle-delay 0.0)
  (setq password-cache t
        password-cache-expiry 3600)
                                        ; (setenv "KUBECONFIG" (string-join (eshell-extended-glob "/Users/ccomar/.kube/(*.(yaml|config)|config)") ":"))

  (defun +eshell-buffer-contents ()
    "get the contents of the current buffer, ensuring it's font locked."
    (if (fboundp 'font-lock-ensure)
        (font-lock-ensure)
      (with-no-warnings (font-lock-fontify-buffer)))
    (let ((contents (buffer-string)))
      (remove-text-properties 0 (length contents) '(read-only nil) contents)
      contents))

  (defun eshell-cat-with-syntax-highlight (file-or-buf)
    "Like cat(1) but with syntax highlighting."
    (if (bufferp file-or-buf)
        (eshell-print (with-current-buffer file-or-buf (+eshell-buffer-contents)))
      (let ((existing-buffer (get-file-buffer file-or-buf))
            (buffer (find-file-noselect file-or-buf)))
        (eshell-print (with-current-buffer buffer (+eshell-buffer-contents)))
        (unless existing-buffer (kill-buffer buffer))
        nil)))
  (advice-add 'eshell/cat :override #'eshell-cat-with-syntax-highlight)

  (defun +eshell/here (&optional command)
    "Open eshell in the current window."
    (interactive "P")
    (let ((buf (+eshell--unused-buffer t)))
      (with-current-buffer (switch-to-buffer buf)
        (if (eq major-mode 'eshell-mode)
            (run-hooks 'eshell-mode-hook)
          (eshell-mode))
        (when command
          (+eshell-run-command command buf)))
      buf))

  (defun +eshell-fish-path (path max-len)
    "Return a potentially trimmed-down version of the directory PATH,
     replacing parent directories with their initial characters to
     try to get the character length of PATH (sans directory slashes)
     down to MAX-LEN."
    (let* ((components (split-string (abbreviate-file-name path) "/"))
           (len (+ (1- (length components))
                   (cl-reduce '+ components :key 'length)))
           (str ""))
      (while (and (> len max-len)
                  (cdr components))
        (setq str (concat str
                          (cond ((= 0 (length (car components))) "/")
                                ((= 1 (length (car components)))
                                 (concat (car components) "/"))
                                (t
                                 (if (string= "."
                                              (string (elt (car components) 0)))
                                     (concat (substring (car components) 0 2)
                                             "/")
                                   (string (elt (car components) 0) ?/)))))
              len (- len (1- (length (car components))))
              components (cdr components)))
      (concat str (cl-reduce (lambda (a b) (concat a "/" b)) components))))

  ;; synchronize buffer name on directory change.
  (defun +eshell-sync-dir-buffer-name ()
    "Change eshell buffer name by directory change."
    (when (equal major-mode 'eshell-mode)
      (rename-buffer (format "eshell: %s" (+eshell-fish-path default-directory 30))
                     t)))

  (add-hook 'eshell-directory-change-hook #'+eshell-sync-dir-buffer-name)
  (add-hook 'eshell-mode-hook #'+eshell-sync-dir-buffer-name)

  (require 'aio)
  (defun aio-call-process (name buffer cmd)
    (let ((process (apply #'start-process name buffer "bash" (list "-c" cmd)))
          (promise (aio-promise))
          (s (aio-make-select)))
      (aio-select-add
       s
       (prog1 promise
         (setf (process-sentinel process)
               (lambda (_ status) (aio-resolve promise (lambda () status))))))))

  (aio-defun aio-run (name cmd)
    (letrec ((curr (current-buffer))
             (temp (get-buffer-create (combine-and-quote-strings (list "*" name "*") " ")))
             (_cap (set-buffer temp))
             (_a (erase-buffer)))
      (aio-await (apply #'aio-call-process name (current-buffer) cmd))
      (let ((r (buffer-string))
            (_cap (set-buffer curr)))
        r)))

  (defun eshell/async-command-to-string (cmd &rest args)
    (aio-wait-for (aio-run cmd (list (combine-and-quote-strings (cons cmd args) " ")))))

  (defun +esh-help/async-man-string (cmd)
    "Return help string for the shell command CMD."
    (let ((lang (getenv "LANG")))
      (setenv "LANG" "C")
      (let ((str (aio-wait-for (aio-run manual-program (list (format "%s %s | col -b" manual-program (file-name-base cmd)))))))
        (setenv "LANG" lang)
        str)))

  (advice-add 'esh-help-man-string :override #'+esh-help/async-man-string)
  (advice-add 'eshell/async-command-to-string :around #'envrc-propagate-environment))

(use-package! tramp
  :defer t
  :config
  ;; (eval-when-compile (require 'tramp))
  ;; Define a rsyncx method analogous to scpx
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (add-to-list 'tramp-methods
               `("rsyncx"
                 (tramp-login-program "ssh")
                 (tramp-login-args (("-l" "%u") ("-p" "%p") ("%c") ("-e" "none")
                                    ("-t" "-t") ("%h") ("/bin/bash")))
                 (tramp-async-args (("-q")))
                 (tramp-remote-shell "/bin/bash")
                 (tramp-remote-shell-login ("-l"))
                 (tramp-remote-shell-args ("-c"))
                 (tramp-copy-program
                  ,(if (and (eq system-type 'darwin)
                            (file-executable-p "/usr/bin/rsync"))
                       "/usr/bin/rsync"
                     "rsync"))
                 (tramp-copy-args (("-t" "%k") ("-p") ("-r") ("-s") ("-c")))
                 (tramp-copy-env (("RSYNC_RSH")
                                  ("ssh" "%c")))
                 (tramp-copy-keep-date t)
                 (tramp-copy-keep-tmpfile t)
                 (tramp-copy-recursive t))))

;; (use-package! aweshell)

(defun eshell/git (command &rest args)
  (pcase command
    ("pr" (apply #'algernon/git-pr args))
    ("log" (apply #'algernon/git-log args))
    ("status" (progn (magit-status) nil))
    ("grep" (apply #'algernon/git-grep args))
    (_  (progn (eshell-wait-for-processes (list (eshell-external-command "git" (cons command args)))) nil))))

;; (use-package! eshell-git-prompt
;;   :after eshell
;;   :config
;;   (defun eshell-git-prompt-powerline2 ()
;;     (let ((segment-separator "\xe0b0")
;;           (branch            "\xe0a0")
;;           (detached          "\x27a6")
;;           (cross             "\x2718")
;;           dir git git-face sign)
;;       (setq dir
;;             (propertize
;;              (concat
;;               " "
;;               (unless (eshell-git-prompt-exit-success-p)
;;                 (concat cross " "))
;;               (eshell-git-prompt-powerline-dir)
;;               " ")
;;              'face 'eshell-git-prompt-powerline-dir-face))
;;       (setq git
;;             (when (eshell-git-prompt--git-root-dir)
;;               (setq git-face
;;                     (if (eshell-git-prompt--collect-status)
;;                         'eshell-git-prompt-powerline-not-clean-face
;;                       'eshell-git-prompt-powerline-clean-face))
;;               (setq eshell-git-prompt-branch-name (eshell-git-prompt--branch-name))
;;               (propertize
;;                (concat " "
;;                        (-if-let (branch-name eshell-git-prompt-branch-name)
;;                            (concat branch " " branch-name)
;;                          (concat detached " "(eshell-git-prompt--commit-short-sha)))
;;                        " ")
;;                'face git-face)))
;;       (setq sign
;;             (concat
;;              (with-face "\n└─" 'eshell-git-prompt-multiline2-secondary-face)
;;              (if (not (eshell-git-prompt-exit-success-p))
;;                  (with-face ">>" 'eshell-git-prompt-multiline2-fail-face)
;;                (with-face ">>" 'eshell-git-prompt-multiline2-secondary-face))))
;;       (eshell-git-prompt---str-read-only
;;        (concat
;;         (with-face "┌─" 'eshell-git-prompt-multiline2-secondary-face)
;;         (if git
;;             (concat dir
;;                     (with-face segment-separator
;;                       :foreground (face-background 'eshell-git-prompt-powerline-dir-face)
;;                       :background (face-background git-face))
;;                     git
;;                     (with-face segment-separator
;;                       :foreground (face-background git-face)))
;;           (concat dir
;;                   (with-face segment-separator
;;                     :foreground (face-background 'eshell-git-prompt-powerline-dir-face))))
;;         sign " "))))

;;   (defconst eshell-git-prompt-powerline2-regexp "^[^$\n]*└─>>  ")
;;   (add-to-list 'eshell-git-prompt-themes
;;                '(powerline2
;;                  eshell-git-prompt-powerline2
;;                  eshell-git-prompt-powerline2-regexp))
;;   (eshell-git-prompt-use-theme 'powerline2))
(use-package! eshell-p10k
  :after eshell
  :config
  (remove-hook! 'eshell-post-command-hook #'+eshell-protect-output-in-visual-modes-h)
  (remove-hook! 'eshell-pre-command-hook #'+eshell-protect-input-in-visual-modes-h)
  (setq eshell-prompt-function #'eshell-p10k-default-prompt
        eshell-prompt-regexp eshell-p10k-prompt-regex))

(use-package! awscli-capf
  :defer t
  :after eshell)

(add-hook! eshell-mode
  (setenv "TERM" "xterm-256color")
  (map! :map eshell-mode-map :ni "C-r" #'consult-history)
  (map! :map eshell-command-map :ni "C-r" #'consult-history)
  (map! :map eshell-mode-map :i "C-d" #'eshell-send-eof-to-process)
  (map! :map eshell-mode-map :nv
        "$" #'evil-end-of-line))

(add-hook! eshell-mode
           #'with-editor-export-git-editor
           #'with-editor-export-editor
           #'global-fish-completion-mode
           #'solaire-mode
           ;; #'awscli-capf-add
           #'(lambda () (eshell/alias "git"))
           (mode-line-invisible-mode -1))

(defun algernon/git-grep (&rest args)
  (interactive)

  (let* ((command (format "git grep -n %s" (s-join " " args)))
         (buffer (generate-new-buffer "*git-grep*"))
         (buffer-name (format "#<buffer %s>" buffer)))
    (with-current-buffer buffer
      (insert (format "-*- mode: grep; default-directory: \"%s\" -*-\n\n%s\n"
                      (eshell/pwd) command)))
    (eshell-printn buffer-name)
    (eshell-do-eval
     (eshell-parse-command (format "*%s >>%s" command buffer-name))
     t)
    (with-current-buffer buffer
      (funcall 'grep-mode)
      (goto-char (point-min))
      (switch-to-buffer buffer))))

(defun algernon/git-log (&rest args)
  (let* ((branch-or-file (car args))
         (file-list (if (and branch-or-file (f-file-p branch-or-file))
                        args
                      (cdr args)))
         (branch (if (and branch-or-file (f-file-p branch-or-file))
                     "HEAD"
                   branch-or-file)))
    (message branch-or-file)
    (if branch-or-file
        (magit-log-other (list branch) '()
                         (mapcar (lambda (f) (concat (eshell/pwd) "/" f))
                                 file-list))
      (magit-log-head)))
  nil)

(defun algernon/git-pr (pr branch)
  (eshell-do-eval
   (eshell-parse-command (format "git fetch origin pull/%s/head:%s" pr branch)) t))

(defun eshell/find-file (&rest args)
  "Open file even if it is not owned by you via sudo. Only adds sudo if needed."
  (mapcar #'eshell-find-single-file args))

(defun eshell/vim (&rest args)
  (mapcar #'eshell-find-single-file args))

