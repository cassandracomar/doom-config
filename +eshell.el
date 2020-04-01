;;; ~/.doom.d/+eshell.el -*- lexical-binding: t; -*-

(setq! eshell-history-size 9999999)

(defun eshell/ivy-history ()
  "Interactive search eshell history."
  (interactive)
  (require 'em-hist)
  (save-excursion
    (let* ((start-pos (eshell-bol))
       (end-pos (point-at-eol))
       (input (buffer-substring-no-properties start-pos end-pos)))
      (let* ((command (ivy-read "Command: "
                (delete-dups
                 (aweshell-parse-shell-history))
                :preselect input
                :action #'ivy-completion-in-region-action))
         (cursor-move (length command)))
    (kill-region (+ start-pos cursor-move) (+ end-pos cursor-move))
    )))
  ;; move cursor to eol
  (end-of-line)
  )

(defun eshell-append-history ()
  "Call `eshell-write-history' with the `append' parameter set to `t'."
  (when eshell-history-ring
    (let ((newest-cmd-ring (make-ring 1)))
      (ring-insert newest-cmd-ring (car (ring-elements eshell-history-ring)))
      (let ((eshell-history-ring newest-cmd-ring))
        (eshell-write-history eshell-history-file-name t)))))

(defun eshell-update-history ()
  (eshell-read-history eshell-history-file-name)
  (when eshell-history-ring
    (let ((old-ring (copy-list eshell-history-ring)))
      (setq eshell-history-ring (list (car eshell-history-ring)))
                                    ; write
      (setq eshell-history-ring old-ring))))

(add-hook! eshell-pre-command-hook #'eshell-append-history)
(add-hook! eshell-post-command-hook #'eshell-update-history)


(defun pinentry-emacs (desc prompt ok error)
  (let ((str (read-passwd (concat (replace-regexp-in-string "%22" "\"" (replace-regexp-in-string "%0A" "\n" desc)) prompt ": "))))
    str))

(let ((tty (shell-command-to-string "tty")))
  (setenv "GPG_TTY" tty)
  (setenv "SSH_AUTH_SOCK" "/run/user/1000/gnupg/S.gpg-agent.ssh"))

(defun tramp-aware-woman (man-page-path)
  (interactive)
  (let ((dir (eshell/pwd)))
    (woman-find-file
     (if (file-remote-p dir)
         (let ((vec (tramp-dissect-file-name dir)))
           (tramp-make-tramp-file-name
            (tramp-file-name-method vec)
            (tramp-file-name-user vec)
            (tramp-file-name-host vec)
            man-page-path))
       man-page-path))))

(defun pcmpl-git-commands ()
  "Return the most common git commands by parsing the git output."
  (with-temp-buffer
    (call-process-shell-command "git" nil (current-buffer) nil "help" "--all")
    (goto-char 0)
    (search-forward "Main Porcelain Commands")
    (let (commands)
      (while (re-search-forward
              "^[[:blank:]]+\\([[:word:]-.]+\\)[[:blank:]]*\\([[:word:]-.]+\\)?"
              nil t)
        (push (match-string 1) commands)
        (when (match-string 2)
          (push (match-string 2) commands)))
      (sort commands #'string<))))

(defconst pcmpl-git-commands (pcmpl-git-commands)
  "List of `git' commands.")

(defvar pcmpl-git-ref-list-cmd "git for-each-ref refs/ --format='%(refname)'"
  "The `git' command to run to get a list of refs.")

(defun pcmpl-git-get-refs (type)
  "Return a list of `git' refs filtered by TYPE."
  (with-temp-buffer
    (insert (shell-command-to-string pcmpl-git-ref-list-cmd))
    (goto-char (point-min))
    (let (refs)
      (while (re-search-forward (concat "^refs/" type "/\\(.+\\)$") nil t)
        (push (match-string 1) refs))
      (nreverse refs))))

(defun pcmpl-git-remotes ()
  "Return a list of remote repositories."
  (split-string (shell-command-to-string "git remote")))

(defun pcomplete/git ()
  "Completion for `git'."
  ;; Completion for the command argument.
  (pcomplete-here* pcmpl-git-commands)
  (cond
   ((pcomplete-match "help" 1)
    (pcomplete-here* pcmpl-git-commands))
   ((pcomplete-match (regexp-opt '("pull" "push")) 1)
    (pcomplete-here (pcmpl-git-remotes)))
   ;; provide branch completion for the command `checkout'.
   ((pcomplete-match "checkout" 1)
    (pcomplete-here* (append (pcmpl-git-get-refs "heads")
                             (pcmpl-git-get-refs "tags"))))
   (t
    (while (pcomplete-here (pcomplete-entries))))))

(defun vterm-exec (&rest args)
  (interactive)
  (evil-insert-state nil)
  (vterm-send-string (string-join args " "))
  (vterm-send-string "; exit")
  (vterm-send-return)
  )
(defun eshell-exec-visual-named (buf-name &rest args)
  "Run the specified PROGRAM in a terminal emulation buffer.
  ARGS are passed to the program.  At the moment, no piping of input is
  allowed."
  (let* (eshell-interpreter-alist
         (original-args args)
         (interp (eshell-find-interpreter (car args) (cdr args)))
         (in-ssh-tramp (and (tramp-tramp-file-p default-directory)
                            (equal (tramp-file-name-method
                                    (tramp-dissect-file-name default-directory))
                                   "ssh")))
         (program (if in-ssh-tramp
                      "ssh"
                    (car interp)))
         (args (if in-ssh-tramp
                   (let ((dir-name (tramp-dissect-file-name default-directory)))
                     (eshell-flatten-list
                      (list
                       "-t"
                       (tramp-file-name-host dir-name)
                       (format
                        "\"export TERM=xterm-256color; cd %s; exec %s\""
                        (tramp-file-name-localname dir-name)
                        (string-join
                         (append
                          (list (tramp-file-name-localname (tramp-dissect-file-name (car interp))))
                          (cdr args))
                         " ")))))
                 (eshell-flatten-list
                  (eshell-stringify-list (append (cdr interp)
                                                 (cdr args))))))
         (term-buf
          (generate-new-buffer
           (concat "*"
                   (if in-ssh-tramp
                       (format "%s %s" default-directory (string-join original-args " "))
                     (file-name-nondirectory program))
                   "*")))
         (eshell-buf (current-buffer)))
    (save-current-buffer
      (switch-to-buffer term-buf)
      (rename-buffer buf-name)
      (vterm-mode)
      (set (make-local-variable 'term-term-name) eshell-term-name)
      (make-local-variable 'eshell-parent-buffer)
      (setq eshell-parent-buffer eshell-buf)
                                    ;(term-exec term-buf program program nil args)
      (apply 'vterm-exec program args)
      (let ((proc (get-buffer-process term-buf)))
        (if (and proc (eq 'run (process-status proc)))
            (set-process-sentinel proc 'eshell-term-sentinel)
          (error "Failed to invoke visual command")))
      (term-char-mode)
      (if eshell-escape-control-x
          (term-set-escape-char ?\C-x))))
  nil)

(defun eshell/visual (&rest args)
  "Create a terminal buffer for a single visual command in Eshell."
  (if (equal "sudo" (car args))
      (apply 'eshell-exec-visual-named (cadr args) args)
    (apply 'eshell-exec-visual-named (car args) args))
  )

(defun eshell-exec-visual (&rest args)
  (apply 'eshell/visual args))

(after! eshell
  (setq su-mode t)
  (setq su-auto-save-mode t)
  (setq eshell-save-history-on-exit nil)

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
  (setq xterm-color-preserve-properties t)
  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  (setq eshell-output-filter-functions
        (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
  (setq with-editor-emacsclient-executable "/usr/bin/emacsclient")

  (add-to-list 'eshell-modules-list 'eshell-rebind)
  (add-to-list 'eshell-modules-list 'eshell-tramp)
  (setq eshell-cmpl-dir-ignore "\\`\\(CVS\\)/\\'")
  (setq su-auto-save-mode-lighter nil)

  (setq eshell-where-to-jump 'begin)
  (setq eshell-review-quick-commands nil)
  (setq eshell-smart-space-goes-to-end t)

  ;(add-to-list 'eshell-visual-commands "htop")
  ;(add-to-list 'eshell-visual-commands "openfortivpn")
  (setq eshell-exit-hook nil)
  (setq eshell-destroy-buffer-when-process-dies t)
  )

(after! tramp
  (eval-when-compile (require 'tramp))
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
                 (tramp-copy-recursive t)))
  )

; not lazy-loaded on purpose
(use-package! aweshell)

(defun eshell/git (command &rest args)
  (pcase command
    ("pr" (apply #'algernon/git-pr args))
    ("log" (apply #'algernon/git-log args))
    ("status" (progn
                (magit-status)
                (eshell/echo)))
    ("grep" (apply #'algernon/git-grep args))
    (_ (let ((command (s-join " " (append (list "git" command) args))))
         (message command)
         (shell-command-to-string command)))))

   ;; =================================
   ;; automatically request root access
   ;; =================================

(use-package! su
  :after eshell
  :init
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'el-patch)))

  (el-patch-feature su)

  (el-patch-defcustom su-auto-make-directory t
                      "Automatically become other users to create directories"
                      :type 'boolean
                      :group 'su)

  (el-patch-defcustom su-auto-write-file t
                      "Automatically become other users to write files"
                      :type 'boolean
                      :group 'su)

  (el-patch-defcustom su-auto-read-file t
                      "Automatically become other users to read files"
                      :type 'boolean
                      :group 'su)

  (el-patch-defcustom su-enable-helm-integration t
                      "Enable integration with helm"
                      :type 'boolean
                      :group 'su)

  (el-patch-defcustom su-enable-semantic-integration t
                      "Enable integration with semantic"
                      :type 'boolean
                      :group 'su)

  (autoload #'su--nadvice-make-directory-auto-root "su")
  (autoload #'su--nadvice-find-file-noselect "su")
  (autoload #'su--nadvice-supress-find-file-hook "su")
  (autoload #'su--nadvice-find-file-noselect-1 "su")

  (el-patch-define-minor-mode su-mode
                              "Automatically read and write files as users"
                              :init-value nil
                              :group 'su
                              :global t
                              (if su-mode
                                  (progn
                                    (when su-auto-make-directory
                                      (advice-add 'basic-save-buffer :around
                                                  #'su--nadvice-make-directory-auto-root)

                                      (when su-enable-helm-integration
                                        (with-eval-after-load 'helm-files
                                          (advice-add 'helm-find-file-or-marked :around
                                                      #'su--nadvice-make-directory-auto-root))))

                                    (when su-auto-write-file
                                      (add-hook 'find-file-hook #'su--edit-file-as-root-maybe)
                                      (advice-add 'find-file-noselect :around
                                                  #'su--nadvice-find-file-noselect)

                                      (when su-enable-semantic-integration
                                        (with-eval-after-load 'semantic/fw
                                          (advice-add 'semantic-find-file-noselect :around
                                                      #'su--nadvice-supress-find-file-hook))))

                                    (when su-auto-read-file
                                      (advice-add 'find-file-noselect-1 :around
                                                  #'su--nadvice-find-file-noselect-1)))

                                (remove-hook 'find-file-hook #'su--edit-file-as-root-maybe)
                                (advice-remove 'basic-save-buffer
                                               #'su--nadvice-make-directory-auto-root)
                                (advice-remove 'helm-find-file-or-marked
                                               #'su--nadvice-make-directory-auto-root)
                                (advice-remove 'find-file-noselect
                                               #'su--nadvice-find-file-noselect)
                                (advice-remove 'semantic-find-file-noselect
                                               #'su--nadvice-supress-find-file-hook)
                                (advice-remove 'find-file-noselect-1
                                               #'su--nadvice-find-file-noselect-1)))

  (su-mode +1)

  :config
  (eval-when-compile
    (with-demoted-errors "Load error: %s"
      (require 'el-patch)))


  (defun nadvice/su-disable-maybe-setup (flag)
    (if (and (not flag) (bound-and-true-p su-auto-save-mode))
        (su-auto-save-mode -1)))

  (el-patch-feature su)
  (el-patch-define-minor-mode su-auto-save-mode
                              "Automatically save buffer as root"
                              :lighter su-auto-save-mode-lighter
                              (if su-auto-save-mode
                                  ;; Ensure that su-auto-save-mode is visible by moving it to the
                                  ;; beginning of the minor mode list
                                  (progn
                                    (el-patch-add
                                     (advice-add 'set-buffer-modified-p :before
                                                 #'nadvice/su-disable-maybe-setup))
                                    (let ((su-auto-save-mode-alist-entry
                                           (assoc 'su-auto-save-mode minor-mode-alist)))
                                      (setq minor-mode-alist
                                            (delete su-auto-save-mode-alist-entry minor-mode-alist))
                                      (push su-auto-save-mode-alist-entry minor-mode-alist))
                                    (add-hook 'before-save-hook #'su--before-save-hook nil t))

                                (el-patch-add
                                 (advice-remove 'set-buffer-modified-p
                                                #'nadvice/su-disable-maybe-setup))
                                (remove-hook 'before-save-hook #'su--before-save-hook t)))
  )
(use-package! fish-completion
  :after bash-completion
  :config
  (setq fish-completion-fallback-on-bash-p t))
(use-package! bash-completion
  :after eshell)
(use-package! eshell-git-prompt
  :after eshell
  :config
  (eshell-git-prompt-use-theme 'powerline))

(add-hook! eshell-mode
  (setenv "TERM" "xterm-256color")
  (map! :map eshell-mode-map :ni "C-r" #'eshell/ivy-history)
  (map! :map eshell-command-map :ni "C-r" #'eshell/ivy-history)
  (map! :map eshell-mode-map :i "C-d" #'eshell-send-eof-to-process)
  )

(add-hook! eshell-mode '(su-mode with-editor-export-git-editor with-editor-export-editor global-fish-completion-mode))

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
        (magit-log (list branch) '()
                   (mapcar (lambda (f) (concat (eshell/pwd) "/" f))
                           file-list))
      (magit-log-head)))
  (eshell/echo))

(defun algernon/git-pr (pr branch)
  (eshell-do-eval
   (eshell-parse-command (format "git fetch origin pull/%s/head:%s" pr branch)) t))
