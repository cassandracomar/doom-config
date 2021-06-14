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
(add-hook! eshell-post-command-hook #'direnv-update-directory-environment)

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

  (require 'em-smart)
  (setq eshell-where-to-jump 'begin)
  (setq eshell-review-quick-commands nil)
  (setq eshell-smart-space-goes-to-end t)
  (setq eshell-scroll-to-bottom-on-output nil)
  (setq eshell-error-if-no-glob nil)
  (add-hook 'eshell-mode-hook #'eshell-smart-initialize)
  (add-hook 'eshell-pre-command-hook #'eshell-append-history)
  (add-hook 'eshell-post-command-hook #'eshell-update-history)
  (remove-hook 'eshell-mode-hook #'hide-mode-line-mode)
  (setq vterm-kill-buffer-on-exit t)
  ;; remove confirmation for process buffers
  (setq kill-buffer-query-functions
    (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

  (setq eshell-exit-hook nil)
  (setq eshell-destroy-buffer-when-process-dies t)

  (setq company-minimum-prefix-length 0
        company-idle-delay 0.0)
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

(use-package! aweshell)

(defun eshell/git (command &rest args)
  (pcase command
    ("pr" (apply #'algernon/git-pr args))
    ("log" (apply #'algernon/git-log args))
    ("status" (progn
                (magit-status)
                (eshell/echo)))
    ("grep" (apply #'algernon/git-grep args))
    (_ (let ((command (s-join " " (append (list "*git" command) args))))
         (message command)
         (eshell-command-result command)))))

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
(use-package! awscli-capf
  :after eshell)

(add-hook! eshell-mode
  (setenv "TERM" "xterm-256color")
  (map! :map eshell-mode-map :ni "C-r" #'eshell/ivy-history)
  (map! :map eshell-command-map :ni "C-r" #'eshell/ivy-history)
  (map! :map eshell-mode-map :i "C-d" #'eshell-send-eof-to-process)
  (map! :map eshell-mode-map :nv
        "$" #'evil-end-of-line)
  )

(add-hook! eshell-mode '(with-editor-export-git-editor with-editor-export-editor global-fish-completion-mode solaire-mode awscli-capf-add))

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

(defun eshell-find-single-file (file-name)
  (let ((file-writeable? (file-writable-p file-name)))
    (if file-writeable?
        (find-file file-name)
      (doom/sudo-find-file file-name))))

(defun eshell/find-file (&rest args)
  "Open file even if it is not owned by you via sudo. Only adds sudo if needed."
  (mapcar (lambda (f) (eshell-find-single-file f)) args))

(defun eshell/vim (&rest args)
  (eshell/find-file args))
