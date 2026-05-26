;;; ~/.doom.d/+eshell.el -*- lexical-binding: t; -*-

(setq! eshell-history-size 9999999)

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
    (let ((old-ring (ring-copy eshell-history-ring)))
      (setq eshell-history-ring (list (car eshell-history-ring)))
                                        ; write
      (setq eshell-history-ring old-ring))))

(map! :leader "o E" #'+eshell/new)
(defun +eshell/new ()
  (interactive)
  (eshell t))

(add-hook! eshell-pre-command-hook #'eshell-append-history)
(add-hook! eshell-post-command-hook #'eshell-update-history)
(add-hook! eshell-post-command-hook #'envrc--update)

(after! eshell
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
    "Return a potentially trimmed-down version of the directory PATH, replacing
     parent directories with their initial characters to try to get the character
     length of PATH (sans directory slashes) down to MAX-LEN."
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
             (cap (set-buffer temp))
             (a (erase-buffer)))
      (aio-await (apply #'aio-call-process name (current-buffer) cmd))
      (let ((r (buffer-string))
            (cap (set-buffer curr)))
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
  (advice-add 'eshell/async-command-to-string :around #'envrc-propagate-environment)

  (defun +eshell-bypass-pcomplete-cmd-a (orig-fn command)
    "Bypass global `pcomplete/<cmd>' shims in eshell with fish-completion-mode on,
but allow eshell-specific `pcomplete/eshell-mode/<cmd>' overrides."
    (if (and (eq major-mode 'eshell-mode) fish-completion-mode)
        (let ((sym (intern-soft (concat "pcomplete/eshell-mode/" command))))
          (and sym (fboundp sym) sym))
      (funcall orig-fn command)))
  (advice-add 'pcomplete-find-completion-function :around #'+eshell-bypass-pcomplete-cmd-a)

  (defvar +nix--cli-spec-cache nil
    "Cached parse of `nix __dump-cli' JSON, keyed by `nix' executable path.")

  (defun +nix--cli-spec ()
    "Return parsed `nix __dump-cli' as a hash-table tree, lazily cached."
    (let ((path (executable-find "nix")))
      (unless (equal path (car +nix--cli-spec-cache))
        (setq +nix--cli-spec-cache
              (cons path
                    (with-temp-buffer
                      (when (zerop (call-process "nix" nil t nil "__dump-cli"))
                        (goto-char (point-min))
                        (ignore-errors
                          (json-parse-buffer :object-type 'hash-table)))))))
      (cdr +nix--cli-spec-cache)))

  (defun +nix--subcommand-description (subcommand-path candidate)
    "Look up CANDIDATE's description in nix's CLI spec under SUBCOMMAND-PATH.
SUBCOMMAND-PATH is the list of resolved subcommands preceding CANDIDATE."
    (when-let* ((spec (+nix--cli-spec))
                (node (gethash "args" spec)))
      (catch 'done
        (dolist (cmd subcommand-path)
          (let ((cmds (gethash "commands" node)))
            (unless (and cmds (gethash cmd cmds))
              (throw 'done nil))
            (setq node (gethash cmd cmds))))
        (let ((cmds (gethash "commands" node)))
          (when (and cmds (gethash candidate cmds))
            (gethash "description" (gethash candidate cmds)))))))

  (defun pcomplete/eshell-mode/nix ()
    "Complete `nix' via NIX_GET_COMPLETIONS, honoring the `filenames' hint
so flake refs (e.g. `nixpkgs#hello') don't get a trailing space.
Descriptions are attached as `pcomplete-annotation' for corfu/etc., falling
back to `nix __dump-cli' for subcommands that nix doesn't describe inline.

To allow corfu to filter as the user types or backspaces within a path
segment, we ask nix for the FULL sibling set at the parent level (by
truncating the current arg at the last `#'/`.'/`:'/`?' before sending) and
report the partial as the pcomplete stub.  Backspacing past that boundary
exits completion-in-region and triggers a fresh CAPF call for the next
parent level.  The full installable is preserved on each candidate as the
`nix-full-ref' text property so `+pcomplete-doc-buffer' can fetch
`.meta.description' lazily."
    (let* ((args pcomplete-args)
           (n (1- (length args)))
           (current-arg (or (car (last args)) ""))
           (stub-start (let ((p (string-match "[#.:?][^#.:?]*\\'" current-arg)))
                         (if p (1+ p) 0)))
           (stub-prefix (substring current-arg 0 stub-start))
           (stub (substring current-arg stub-start))
           ;; Send the parent path (with partial stripped) so nix returns the
           ;; full sibling set, letting corfu filter dynamically.
           (query-args (append (butlast (cdr args)) (list stub-prefix)))
           (process-environment
            (cons (format "NIX_GET_COMPLETIONS=%d" n) process-environment))
           (output (with-output-to-string
                     (apply #'call-process "nix" nil (list standard-output nil) nil
                            query-args)))
           (lines (split-string output "\n" t))
           (header (car lines))
           (subcommand-path (cl-subseq args 1 n))
           (completions
            (mapcar (lambda (line)
                      (let* ((parts (split-string line "\t"))
                             (full-name (car parts))
                             (short-name (if (and (not (string-empty-p stub-prefix))
                                                  (string-prefix-p stub-prefix full-name))
                                             (substring full-name (length stub-prefix))
                                           full-name))
                             (desc (or (and (cadr parts)
                                            (not (string-empty-p (cadr parts)))
                                            (cadr parts))
                                       (+nix--subcommand-description
                                        subcommand-path short-name))))
                        (apply #'propertize short-name
                               'nix-full-ref full-name
                               (when desc
                                 (list 'pcomplete-annotation (concat " " desc)
                                       'pcomplete-help desc)))))
                    (cdr lines))))
      (when (member header '("filenames" "attrs"))
        ;; `pcomplete-exit-function' is let-bound by
        ;; `pcomplete-completions-at-point'; override it locally so we don't
        ;; mutate the global `pcomplete-termination-string'.
        (setq pcomplete-exit-function (lambda (&rest _) nil)))
      (dolist (_ (cddr args))
        (pcomplete-here))
      (pcomplete-here completions stub t)))

  (defvar +pcomplete-doc-cache (make-hash-table :test 'equal)
    "Session-local cache of completion documentation by candidate string.")

  (defun +nix--fetch-meta-description (ref)
    "Fetch `<REF>.meta.description' via `nix eval --raw'.
Returns nil on failure or empty result.  Cached in `+pcomplete-doc-cache'."
    (let ((cached (gethash ref +pcomplete-doc-cache 'miss)))
      (if (not (eq cached 'miss))
          cached
        (let ((desc (with-temp-buffer
                      (when (zerop (call-process
                                    "nix" nil (list (current-buffer) nil) nil
                                    "eval" "--raw"
                                    (format "%s.meta.description" ref)))
                        (let ((s (string-trim (buffer-string))))
                          (and (not (string-empty-p s)) s))))))
          (puthash ref desc +pcomplete-doc-cache)
          desc))))

  (defvar +pcomplete-doc--last-buffer nil
    "Most recent buffer returned by `+pcomplete-doc-buffer', killed on next call.")

  (defun +pcomplete-doc-buffer (cand)
    "Return a doc buffer for CAND, used as `:company-doc-buffer' for pcomplete.
Checks `pcomplete-help' text property first; for `flake#attr'-style candidates
without an inline description, lazily fetches `.meta.description' via nix.

Uses a freshly-generated buffer per call (with `inhibit-buffer-hooks') so that
`erase-buffer'/`insert' side-effects don't leak into corfu's render pipeline.
The previous buffer is killed when a new one is created to avoid leaks."
    (when (stringp cand)
      (when-let* ((full-ref (or (get-text-property 0 'nix-full-ref cand) cand))
                  (doc (or (get-text-property 0 'pcomplete-help cand)
                           (and (string-match-p "#" full-ref)
                                (+nix--fetch-meta-description full-ref)))))
        (when (buffer-live-p +pcomplete-doc--last-buffer)
          (kill-buffer +pcomplete-doc--last-buffer))
        (let ((buf (generate-new-buffer " *pcomplete-doc*" t)))
          (with-current-buffer buf
            (insert doc))
          (setq +pcomplete-doc--last-buffer buf)
          buf))))

  (defun +pcomplete-add-doc-buffer-a (result)
    "Append `:company-doc-buffer' to `pcomplete-completions-at-point' result
in eshell-mode with fish-completion-mode active, so corfu-popupinfo can
lazily fetch per-candidate descriptions."
    (when (and result (listp result)
               (eq major-mode 'eshell-mode)
               (bound-and-true-p fish-completion-mode))
      (setq result (append result (list :company-doc-buffer
                                        #'+pcomplete-doc-buffer))))
    result)
  (advice-add 'pcomplete-completions-at-point :filter-return
              #'+pcomplete-add-doc-buffer-a))

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

(use-package! fish-completion
  :commands global-fish-completion-mode fish-completion-mode
  :defer t
  :config
  (setq fish-completion-fallback-on-bash-p nil)

  (defun +fish-completion--list-completions-a (raw-prompt)
    "Like `fish-completion--list-completions' but preserve fish's per-completion
descriptions as `pcomplete-annotation' / `pcomplete-help' text properties so
corfu and friends can display them."
    (let ((candidates (fish-completion--list-completions-with-desc raw-prompt)))
      (when candidates
        (mapcar (lambda (e)
                  (let* ((parts (split-string e "\t"))
                         (name (car parts))
                         (desc (cadr parts)))
                    (if (and desc (not (string-empty-p desc)))
                        (propertize name
                                    'pcomplete-annotation (concat " " desc)
                                    'pcomplete-help desc)
                      name)))
                (split-string candidates "\n" t)))))
  (advice-add 'fish-completion--list-completions :override
              #'+fish-completion--list-completions-a))
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

(defun eshell-find-single-file (file-name)
  (let ((file-writeable? (file-writable-p file-name)))
    (if file-writeable?
        (find-file file-name)
      (doom/sudo-find-file file-name))))

(defun eshell/find-file (&rest args)
  "Open file even if it is not owned by you via sudo. Only adds sudo if needed."
  (mapcar #'eshell-find-single-file args))

(defun eshell/vim (&rest args)
  (mapcar #'eshell-find-single-file args))

