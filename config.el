;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Cassandra Comar"
      user-mail-address "cass@ndra.io")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Iosevka Custom" :size 32))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-molokai)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/todo/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;; ENVIRONMENT
(setenv "SSH_AUTH_SOCK" (shell-command-to-string "gpgconf --list-dirs agent-ssh-socket"))
(setenv "GPG_AGENT_INFO" (shell-command-to-string "gpgconf --list-dirs agent-socket"))
(advice-add 'risky-local-variable-p :override #'ignore)
(setq enable-local-variables :all)
(setq! with-editor-emacsclient-executable "/home/cassandra/.nix-profile/bin/emacsclient")

; set up env vars from encrypted sources
(defun pinentry-emacs (desc prompt ok error)
  (let ((str (read-passwd (concat (replace-regexp-in-string "%22" "\"" (replace-regexp-in-string "%0A" "\n" desc)) prompt ": "))))
    str))
(setq auth-sources '("/home/cassandra/.authinfo.gpg"))
(setq epa-file-encrypt-to '("cass@ndra.io"))
(setq epa-file-select-keys t)
(setq file-name-handler-alist (cons epa-file-handler file-name-handler-alist))
(defun hack-pinentry-startup ()
  (let ((tty (shell-command-to-string "tty")))
                (setenv "DISPLAY" ":0")
                (setenv "GPG_TTY" tty)
                (setenv "SSH_AUTH_SOCK" "/run/user/1000/gnupg/S.gpg-agent.ssh")
                (load-library "/home/cassandra/.secrets.el.gpg")))
(if (daemonp)
    (add-hook 'after-make-frame-functions
          (lambda (frame)
            (interactive)
            (with-selected-frame frame
              (hack-pinentry-startup))))
    (hack-pinentry-startup))

;; KEYBINDINGS
(setq doom-localleader-key ",")
(map! :leader "SPC" #'execute-extended-command)
(map! :leader "p t" #'+treemacs/toggle)
(map! :leader "p x" #'lsp-ui-flycheck-list)
(map! :leader "DEL" #'projectile-find-file)
(map! :nv "TAB" #'company-indent-or-complete-common
      :nv [tab] #'company-indent-or-complete-common)
(map! :after global-company-mode
      company-active-map
      [return]   #'company-complete
      "<return>" #'company-complete
      "RET"      #'company-complete)
(map! :after global-company-mode
      company-search-map
      [return]   #'company-complete
      "<return>" #'company-complete
      "RET"      #'company-complete)
                                        ;(map! :leader "g s" #'magit)
(map! :leader "b b" #'switch-to-buffer)
(map! :leader "o u" #'undo-tree-visualize)
(map! :leader
      "0" #'winum-select-window-0-or-10
      "1" #'winum-select-window-1
      "2" #'winum-select-window-2
      "3" #'winum-select-window-3
      "4" #'winum-select-window-4
      "5" #'winum-select-window-5
      "6" #'winum-select-window-6
      "7" #'winum-select-window-7
      "8" #'winum-select-window-8
      "9" #'winum-select-window-9
      "0" #'treemacs-select-window)
(map! :nv "J" #'+lookup/definition)
(map! :nv "C-J" #'lsp-ui-peek-jump-backward)
(map! :nv "C-K" #'+lookup/references)
(map! :nv "H" #'treemacs-select-window)
(map! :nv "L" #'lsp-ui-imenu)
(map! :nv "g b" #'lsp-ui-peek-jump-backward)
(map! :nv "g B" #'lsp-ui-peek-jump-forward)
(map! :n
      "V" #'evil-visual-line)

;; TRANSIENT STATES
(use-package! hercules)

(general-def
  :prefix-map 'custom-paste-map
  "C-j" #'evil-paste-pop-next
  "C-k" #'evil-paste-pop)

(hercules-def
 :show-funs '(evil-paste-after evil-paste-before)
 :keymap 'custom-paste-map
 :transient t)

;; PACKAGE CONFIG
(use-package! company-prescient
  :after company
  :hook (company-mode . company-prescient-mode))

(use-package! direnv
  :config
  (direnv-mode))

(after! evil-snipe
  (setq! evil-snipe-scope 'visible)
  (setq! evil-snipe-repeat-scope 'whole-visible)
  (setq! evil-snipe-spillover-scope 'whole-visible))

;; configure evil
                                        ; make evil-search-word look for symbol rather than word boundaries
(defalias #'forward-evil-word #'forward-evil-symbol)
(setq! evil-symbol-word-search t)

                                        ; don't substitute globally by default
(setq! evil-ex-substitute-global nil)

                                        ; set up smartparens
(after! smartparens
  (show-smartparens-global-mode +1))

;; magit
(after! magit
  (setq with-editor-emacsclient-executable "/home/cassandra/.nix-profile/bin/emacsclient"))

(use-package! magit-delta
  :defer
  :after-call magit-status
  :preface
  ;; (setq
  ;;   magit-delta-default-dark-theme "OneHalfDark"
  ;;   magit-delta-default-light-theme "OneHalfLight")
  :config
  (magit-delta-mode)
  (setq magit-save-repository-buffers t))

;; set up LSP
(use-package! lsp
  :defer t
  :init
  (setq lsp-rust-analyzer-server-display-inlay-hints t)
  (setq lsp-rust-analyzer-display-parameter-hints t)
  (setq lsp-rust-analyzer-display-chaining-hints t)
  (setq lsp-rust-analyzer-proc-macro-enable t)
  (setq lsp-rust-analyzer-cargo-load-out-dirs-from-check t)
  (setq lsp-rust-unstable-features t)
  (setq lsp-prefer-capf t)
  (setq lsp-auto-execute-action t)
  (setq lsp-before-save-edits t)
  (setq lsp-enable-snippet t)
  (setq lsp-rust-analyzer-cargo-watch-enable t)
  (setq lsp-rust-analyzer-cargo-all-targets t)
  (setq lsp-rust-analyzer-cargo-watch-command "clippy")
  (setq lsp-rust-analyzer-cargo-watch-args "--tests")
  ;; (setq lsp-rust-features ["k8s_integration"])
  :config
  (puthash "kubernetes" "*.yaml" lsp-yaml-schemas)
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0)
  )
(after! rustic
  (setq rustic-format-on-save t)
  (after! dap-mode
    (require 'dap-gdb-lldb)
    (dap-register-debug-template "Rust::GDB Run Configuration"
                                 (list :type "gdb"
                                       :request "launch"
                                       :name "GDB::Run"
                                       :gdbpath "rust-gdb"
                                       :target nil
                                       :cwd nil))
    )
  )


                                        ; show lsp sideline on hover ... doom docs mark this as noisy so disable if it causes a problem
(after! lsp-ui
  (setq lsp-ui-sideline-show-hover t))

(after! lsp
  (add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("rnix-lsp"))
                    :major-modes '(nix-mode)
                    :server-id 'nix))
  )
(add-hook! nix-mode-hook #'lsp)

(use-package! lsp-treemacs
  :after lsp)
(after! lsp-treemacs
  (lsp-treemacs-sync-mode 1))

                                        ; sh stuff
(add-hook! sh-mode #'lsp)

                                        ; rust stuff
(add-hook! 'lsp-on-idle-hook #'lsp-rust-analyzer-inlay-hints-change-handler)

                                        ; haskell stuff
(add-hook! haskell-mode lsp)
(after! lsp-haskell
  (set-ligatures! 'haskell-mode
    :lambda "\\"
    :composition "."))

                                        ; groovy stuff
(after! groovy-mode
  (add-to-list 'auto-mode-alist '("Jenkinsfile$" . groovy-mode)))

                                        ; yaml-mode
(after! yaml-mode
  (add-to-list 'auto-mode-alist '("\\.yaml\\.j2\\'" . yaml-mode)))

                                        ; org-mode
                                        ;(use-package! org-babel :defer false)

(after! evil-org
  (setq org-want-todo-bindings t)
  (setq org-log-done t)
  (setq org-todo-keywords '((sequence "TODO(t)" "WORKING(w!)" "BLOCKED(b@/!)" "STALLED(s!)" "|" "DONE(d!)" "DEFERRED(f!)" "CANCELED(c)")))
  (evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading))
  )
(add-hook! org-mode visual-line-mode)

(map! :after evil-org
      :map evil-org-mode-map
      :nv
      [remap evil-org-org-insert-heading-below] #'+org/insert-item-above
      [remap evil-org-open-below] #'+org/insert-item-below
      )

(setq! org-roam-directory "~/todo/")
(after! org-journal
  (setq org-journal-date-prefix "+TITLE: ")
  (setq org-journal-file-format "%Y-%m-%d.org")
  (setq org-journal-date-format "%A, %d %B %Y"))


; elasticsearch
(use-package! es-mode
  :after org
  :mode ("\\.es\\'" . #'es-mode)
  :config
  ;; (add-to-list 'org-babel-load-languages '(elasticsearch . t))
  (add-to-list '+org-babel-mode-alist '(es . elasticsearch)))

; systemd
(use-package! systemd
  :defer t
  :init (setq systemd-use-company-p t))

(load! "+eshell")

;; FUNCTIONS
(defun ex-save-kill-buffer-and-close ()
  (interactive)
  (save-buffer)
  (kill-this-buffer)
  )
(evil-ex-define-cmd "q" 'kill-this-buffer)
(evil-ex-define-cmd "wq" 'ex-save-kill-buffer-and-close )
(evil-ex-define-cmd "x" 'ex-save-kill-buffer-and-close )

;; ensure aliases get saved
(advice-remove #'eshell-write-aliases-list #'ignore)
                                        ;(advice-remove #'eshell/cat #'aweshell-cat-with-syntax-highlight)
