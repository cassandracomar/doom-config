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
(setq doom-font "Iosevka Nerd Font:size=20:weight=light")
(setq doom-symbol-font "Iosevka Nerd Font")
(setq nerd-icons-font-family "Iosevka Nerd Font")
(setq doom-emoji-fallback-font-families
      '("Noto Color Emoji"
        "Apple Color Emoji"
        "Noto Emoji"
        "Symbola"))
(setq doom-symbol-fallback-font-families
      '("Noto Sans Symbols"
        "Noto Sans Symbols 2"
        "Symbola"
        "Apple Color Emoji"))
;; (setq nerd-icons-font-names '("IosevkaNerdFont-Regular.ttf" "SymbolsNerdFont-Regular.ttf"))
(add-to-list 'default-frame-alist `(font . ,doom-font))
(add-to-list 'default-frame-alist '(undecorated . t))

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
(menu-bar-mode -1)


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
;; (general-auto-unbind-keys :off)
;; (remove-hook 'doom-after-init-modules-hook #'general-auto-unbind-keys)

(add-to-list 'trusted-content "~/src/github.com/cassandracomar/doom-config/")
(setq doom-user-dir "~/src/github.com/cassandracomar/doom-config/")

(add-hook 'emacs-startup-hook
          (lambda ()
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state)
                                  (garbage-collect-maybe (round (/ 1 gc-cons-percentage))))))
              (add-hook 'after-focus-change-function
                        (lambda ()
                          (garbage-collect-maybe (round (/ 1 gc-cons-percentage))))))))
;; ENVIRONMENT
(setenv "SSH_AUTH_SOCK" (shell-command-to-string "gpgconf --list-dirs agent-ssh-socket"))
(setenv "GPG_AGENT_INFO" (shell-command-to-string "gpgconf --list-dirs agent-socket"))
(setenv "TERM" "xterm-256color")
(setenv "PAGER" "cat")
(setenv "DOOMPAGER" "cat")

(advice-add 'risky-local-variable-p :override #'ignore)
(setq enable-local-variables :all)
(setq enable-local-eval t)
;; (setq! with-editor-emacsclient-executable "/opt/homebrew/bin/emacsclient")
(setq! forge-database-connector 'sqlite-builtin)

                                        ; set up env vars from encrypted sources
(defun pinentry-emacs (desc prompt ok error)
  (let ((str (read-passwd (concat (replace-regexp-in-string "%22" "\"" (replace-regexp-in-string "%0A" "\n" desc)) prompt ": "))))
    str))
(setq auth-sources (list (format "%s/.authinfo.gpg" (getenv "HOME"))))
(setq epa-file-encrypt-to '("cass@ndra.io"))
(setq epa-file-select-keys nil)
(setq file-name-handler-alist (cons epa-file-handler file-name-handler-alist))
;; (after! grip-mode
;;   (let ((credential (auth-source-user-and-password "git.drwholdings.com")))
;;     (setq grip-github-api-url "https://git.drwholdings.com/api/v3"
;;           grip-github-user (car credential)
;;           grip-github-password (cadr credential)
;;           grip-update-after-change nil)))
(unless (display-graphic-p)
  (require 'evil-terminal-cursor-changer)
  (setq etcc-term-type-override 'xterm)
  (evil-terminal-cursor-changer-activate))

;; utils
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
  (interactive)
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

;; UI
(use-package! doom-modeline
  :config
  (setq doom-modeline-lsp nil
        doom-modeline-hud t))

(use-package! spacious-padding
  :demand t
  :hook (after-init . spacious-padding-mode)
  :init
  (setq spacious-padding-widths
        '( :internal-border-width 5
           :header-line-width 4
           :left-fringe-width 0
           :mode-line-width 0
           :tab-width 0
           :right-divider-width 5
           :scroll-bar-width 2)))

;; (use-package! eglot
;;   :config
;;   (remove-hook! '(eglot-managed-mode-hook doom-modeline-mode-hook) #'doom-modeline-override-eglot)
;;   (add-hook! 'eglot-managed-mode-hook
;;     (add-to-list
;;      'mode-line-misc-info
;;      `(eglot--managed-mode
;;        (" ["
;;         (:eval
;;          (cl-loop for e in eglot-mode-line-format
;;                   for render = (format-mode-line e)
;;                   unless (eq render "")
;;                   collect (cons render
;;                                 (eq e 'eglot-mode-line-menu))
;;                   into rendered
;;                   finally
;;                   (return (cl-loop for (rspec . rest) on rendered
;;                                    for (r . titlep) = rspec
;;                                    concat r
;;                                    when rest concat (if titlep ":" "/")))))
;;         "] ")))))

;; KEYBINDINGS
(setq doom-localleader-key ",")
(map! :leader "SPC" #'execute-extended-command)
(map! :leader "S-SPC" #'execute-extended-command-for-buffer)
(map! :leader "p t" #'+treemacs/toggle)
(map! :leader "p x" #'lsp-ui-flycheck-list)
(map! :leader "o o" #'envrc-reload)
(map! :leader "p p" #'consult-projectile-switch-project)
(map! :leader "p f" #'consult-projectile)
;; (map! :nv "TAB" #'company-indent-or-complete-common
;;       :nv [tab] #'company-indent-or-complete-common)
(map! :map corfu-map
      :i "C-s" #'+corfu/move-to-minibuffer
      :i
      "<return>" #'corfu-complete
      "RET" #'corfu-complete
      [return] #'corfu-complete
      [backspace] #'evil-delete-backward-char-and-join
      "DEL" #'evil-delete-backward-char-and-join)
(map! :gnvi
      "<tab>" #'indent-for-tab-command
      "TAB" #'indent-for-tab-command)

(map! :mode eshell-mode
      :i
      "<tab>" #'completion-at-point
      "TAB" #'completion-at-point)
(map! :mode eat-mode
      :gni
      "<up>" #'eat-line-previous-input
      "<down>" #'eat-line-next-input
      "C-k" #'eat-previous-shell-prompt
      "C-j" #'eat-next-shell-prompt
      "C-r" #'consult-history
      "<tab>" #'completion-at-point
      "TAB" #'completion-at-point)
(map! :map vertico-map
      "<tab>" #'vertico-insert
      [backtab] #'vertico-previous)
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
(map! :nv "C-J" #'xref-go-back)
(map! :nv "C-K" #'+lookup/references)
(map! :nv "H" #'treemacs-select-window)
(map! :nv "L" #'lsp-ui-imenu)
(map! :nv "g b" #'xref-go-back)
(map! :nv "g B" #'xref-go-forward)
(map! :n
      "V" #'evil-visual-line)
(map! :nv "g l" (lambda (l) (apply #'evil-goto-line l)))

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

(use-package! corfu
  :init
  (setq
   +corfu-want-minibuffer-completion 'aggressive
   +corfu-want-tab-prefer-expand-snippets nil
   +corfu-want-tab-prefer-navigating-snippets nil
   shell-file-name-chars "[]~/A-Za-z0-9+@:_.$#%,={}- "
   shell-file-name-quote-list '(?$ ?\* ?\! ?\" ?\'))

  :config
  (setq! corfu-auto-prefix 0
         corfu-auto-delay 0.0
         corfu-preselect 'directory
         global-corfu-modes t
         corfu-auto t
         corfu-count 16
         corfu-max-width 60)
  :custom
  corfu-auto t)
;; (use-package corfu-pixel-perfect
;;   :after (corfu)
;;   :hook (global-corfu-mode . corfu-pixel-perfect-mode)
;;   :custom
;;   (corfu-pixel-perfect-ignore-annotation-modes '(haskell-mode)))
(use-package! consult-hoogle
  :ensure haskell-mode
  :after haskell-mode
  :config
  (map! "C-S-K" #'consult-hoogle-project))

(use-package! orderless
  :config
  (setq +vertico-company-completion-styles '(orderless))
  (setq completion-styles '(orderless basic)
        orderless-matching-styles '(orderless-flex orderless-literal-prefix orderless-regexp)))

(after! evil-snipe
  (setq! evil-snipe-scope 'visible)
  (setq! evil-snipe-repeat-scope 'whole-visible)
  (setq! evil-snipe-spillover-scope 'whole-visible))

(use-package! marginalia
  :init
  (setq marginalia-command-categories
        '((projectile-find-file . projectile-file)
          (projectile-find-dir . projectile-file)
          (projectile-switch-project . projectile-file)
          (execute-extended-command . command))))

(use-package! consult-projectile
  :defer nil)

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
;; (after! magit
;;   (setq with-editor-emacsclient-executable "/opt/homebrew/bin/emacsclient"))

;; (use-package! magit-delta
;;   :disabled t
;;   :hook (magit-mode . magit-delta-mode)
;;   :config
;;   (setq magit-delta-delta-args '("--max-line-distance" "0.6" "--color-only"))
;;   (setq magit-save-repository-buffers t)
;;   (setq magit-process-finish-apply-ansi-colors t))
;; (use-package! diff-hl
;;   :config
;;   (remove-hook! 'magit-pre-refresh-hook  #'diff-hl-magit-pre-refresh)
;;   (remove-hook! 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
;;   )

(use-package! forge
  :config
  (push '("git.drwholdings.com"               ; GITHOST
          "git.drwholdings.com/api/v3"           ; APIHOST
          "git.drwholdings.com"               ; WEBHOST and INSTANCE-ID
          forge-github-repository)    ; CLASS
        forge-alist))
;; set up LSP
(use-package! lsp
  ;; :defer t
  :demand t
  :init
  ;; (defun lsp-booster--advice-json-parse (old-fn &rest args)
  ;;   "Try to parse bytecode instead of json."
  ;;   (or
  ;;    (when (equal (following-char) ?#)
  ;;      (let ((bytecode (read (current-buffer))))
  ;;        (when (byte-code-function-p bytecode)
  ;;          (funcall bytecode))))
  ;;    (apply old-fn args)))
  ;; (advice-add (if (progn (require 'json)
  ;;                        (fboundp 'json-parse-buffer))
  ;;                 'json-parse-buffer
  ;;               'json-read)
  ;;             :around
  ;;             #'lsp-booster--advice-json-parse)

  ;; (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  ;;   "Prepend emacs-lsp-booster command to lsp CMD."
  ;;   (let ((orig-result (funcall old-fn cmd test?)))
  ;;     (if (and (not test?)                             ;; for check lsp-server-present?
  ;;              (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
  ;;              lsp-use-plists
  ;;              (not (functionp 'json-rpc-connection))  ;; native json-rpc
  ;;              (executable-find "emacs-lsp-booster"))
  ;;         (progn
  ;;           (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
  ;;             (setcar orig-result command-from-exec-path))
  ;;           (message "Using emacs-lsp-booster for %s!" orig-result)
  ;;           (cons "emacs-lsp-booster" orig-result))
  ;;       orig-result)))
  ;; (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

  (defun minad/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)) ;; Configure orderless
    (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point))))

  (setq lsp-auto-guess-root t)
  (setq lsp-rust-analyzer-server-format-inlay-hints t)
  (setq lsp-inlay-hint-enable t)
  (setq lsp-rust-analyzer-display-parameter-hints t)
  (setq lsp-rust-analyzer-display-chaining-hints t)
  (setq lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (setq lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names t)
  ;; (setq lsp-rust-analyzer-display-closure-return-type-hints t)
  (setq lsp-rust-analyzer-display-reborrow-hints "always")
  (setq lsp-rust-analyzer-binding-mode-hints t)
  (setq lsp-rust-analyzer-lens-enable t)
  (setq lsp-rust-analyzer-lens-references-adt-enable t)
  (setq lsp-rust-analyzer-lens-implementations-enable t)
  (setq lsp-rust-analyzer-lens-references-trait-enable t)
  (setq lsp-rust-analyzer-lens-references-method-enable t)
  (setq lsp-rust-analyzer-lens-references-enum-variant-enable t)
  (setq lsp-rust-analyzer-proc-macro-enable t)
  (setq lsp-rust-analyzer-cargo-load-out-dirs-from-check t)
  (setq lsp-rust-unstable-features t)
  ;; (setq lsp-completion-provider :capf)
  (setq lsp-auto-execute-action t)
  (setq lsp-before-save-edits t)
  (setq lsp-enable-snippet t)
  (setq lsp-rust-analyzer-cargo-watch-enable t)
  ;; (setq lsp-rust-analyzer-cargo-all-targets t)
  (setq lsp-rust-analyzer-cargo-watch-command "clippy")
  (setq lsp-rust-analyzer-cargo-watch-args ["--tests"])
  ;; (setq lsp-rust-analyzer-cargo-unset-test ["core", "derivative"])
  (setq lsp-rust-analyzer-experimental-proc-attr-macros t)
  ;; (setq lsp-nix-nil-formatter ["nixpkgs-fmt"])

  (setq nix-command-path (executable-find "nix"))

  (defun pmx--project-flake-path ()
    (let ((flake-path (expand-file-name "flake.nix" (projectile-project-root))))
      (setq-local lsp-nix-nixd-nixpkgs-expr
                  (if (file-exists-p flake-path)
                      (format "import (builtins.getFlake \"%s\").inputs.nixpkgs { }" (file-name-directory flake-path))
                    "import <nixpkgs> { }"))

      (setq-local lsp-nix-nixd-server-arguments
                  `("--log=verbose" "--semantic-tokens=true" "--inlay-hints" ,(format "--nixpkgs-expr=%s" lsp-nix-nixd-nixpkgs-expr)))))

  (add-hook! 'nix-mode-hook #'pmx--project-flake-path)

  (setq lsp-nix-nixd-formatting-command [ "nix" "fmt" "--" ])
  (setq lsp-nix-nil-server-path nil)
  ;; (setq lsp-use-plists t)
  ;; (setq lsp-rust-analyzer-server-command '("emacs-lsp-booster" "rust-analyzer"))
  ;; (setq lsp-rust-analyzer-diagnostics-disabled ["unresolved-proc-macro"])
  ;; (setq lsp-rust-features ["k8s_integration"])
  :config
  ;; (setq company-minimum-prefix-length 1
  ;;       company-idle-delay 0.0)

  (lsp-defcustom lsp-nix-nil-flake-auto-archive t
    "auto archiving behavior for flake inputs that might use network"
    :type 'boolean
    :group 'lsp-nix-nil
    :lsp-path "nil.nix.flake.autoArchive"
    :package-version '(lsp-mode . "8.0.1"))
  (lsp-defcustom lsp-nix-nil-flake-auto-eval-inputs t
    " Whether to auto-eval flake inputs.
    The evaluation result is used to improve completion, but may cost
    lots of time and/or memory."
    :type 'boolean
    :group 'lsp-nix-nil
    :lsp-path "nil.nix.flake.autoEvalInputs"
    :package-version '(lsp-mode . "8.0.1"))
  (lsp-defcustom  lsp-nix-nil-nixpkgs-input-name "nixpkgs"
    " The input name of nixpkgs for NixOS options evaluation.
  The options hierarchy is used to improve completion, but may cost
  lots of time and/or memory.
  If this value is `null` or is not found in the workspace flake's
  inputs, NixOS options are not evaluated."
    :type 'string
    :group 'lsp-nix-nil
    :lsp-path "nil.nix.flake.nixpkgsInputName"
    :package-version '(lsp-mode . "8.0.1"))
  (setq lsp-enable-folding t
        lsp-enable-text-document-color t)
  (setq lsp-semantic-tokens-enable t)
  (add-hook! 'lsp-before-initialize-hook
    (require 'haskell-font-lock)
    (let ((faces '(("interface" . font-lock-preprocessor-face)
                   ("enum" . haskell-type-face)
                   ("enumMember" . haskell-definition-face)
                   ("operator" . font-lock-keyword-face)
                   ("macro" . font-lock-preprocessor-face)
                   ("typeParameter" . font-lock-variable-name-face)
                   ("property" . haskell-definition-face)
                   ("modifier" . font-lock-preprocessor-face)
                   ("decorator" . haskell-pragma-face))))
      (cl-loop for face in faces do
               (add-to-list 'lsp-semantic-token-faces face))))
  (set-lookup-handlers! lsp-mode :documentation #'lsp-ui-doc-toggle)
  (remove-hook! 'lsp-mode-hook #'lsp-completion-mode)
  (add-hook! 'lsp-mode-hook  (lsp-semantic-tokens-mode +1))
  (add-hook! #'lsp-completion-mode #'minad/lsp-mode-setup-completion)

  (with-eval-after-load 'terraform-mode
    (setf (alist-get 'terraform-mode lsp-language-id-configuration) "opentofu")))

(load! "+lsp-tofu")
(require 'lsp-tofu)
(add-to-list 'lsp-client-packages 'lsp-tofu)
(delq 'lsp-terraform lsp-client-packages)
(setf (alist-get 'terraform-mode lsp-language-id-configuration) "opentofu")

(use-package! terraform-mode
  :defer t
  :init
  (setq terraform-command "tofu"))

(use-package! lsp-ui
  :config
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-hover t)
  (setq lsp-ui-sideline-show-code-actions t))
(use-package! lsp-yaml
  ;; :config
  ;; (puthash "kubernetes" "*.yaml" lsp-yaml-schemas)
  )

;; (after! lsp-mode
;;   ;; https://github.com/emacs-lsp/lsp-mode/issues/3577#issuecomment-1709232622
;;   (delete 'lsp-terraform lsp-client-packages))
(use-package! rustic
  :defer t
  :init
  :config
  (setq rustic-format-on-save t)
  (after! dap-mode
    (require 'dap-gdb-lldb)
    (dap-register-debug-template "Rust::GDB Run Configuration"
                                 (list :type "gdb"
                                       :request "launch"
                                       :name "GDB::Run"
                                       :gdbpath "rust-gdb"
                                       :target nil
                                       :cwd nil))))

(use-package! lsp-nix
  :after (lsp-mode)
  :demand t
  :config
  (add-hook! #'nix-mode-hook (+format-with-lsp-mode -1)))

;; (use-package! eglot-booster
;;   :after eglot
;;   :config (eglot-booster-mode))

;; (after! eglot
;;   (load! "+eglot"))

(set-popup-rule! "^\\*helpful" :size 0.5 :quit t :select t :side 'right)
(set-popup-rule! "^\\*lsp-help\\*" :size 0.5 :quit t :select t :side 'right)
;; (use-package! eglot-semtok
;;   :config
;;   (add-hook! 'eglot-connect-hook #'eglot-semtok-on-connected))
;; (use-package! eglot-semantic-tokens
;;   :after eglot
;;   :config
;;   (setq eglot-enable-semantic-tokens t))
;; (use-package! eldoc-box
;;   :after eglot
;;   :config
;;   (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode 100 t))

(use-package! nix-mode
  :defer t
  :init
  (defun nix--prefix-bounds-override ()
    "Get bounds of Nix attribute path at point as a (BEG . END) pair, or nil."
    (save-excursion
      (when (<= (skip-chars-backward "a-zA-Z0-9'\\-_\\.") 0)
        (cons (point) (+ (point) (skip-chars-forward "a-zA-Z0-9'\\-_\\."))))))
  (advice-add #'nix--prefix-bounds :override #'nix--prefix-bounds-override)
  :config
  (setq company-idle-delay 0.0)
  (add-hook! before-save-hook #'nix-format-before-save)
  (map! :map nix-repl-mode-map
        :nvi
        "<tab>" #'completion-at-point
        "TAB" #'completion-at-point)
  (add-to-list 'markdown-code-lang-modes '("nix" . nix-mode))
  :custom
  (nix-nixfmt-bin "nix fmt -- -"))
(add-hook! nix-mode #'lsp)

(use-package! markdown-mode
  :config
  ;; (company-mode -1)
  (setq fill-column 120)
  (auto-fill-mode))
(add-hook! evil-markdown-mode
  (setq fill-column 120)
  (auto-fill-mode))

(set-formatter! 'nixpkgs-fmt '("nix" "fmt" "--" "-") :modes '(nix-mode))

(use-package! lsp-treemacs
  :after lsp)
(after! lsp-treemacs
  (lsp-treemacs-sync-mode 1))

;; sh stuff
(add-hook! sh-mode #'lsp)

                                        ; haskell stuff
(add-hook! haskell-mode #'lsp)
(plist-put! +ligatures-extra-symbols
            :type "⦂"
            :composition "∘"
            :dot ".")
(appendq! +ligatures-prog-mode-list '(">>=" ">>-" "=<<" "-<<" "<." "<.>" ".>" "\\/" "/\\" "==>" "<==" "/=" "==" "->" "<-" "=>" "<=" "||" "&&" "<|>" ">>" "<<" ">>>" "<<<" ".." "..." "<|" "|>" "<>"))
(ligature-set-ligatures 't +ligatures-prog-mode-list)

(use-package! lsp-haskell
  :init
  (setq lsp-haskell-session-loading "singleComponent")
  ;; (setq lsp-haskell-server-path "haskell-language-server-wrapper")
  ;; (setq lsp-haskell-server-wrapper-function (lambda (argv) (append (list "emacs-lsp-booster" "--") argv)))
  (setq lsp-haskell-formatting-provider "fourmolu")
  (setq lsp-haskell-plugin-fourmolu-config-external t)
  (setq lsp-rename-use-prepare nil)
  (setq lsp-haskell-plugin-semantic-tokens-global-on t)
  (setq lsp-haskell-plugin-rename-config-cross-module t)
  (setq lsp-haskell-max-completions 120)

  (lsp-defcustom lsp-haskell-plugin-ghcide-type-lenses-local-binding-inlay-hint-on t
    "Enables local binding inlay hints"
    :type 'boolean
    :group 'lsp-haskell-plugins
    :package-version '(lsp-mode . "9.0.0")
    :lsp-path "haskell.plugin.ghcide-type-lenses.config.localBindingInlayHintOn")
  (setq lsp-haskell-plugin-ghcide-type-lenses-local-binding-inlay-hint-on t)
  (setq lsp-haskell-plugin-ghcide-type-lenses-config-mode "always")

  :config
  (+format-with-lsp-mode))
(setq +ligatures-in-modes t)
;; (use-package! haskell-mode
;;   :mode ("\\.cabal\\'" . 'haskell-cabal-mode))
;; (use-package! haskell-ts-mode
;;   :custom
;;   (haskell-ts-font-lock-level 4)
;;   (haskell-ts-ghci "ghci")
;;   (haskell-ts-use-indent t))
;; (add-hook! haskell-ts-mode 'prettify-symbols-mode)

(after! haskell-mode
  (set-ligatures! 'haskell-mode
    :lambda "\\"
    :composition " . "
    :for "forall"
    :int "Int"
    :bool "Bool"
    :float "Float"
    :null "Nothing"
    :str "String"
    :true "True"
    :false "False"
    :not "not"
    :or "`or`"
    :and "`and`"
    :in "<-"
    :union "any"
    :intersect "all"
    :type "::")
  (+format-with-lsp-mode))

;;                                         groovy stuff
(after! groovy-mode
  (add-to-list 'auto-mode-alist '("Jenkinsfile$" . groovy-mode)))

;;                                         ; yaml-mode
(after! yaml-mode
  (add-to-list 'auto-mode-alist '("\\.yaml\\.j2\\'" . yaml-mode))
  (set-company-backend! 'yaml-mode 'company-capf)
  (set-company-backend! 'yaml-ts-mode 'company-capf)
  (add-to-list 'markdown-code-lang-modes '("yaml" . yaml-mode))
  (corfu-mode +1))
(add-hook! yaml-mode (corfu-mode +1))

;; projectile
(add-hook! projectile-after-switch-project-hook '(projectile-invalidate-cache nil))

(after! evil-org
  (setq org-want-todo-bindings t)
  (setq org-log-done t)
  (setq org-todo-keywords '((sequence "TODO(t)" "WORKING(w!)" "BLOCKED(b@/!)" "STALLED(s!)" "|" "DONE(d!)" "DEFERRED(f!)" "CANCELED(c)")))
  (evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading))
  )
;; (add-hook! org-mode visual-line-mode)

(map! :after evil-org
      :map evil-org-mode-map
      :nv
      [remap evil-org-org-insert-heading-below] #'+org/insert-item-above
      [remap evil-org-open-below] #'+org/insert-item-below
      )

;; (setq! org-roam-directory "~/todo/")
;; (after! org-journal
;;   (setq org-journal-date-prefix "+TITLE: ")
;;   (setq org-journal-file-format "%Y-%m-%d.org")
;;   (setq org-journal-date-format "%A, %d %B %Y"))


                                        ; elasticsearch
(use-package! es-mode
  :after org
  :mode ("\\.es\\'" . 'es-mode)
  :config
  ;; (add-to-list 'org-babel-load-languages '(elasticsearch . t))
  ;; (add-to-list '+org-babel-mode-alist '(es . elasticsearch))
  )

                                        ; systemd
(use-package! systemd
  :defer t
  :init (setq systemd-use-company-p t))

                                        ; jsonnett
;; (use-package! jsonnet-mode
;;   :defer t
;;   :mode "(\\.libsonnet|\\.jsonnet)")
;; (use-package! jsonnet-language-server
;;   :after jsonnet-mode
;;   :init (setq lsp-jsonnet-executable "jsonnet-language-server --tanka --lint"))
(use-package! jq-mode
  :mode "\\.jq$")

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
;; (advice-remove #'eshell-write-aliases-list #'ignore)


(defun eshell/gh-query-repos (hostname owner)
  (let*
      ((query-string
        (mapconcat
         (lambda (s) (s-format s 'aget `(("owner" . ,owner) ("hostname" . ,hostname))))
         '(""
           "    query {"
           "      search("
           "        type:REPOSITORY,"
           "        query: \"\"\""
           "          ${owner} in:owner"
           "          archived:false"
           "        \"\"\","
           "        last: 100"
           "      ) {"
           "        repos: edges {"
           "          repo: node {"
           "            ... on Repository {"
           "              name"
           "              sshUrl"
           "            }"
           "          }"
           "        }"
           "      }"
           "    }"
           "") "\n"))
       (gh-command (s-format "gh api graphql --hostname git.drwholdings.com -f query='${query-string}'" 'aget `(("query-string" . ,query-string))))
       (jq-process "jq '.data.search.repos[] | .repo | [.name, .sshUrl] | @tsv' -rcM")
       (table `(("query-string" . ,query-string)
                ("gh-command" . ,gh-command)
                ("jq-process" . ,jq-process))))
    (s-format "${gh-command} | ${jq-process}" 'aget table)
    )
  )

(set-eshell-alias!
 "s" "eshell/git status"
 "update-workspace-cargolock" "for f in { fd -c never Cargo.lock .. } { pushd ${dirname $f}; cargo update -w --offline; popd }"
 "check-workspace-cargolock" "for f in { fd -c never Cargo.lock .. } { pushd ${dirname $f}; cargo update -w --locked; popd }"
 "ri1-chhqctl" "kubectl --context=ri1-chhq $*"
 "ri1-chhq-devctl" "kubectl --context=ri1-chhq-dev $*"
 "risk-dev-chhq1ctl" "kubectl --context=risk-dev-chhq-1 $*"
 "risk-prod-chhq2ctl" "kubectl --context=risk-prod-chhq-2 $*"
 "risk-prod-ln1x2ctl" "kubectl --context=risk-prod-ln1x-2 $*"
 "risk1-chhqctl" "kubectl --context=risk1-chhq $*"
 "risk1-ln1xctl" "kubectl --context=risk1-ln1x $*"
 "ti-devctl" "kubectl --context=ti-dev $*"
 "ti-prodctl" "kubectl --context=ti-prod $*"
 "ti-prod-ch2d1ctl" "kubectl --context=ti-prod-ch2d-1 $*"
 "ti-prod-chhq2ctl" "kubectl --context=ti-prod-chhq-2 $*"
 "up-dev-chhq1ctl" "kubectl --context=up-dev-chhq-1 $*"
 "up-dev-chhq2ctl" "kubectl --context=up-dev-chhq-2 $*"
 "up-dev-chhq3ctl" "kubectl --context=up-dev-chhq-3 $*"
 "up-dev-chhq4ctl" "kubectl --context=up-dev-chhq-4 $*"
 "up-prod-ch2d1ctl" "kubectl --context=up-prod-ch2d-1 $*"
 "up-prod-chhq1ctl" "kubectl --context=up-prod-chhq-1 $*"
 "up-prod-chhq2ctl" "kubectl --context=up-prod-chhq-2 $*"
 "up-prod-use21ctl" "kubectl --context=up-prod-use2-1 $*"
 "xdct-prod-ch2d1ctl" "kubectl --context=xdct-prod-ch2d-1 $*"
 "xdct-prod-use21ctl" "kubectl --context=xdct-prod-use2-1 $*"
 )

(defun shell-new ()
  (interactive)

  (let ((currentbuf (get-buffer-window (current-buffer)))
        (newbuf     (generate-new-buffer-name "*shell*")))

    (generate-new-buffer newbuf)
    (set-window-dedicated-p currentbuf nil)
    (set-window-buffer currentbuf newbuf)
    (shell newbuf)))

(setq confirm-kill-emacs nil)

(use-package! telephone-line
  :config
  (telephone-line-mode 1))
;;
;; mu4e settings
(add-load-path! "/opt/homebrew/share/emacs/site-lisp/mu")
(use-package! mu4e
  :defer t
  :config
  (set-email-account!
   "ccomar@drwholdings.com"
   '((user-mail-address . "ccomar@drwholdings.com")
     (mtpmail-smtp-user . "us\\ccomar")
     (mu4e-sent-folder       . "/drwholdings/Sent")
     (mu4e-drafts-folder     . "/drwholdings/Drafts")
     (mu4e-trash-folder      . "/drwholdings/Trash")
     (mu4e-refile-folder     . "/drwholdings/Archive")))
  :custom
  (mu4e-mu-binary (executable-find "mu"))
  (mu4e-maildir "~/.maildir")
  (mu4e-get-mail-command (concat (executable-find "mbsync") " -a"))
  (mu4e-update-interval 60)
  (mu4e-attachment-dir "~/Downloads")
  (mu4e-change-filenames-when-moving t)
  (smtpmail-smtp-server "localhost")
  (smtpmail-smtp-service 1025))
(after! mu4e
  (setq sendmail-program (executable-find "msmtp")
        send-mail-function #'smtpmail-send-it
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-send-mail-function #'message-send-mail-with-sendmail
        message-kill-buffer-on-exit t))

;; (use-package! excorporate
;;   :custom
;;   (excorporate-calendar-show-day-function #'exco-calfw-show-day)
;;   (excorporate-time-zone "Eastern Standard Time")
;;   (excorporate-configuration
;;    '("ccomar@drwholdings.com" . "https://webmail.drwholdings.com/EWS/Exchange.asmx"))
;;   (org-agenda-include-diary t)
;;   :config
;;   (require 'excorporate-calfw)
;;   ;;  (if (daemonp)
;;   ;;      (add-hook 'after-make-frame-functions
;;   ;;                (lambda (frame)
;;   ;;                  (interactive)
;;   ;;                  (with-selected-frame frame
;;   ;;                      (excorporate)
;;   ;;                      )))
;;   ;;      (excorporate))
;;   )

(defun +calendar/show-day ()
  (interactive)
  (apply #'exco-calfw-show-day (cfw:cursor-to-nearest-date)))

(defun +calendar/open ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list (cfw:cal-create-source "Cyan"))))

(map! :map cfw:calendar-mode-map :m "m" #'+calendar/show-day)
(map! :leader "c o" #'+calendar/open)

(defun colorize-buffer ()
  (interactive)
  (read-only-mode -1)
  (ansi-color-apply-on-region (point-min) (point-max))
  (read-only-mode +1))
(setq magit-process-finish-apply-ansi-colors t)
(add-hook! magit-post-stage-hook #'colorize-buffer)



(defun clone-or-update-command (line)
  (let* ((split (s-slice-at "[[:space:]]+" line))
         (name (seq-elt split 0))
         (url (seq-elt split 1))
         (table `(("name" . ,name) ("url" . ,url)))
         (fragment-template  "  if { test -d ${name} } { *echo updating ${name}; pushd ${name}; git checkout main || git checkout master; git pull; popd; } { *echo cloning ${name}; git clone ${url} ${name}; };"))
    (s-format fragment-template 'aget table)))

(defun wrap-command (command)
  (let* ((bufname-saver '(setq-local bufname (buffer-name (current-buffer))))
         (renamer '(rename-buffer bufname)))
    (cl-concatenate 'string "  " (prin1-to-string bufname-saver) "\n\n" command "\n  " (prin1-to-string renamer))))

(defun partition-commands (commands)
  (lambda (n)
    (let* ((len (seq-length commands))
           (indices (seq-filter (lambda (i) (= (mod i 4) (- n 1))) (number-sequence 0 (- len 1))))
           (filtered-commands  (seq-map (lambda (i) (seq-elt commands i)) indices)))
      (wrap-command (mapconcat 'identity filtered-commands "\n")))))

(add-hook! eshell-mode #'eat-eshell-mode)
(add-hook! eshell-mode #'eat-eshell-visual-command-mode)

;; (add-hook! 'org-mode-hook '(org-fragtog-mode))
(add-hook! 'lean4-mode-hook
  (set-input-method "Lean")
  (toggle-input-method))
(use-package! lean4-mode
  :mode "\\.lean\\'")

(use-package! protobuf-mode
  :mode "\\.proto\\'")

(use-package! envrc
  :config
  (setq! envrc-async-processing t)
  (envrc-global-mode))

;; (use-package! sublimity
;;   :config
;;   (require 'sublimity-scroll)
;;   (setq sublimity-scroll-weight 5
;;         sublimity-scroll-drift-length 1)
;;   (sublimity-mode))

(after! eshell

  (defun +esh-help/async-man-string (cmd)
    "Return help string for the shell command CMD."
    (let ((lang (getenv "LANG")))
      (setenv "LANG" "C")
      (let ((str (aio-wait-for (aio-run manual-program (list (format "%s %s | col -b" manual-program (file-name-base cmd)))))))
        (setenv "LANG" lang)
        str)))

  (advice-add 'esh-help-man-string :override #'+esh-help/async-man-string)
  (advice-add 'eshell/async-command-to-string :around #'envrc-propagate-environment))

;; (use-package! sideline
;;   :after eglot flymake
;;   :init
;;   (setq sideline-force-display-if-exceeds t))

;; (use-package! sideline-flymake
;;   :after sideline
;;   :hook (flymake-mode . sideline-mode)
;;   :init
;;   (setq sideline-flymake-display-mode 'point) ;; 'point to show errors only on point
;;                                         ; 'line to show errors on the current line
;;   (appendq! sideline-backends-right '(sideline-flymake)))

(use-package! shx
  :after shell
  :hook (shell-mode-hook . shx-mode))

(use-package! eat
  :config
  ;; TODO: set comint-unquote-function and comint-requote-function to something appropriate for nushell
  ;; quoted filepaths should use `` instead of "" and paths should not be escaped.
  (defun +eat/here (&optional program)
    (interactive)
    (eat--1 nil "new" #'pop-to-buffer-same-window))
  (map! :leader "RET" #'+eat/here)
  (map! :leader "<return>" #'+eat/here)

  (setq nu-executable-path (executable-find "nu"))
  (setq eat-enable-directory-tracking t
        eat-shell (format "%s --config '%s/.config/nushell/emacs-config.nu'" nu-executable-path (getenv "HOME"))
        eat-enable-mouse t
        eat-enable-auto-line-mode nil
        eat-enable-kill-from-terminal t
        eat-enable-yank-to-terminal t
        eat-enable-shell-command-history t
        eat-term-terminfo-directory (format "%s/.terminfo" (getenv "HOME"))
        eat-enable-directory-tracking t
        eat-enable-shell-prompt-annotation t
        eat-enable-blinking-text nil)
  (add-to-list 'consult-mode-histories '(eat-mode eat--line-input-ring eat--line-input-ring-index comint-bol))
  (add-to-list 'eat-semi-char-non-bound-keys [C-r])
  (defun +eat/nu-open (&rest args)
    (interactive)
    (cl-callf find-file (car args) (cdr args)))
  (add-to-list 'eat-message-handler-alist '("open" . +eat/nu-open))

  (keymap-set eat-mode-map "<insert-state> C-r" #'consult-history)
  (keymap-set eat-mode-map "<normal-state> C-r" #'consult-history)

  (keymap-set eat-mode-map "<insert-state> C-j" #'eat-next-shell-prompt)
  (keymap-set eat-mode-map "<normal-state> C-j" #'eat-next-shell-prompt)

  (keymap-set eat-mode-map "<insert-state> C-k" #'eat-previous-shell-prompt)
  (keymap-set eat-mode-map "<normal-state> C-k" #'eat-previous-shell-prompt)

  (keymap-set eat-mode-map "<insert-state> <tab>" #'completion-at-point)
  (keymap-set eat-mode-map "<normal-state> <tab>" #'completion-at-point)

  (setq nushell-history-file (shell-command-to-string "nu -c '$nu.history-path | print -n'"))

  (load "s")
  (load "dash")
  (defcustom carapace-nushell-quoted-arg-chars "~/A-Za-z0-9\\+@:_\\.\\$#%,={} -"
    "regex matching an unquoted argument")
  (defcustom carapace-nushell-unquoted-arg-chars "~/A-Za-z0-9\\+@:_\\.\\$#%,={}-"
    "regex matching an unquoted argument")
  (defcustom carapace-nushell-quoted-arg-regex
    (format "['\"`][%s]+?['\"`][%s]*"
            carapace-nushell-quoted-arg-chars
            carapace-nushell-quoted-arg-chars)
    "regex matching a quoted argument")
  (defcustom carapace-nushell-unquoted-arg-regex (format "[%s]" carapace-nushell-unquoted-arg-chars)
    "regex matching a quoted argument")
  (setq carapace-nushell-quoted-arg-chars "~/A-Za-z0-9\\+@:_\\.\\$#%,={} -")
  (setq carapace-nushell-quoted-arg-regex
        (format "['\"`][%s]+?['\"`][%s]*"
                carapace-nushell-quoted-arg-chars
                carapace-nushell-quoted-arg-chars))

  (setq carapace-completion-command (executable-find "carapace"))
  (defun carapace-completion--fish-fallback (raw-prompt)
    (let* ((prompt (if (equal raw-prompt "") " " raw-prompt))
           (completions (fish-completion--list-completions prompt)))
      (cl-reduce (lambda (table comp)
                   (puthash (s-concat comp " ") `(:display ,comp :value ,comp) table)
                   table)
                 completions
                 :initial-value (make-hash-table :test #'equal :size (length completions)))))

  (defun carapace-completion--call (command &rest args)
    "Return the output of the call to COMMAND ARGS as a string."
    (with-output-to-string
      (with-current-buffer standard-output
        (apply #'call-process
               command
               nil '(t nil) nil
               args))))

  (defun carapace-completion--list-completions-with-desc (shell raw-prompt)
    (if-let* ((quote-count (s-count-matches "`" raw-prompt))
              (prompt (s-replace "`" "'" (if (cl-oddp quote-count) (s-concat raw-prompt "`") raw-prompt)))
              (tokens (split-string-shell-command prompt))
              (command (car tokens))
              (raw-completions (apply #'carapace-completion--call
                                      `(,carapace-completion-command ,command ,(symbol-name shell) ,@tokens)))
              (parsed (ignore-errors (json-parse-string raw-completions :object-type 'plist :array-type 'array))))
        (when parsed
          (cl-reduce (lambda (table comp)
                       (cl-destructuring-bind (&key value &allow-other-keys) comp
                         (puthash (s-replace-regexp "['\"`]" "" value) comp table)
                         table))
                     parsed
                     :initial-value (make-hash-table :test #'equal :size (length parsed))))
      (carapace-completion--fish-fallback raw-prompt)))

  (defun carapace-nushell--line-offset (offset-bounds)
    (cl-flet ((offset-f (bound) (+ (comint-line-beginning-position) bound)))
      (pcase offset-bounds
        (`(,offset-beg . ,offset-end)
         (let ((beg (offset-f offset-beg))
               (end (offset-f offset-end)))
           `(,beg . ,end))))))

  (defun carapace-nushell--point-in-arg-p (arg-bounds)
    (pcase (carapace-nushell--line-offset arg-bounds)
      (`(,beg . ,end)
       (and (>= (point) beg)
            (<= (point) end)))))

  (defun carapace-nushell--unquoted-arg-at-point ()
    (let* ((rawbeg (save-excursion (search-backward-regexp "\\(^\\|\s\\)")))
           (rawend (save-excursion (search-forward-regexp "\\($\\|\s\\)")))
           ;; handle inclusivity of bounds
           (beg (if (eql (char-after rawbeg) ?\s) (+ rawbeg 1) rawbeg))
           (end (if (eql (char-before rawend) ?\s) (- rawend 1) rawend))
           (bol (comint-line-beginning-position))
           (eol (pos-eol)))
      (list
       (if (< beg bol) bol beg)
       (if (> end eol) eol end))))

  (defun carapace-nushell--arg (raw-prompt)
    (let* ((quoted-args  (nreverse (s-matched-positions-all carapace-nushell-quoted-arg-regex raw-prompt)))
           (quoted-arg-offset-at-point (cl-first (seq-filter #'carapace-nushell--point-in-arg-p quoted-args))))
      (pcase (carapace-nushell--line-offset quoted-arg-offset-at-point)
        (`(,beg . ,end) (list beg end))
        (_ (carapace-nushell--unquoted-arg-at-point)))))

  (defun carapace-nushell--raw-prompt (&optional eol)
    "extract the current prompt for completion up to the provided eol marker"
    (let ((end (or eol (pos-eol))))
      (buffer-substring-no-properties (comint-line-beginning-position) end)))

  (defvar-local carapace-nushell--active-completions nil)
  (defun carapace-nushell--completions (prompt &optional no-refresh)
    (if no-refresh
        (or carapace-nushell--active-completions
            (carapace-nushell--completions prompt nil))
      (let ((candidates (carapace-completion--list-completions-with-desc 'nushell prompt)))
        (setq-local carapace-nushell--active-completions candidates))))

  (defun carapace-nushell-backend (action &optional arg &rest _)
    (let* ((prompt (carapace-nushell--raw-prompt))
           (completion-prompt (carapace-nushell--raw-prompt (point)))
           (command (cl-first (split-string prompt))))
      (pcase action
        ('prefix (ignore-errors
                   (pcase-let* ((quote-count (s-count-matches "`" completion-prompt))
                                (quoted-prompt (if (cl-oddp quote-count)
                                                   (s-concat completion-prompt "`")
                                                 completion-prompt))
                                (`(,beg ,end) (carapace-nushell--arg quoted-prompt))
                                (real-end (if (cl-oddp quote-count) (- end 1) end))
                                (prefix (buffer-substring-no-properties beg (point)))
                                (unquoted-prefix (s-replace-regexp "['\"`]" "" prefix))
                                (suffix (buffer-substring-no-properties (point) real-end)))
                     (list unquoted-prefix suffix))))
        ('candidates (when-let* ((candidates (carapace-nushell--completions completion-prompt))
                                 (possible (hash-table-keys candidates))
                                 (unquoted-arg (s-replace-regexp "['\"`]" "" arg))
                                 (pred (lambda (key cand) (s-contains? unquoted-arg key))))
                       (seq-filter (lambda (key) (funcall pred key (gethash key candidates))) possible)))

        ('annotation (let* ((candidates (carapace-nushell--completions completion-prompt t))
                            (cand (gethash arg candidates)))
                       (plist-get cand :description)))
        ('post-completion
         (setq-local carapace-nushell--active-completions nil)

         ;; remove the inserted completion for requoting
         (delete-char (- (length arg)))

         ;; clean up quotes that weren't part of the prefix but were part of the current arg
         (if (eql (char-before (point)) ?`) (delete-char -1))
         (if (eql (char-after (point)) ?`) (delete-char 1))

         (let* ((unquoted-arg (s-replace-regexp "['\"`]" "" (s-trim arg)))
                (requoted-arg (if (s-contains? " " unquoted-arg)
                                  (s-wrap unquoted-arg "`")
                                unquoted-arg)))
           (insert requoted-arg))))))

  (defun replace-eat-completions ()
    (fish-completion-mode -1)
    (setq-local completion-at-point-functions
                (cape-company-to-capf #'carapace-nushell-backend (lambda (&rest _) carapace-nushell--active-completions))))

  (setq-hook! 'eat-mode-hook consult-preview-key nil)
  (add-hook! 'eat-exec-hook
    (lambda (_)
      (eat-line-mode)
      (eat-line-load-input-history-from-file nushell-history-file "bash")
      (replace-eat-completions)))
  (add-hook! 'eat-update-hook
    (eat-line-mode)
    (eat-line-load-input-history-from-file nushell-history-file "bash")
    (replace-eat-completions))
  )

(use-package! nushell-mode
  :mode ("\\.nu\\'" . nushell-mode)
  :config
  (add-hook 'nushell-mode-hook #'lsp 'append))
(use-package! nushell-ts-mode
  :hook (nushell-mode . nushell-ts-mode))

;; (use-package! rego-mode
;;   :defer t
;;   :mode "\\.rego\\'"
;;   :init
;;   (set-eglot-client! rego-mode '("regols"))
;;   (add-hook! 'rego-mode #'eglot #'+format-with-lsp-mode)
;;   :custom
;;   (rego-format-at-save nil))
(setq vundo-glyph-alist vundo-unicode-symbols)

(use-package! gptel
  :config
  (setq gptel-model 'deepseek-code:latest
        gptel-backend (gptel-make-ollama "Ollama"
                        :host "localhost:11434"
                        :stream t
                        :models '(deepseek-code:latest))))

(use-package! gptel-autocomplete
  :after gptel
  :config
  (map! :nvi
        "C-<tab>" #'gptel-complete
        "C-RET" #'gptel-accept-completion))
