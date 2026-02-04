;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Cassandra Comar"
      user-mail-address "cass@nie.rs")

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
(setq coding-system--for-buffer-diff 'utf-8-unix)
(add-to-list 'auto-coding-alist '("/\\.editorconfig" . utf-8-emacs-unix))

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
(setq confirm-kill-emacs nil)
(setq fill-column 120)

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

(igc-start-idle-timer)
(add-hook 'emacs-startup-hook
          (lambda ()
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state)
                                  (igc-collect))))
              (add-hook 'after-focus-change-function
                        (lambda ()
                          (igc-collect))))))

;; ENVIRONMENT
(setenv "SSH_AUTH_SOCK" (shell-command-to-string "gpgconf --list-dirs agent-ssh-socket"))
(setenv "GPG_AGENT_INFO" (shell-command-to-string "gpgconf --list-dirs agent-socket"))
(setenv "TERM" "xterm-256color")
(setenv "PAGER" "bat -f -pp")
(setenv "DOOMPAGER" "bat -f -pp")

(advice-add 'risky-local-variable-p :override #'ignore)
(setq enable-local-variables :all)
(setq enable-local-eval t)
(setq with-editor-emacsclient-executable (executable-find "emacsclient"))
(setq! forge-database-connector 'sqlite-builtin)

;; set up env vars from encrypted sources
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
(defface gnus-group-news-low-empty
  '((((class color)
      (background dark))
     (:foreground "DarkTurquoise"))
    (((class color)
      (background light))
     (:foreground "DarkGreen"))
    (t
     ()))
  "Low level empty newsgroup face."
  :group 'gnus-group)

(defface gnus-group-news-low
  '((((class color)
      (background dark))
     (:foreground "DarkTurquoise"
      :weight bold))
    (((class color)
      (background light))
     (:foreground "DarkGreen"
      :weight bold))
    (t
     ()))
  "Low level newsgroup face."
  :group 'gnus-group)

;; UI
(use-package! doom-modeline
  :config
  (setq doom-modeline-lsp nil
        doom-modeline-hud t))

(use-package! spacious-padding
  :hook '((after-init . spacious-padding-mode))
  :init
  (setq spacious-padding-widths
        '(:internal-border-width 5
          :header-line-width 4
          :left-fringe-width 0
          :mode-line-width 0
          :tab-width 0
          :right-divider-width 5
          :scroll-bar-width 2)))

(use-package! eglot
  :init
  (load! "+eglot")
  :config
  (remove-hook! '(eglot-managed-mode-hook doom-modeline-mode-hook) #'doom-modeline-override-eglot)

  (add-hook! 'eglot-managed-mode-hook
    (add-to-list
     'mode-line-misc-info
     `(eglot--managed-mode
       (" ["
        (:eval
         (cl-loop for e in eglot-mode-line-format
                  for render = (format-mode-line e)
                  unless (eq render "")
                  collect (cons render
                                (eq e 'eglot-mode-line-menu))
                  into rendered
                  finally
                  (return (cl-loop for (rspec . rest) on rendered
                                   for (r . titlep) = rspec
                                   concat r
                                   when rest concat (if titlep ":" "/")))))
        "] "))))
  (map! :map eglot-mode-map :nv "g D" #'+lookup/references)
  (add-to-list 'eglot-semantic-token-types "const")
  (add-to-list 'eglot-semantic-token-types "lifetime")
  (add-to-list 'eglot-semantic-token-types "generic")
  (add-to-list 'eglot-semantic-token-types "constant")
  (setq eglot-semantic-token-modifiers (remove "defaultLibrary" eglot-semantic-token-modifiers))
  :custom-face
  (my-font-lock-variable-use-face ((t (:foreground "#fda135"))))
  (my-font-lock-type-face ((t (:foreground "#5c9cff"))))
  (my-font-lock-type-parameter-face ((t (:foreground "#66d9ef"))))
  (my-font-lock-constant-face ((t (:inherit font-lock-property-use-face))))
  (eglot-semantic-namespace ((t (:foreground "#fb3d81"))))
  (eglot-semantic-type ((t (:inherit my-font-lock-type-face))))
  (eglot-semantic-class ((t (:inherit elisp-symbol-role))))
  (eglot-semantic-enum ((t (:inherit elisp-major-mode-name))))
  (eglot-semantic-enumMember ((t (:inherit my-font-lock-constant-face :slant italic))))
  (eglot-semantic-interface ((t (:inherit elisp-symbol-role))))
  (eglot-semantic-struct ((t (:inherit elisp-major-mode-name))))
  (eglot-semantic-typeParameter ((t (:inherit my-font-lock-type-parameter-face :slant italic))))
  (eglot-semantic-lifetime ((t (:inherit font-lock-preprocessor-face :slant italic))))
  (eglot-semantic-parameter ((t (:inherit my-font-lock-variable-use-face :slant italic))))
  (eglot-semantic-variable ((t (:inherit font-lock-variable-name-face :slant italic))))
  (eglot-semantic-property ((t (:inherit font-lock-property-use-face :slant italic))))
  (eglot-semantic-event ((t (:inherit my-font-lock-type-parameter-face))))
  (eglot-semantic-function ((t (:inherit font-lock-function-name-face :slant italic))))
  (eglot-semantic-method ((t (:inherit font-lock-function-name-face :slant italic))))
  (eglot-semantic-macro ((t (:inherit font-lock-preprocessor-face :slant italic))))
  (eglot-semantic-generic ((t (:inherit font-lock-variable-name-face :slant italic))))
  (eglot-semantic-number ((t (:inherit my-font-lock-constant-face))))
  (eglot-semantic-regexp ((t (:inherit font-lock-preprocessor-face))))
  (eglot-semantic-operator ((t (:inherit font-lock-keyword-face))))
  (eglot-semantic-const ((t (:inherit my-font-lock-constant-face))))
  (eglot-semantic-constant ((t (:inherit my-font-lock-constant-face))))
  (eglot-semantic-declaration ((t (:underline t :slant normal))))
  (eglot-semantic-definition ((t (:underline t :slant normal))))
  (eglot-semantic-static ((t (:overline t :slant normal))))
  (eglot-semantic-modification ((t (:underline t :slant normal)))))

(use-package! eglot-hover
  :defer t
  :hook '((eglot-managed-mode . eglot-hover-mode)))

;; KEYBINDINGS
(defun consult-flymake-project (&rest _)
  (interactive)
  (consult-flymake t))
(setq doom-localleader-key ",")
(map! :leader "SPC" #'execute-extended-command)
(map! :leader "S-SPC" #'execute-extended-command-for-buffer)
(map! :leader "p t" #'+treemacs/toggle)
(map! :leader "o o" #'envrc-reload)
(map! :leader "p p" #'consult-projectile-switch-project)
(map! :leader "p f" #'consult-projectile)
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
(map! :leader "b b" #'consult-buffer)
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
(map! :nv "g b" #'xref-go-back)
(map! :nv "g B" #'xref-go-forward)
(map! :nv "g D" #'+lookup/references)
(map! :nv "g d" #'+lookup/definition)
(map! :nv "g i" #'+lookup/implementations)
(map! :nv "H" #'treemacs-select-window)
(map! :nv "L" #'eglot-hierarchy-mode)
(map! :n  "V" #'evil-visual-line)
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
         corfu-max-width 120)
  :custom
  corfu-auto t)
(use-package! corfu-auto
  :config
  (setq! corfu-auto-prefix 0))

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
(after! projectile  
  (add-to-list 'projectile-globally-ignored-directories "dist-newstyle"))

(use-package! consult-projectile
  :demand t)

;; configure evil
;; make evil-search-word look for symbol rather than word boundaries
(defalias #'forward-evil-word #'forward-evil-symbol)
(setq! evil-symbol-word-search t)

;; don't substitute globally by default
(setq! evil-ex-substitute-global nil)

;; set up smartparens
(after! smartparens
  (show-smartparens-global-mode +1))

;; magit
(add-hook! 'after-save-hook #'magit-after-save-refresh-status)

(use-package! forge
  :defer t
  :config
  (push '("git.drwholdings.com"               ; GITHOST
          "git.drwholdings.com/api/v3"        ; APIHOST
          "git.drwholdings.com"               ; WEBHOST and INSTANCE-ID
          forge-github-repository)            ; CLASS
        forge-alist))

(use-package! terraform-mode
  :defer t
  :init
  (setq terraform-command "tofu"))

(use-package! rustic
  :defer t
  :config
  (setq rustic-format-on-save t)
  :custom-face
  (rust-ampersand-face ((t (:inherit font-lock-keyword-face))))
  (rust-builtin-formatting-macro ((t (:inherit font-lock-preprocessor-face)))))

(set-popup-rule! "^\\*helpful" :size 0.5 :quit t :select t :side 'right)
(set-popup-rule! "^\\*lsp-help\\*" :size 0.5 :quit t :select t :side 'right)

(use-package! markdown-mode
  :defer t
  :mode "\\.md\\'"
  :hook '((markdown-mode . auto-fill-mode))
  :config
  (add-to-list 'markdown-code-lang-modes '("nix" . nix-ts-mode))
  (add-to-list 'markdown-code-lang-modes '("yaml" . yaml-ts-mode)))

(set-formatter! 'nixpkgs-fmt '("nix" "fmt" "--" "-") :modes '(nix-mode))

;; haskell stuff
(plist-put! +ligatures-extra-symbols
            :type "⦂"
            :composition "∘"
            :dot ".")
(setq +ligatures-prog-mode-list '("<=<" ">=>" ">>=" ">>-" "=<<" "-<<" "<." "<.>" ".>" "<*" "<*>" "*>" "\\/" "/\\" "==>" "<==" "/=" "==" "->" "<-" "=>" "<=" "||" "&&" "<|>" "<<<<" ">>>>" ">>>" "<<<" ">>" "<<" ".." "..." "<|" "|>" "<>"))
(ligature-set-ligatures 't +ligatures-prog-mode-list)

(use-package! haskell-mode
  :defer t
  :config
  (add-hook 'haskell-mode-hook #'lsp!)
  (set-eglot-client! '(haskell-mode haskell-ts-mode) '("haskell-language-server-wrapper" "-d" "lsp") "haskell-language-server")
  :custom-face
  (haskell-operator-face ((t (:inherit haskell-keyword-face))))
  (haskell-definition-face ((t (:inherit haskell-keyword-face)))))

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

;; yaml-mode
(use-package! yaml-ts-mode
  :commands yaml-ts-mode
  :mode "\\.yaml\\(\\.j2\\)?\\'"
  :config
  (set-company-backend! 'yaml-ts-mode 'company-capf))
(add-hook! yaml-ts-mode (corfu-mode +1))

;; projectile
(add-hook! projectile-after-switch-project-hook '(projectile-invalidate-cache nil))

(after! evil-org
  (setq org-want-todo-bindings t)
  (setq org-log-done t)
  (setq org-todo-keywords '((sequence "TODO(t)" "WORKING(w!)" "BLOCKED(b@/!)" "STALLED(s!)" "|" "DONE(d!)" "DEFERRED(f!)" "CANCELED(c)")))
  (evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading)))

(map! :after evil-org
      :map evil-org-mode-map
      :nv
      [remap evil-org-org-insert-heading-below] #'+org/insert-item-above
      [remap evil-org-open-below] #'+org/insert-item-below)

;; jsonnett
(use-package! jsonnet-mode
  :defer t
  :mode "\\(\\.libsonnet\\|\\.jsonnet\\)\\'"
  :hook '((jsonnet-mode . lsp!)))

(load! "+eshell")

;; FUNCTIONS
(defun ex-save-kill-buffer-and-close ()
  (interactive)
  (save-buffer)
  (kill-this-buffer))
(evil-ex-define-cmd "q" 'kill-this-buffer)
(evil-ex-define-cmd "wq" 'ex-save-kill-buffer-and-close)
(evil-ex-define-cmd "x" 'ex-save-kill-buffer-and-close)

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
 "xdct-prod-use21ctl" "kubectl --context=xdct-prod-use2-1 $*")

(defun shell-new ()
  (interactive)

  (let ((currentbuf (get-buffer-window (current-buffer)))
        (newbuf     (generate-new-buffer-name "*shell*")))

    (generate-new-buffer newbuf)
    (set-window-dedicated-p currentbuf nil)
    (set-window-buffer currentbuf newbuf)
    (shell newbuf)))

;;
;; mu4e settings
(use-package! mu4e
  :defer t
  :config
  (set-email-account!
   "cass@nie.rs"
   '((user-full-name . "Cassandra Comar")
     (user-mail-address . "cass@nie.rs")
     (mtpmail-smtp-user . "cass@nie.rs")
     (mu4e-sent-folder       . "/cass/sent")
     (mu4e-drafts-folder     . "/cass/drafts")
     (mu4e-trash-folder      . "/cass/trash")
     (mu4e-refile-folder     . "/cass/archive")))
  (setq mu4e-update-interval 60)
  :custom
  (mu4e-mu-binary (executable-find "mu"))
  (mu4e-maildir "~/.local/share/maildir")
  (mu4e-get-mail-command (concat (executable-find "mbsync") " -a"))
  (mu4e-update-interval 60)
  (mu4e-attachment-dir "~/downloads")
  (mu4e-change-filenames-when-moving t))
(after! mu4e
  (setq sendmail-program (executable-find "msmtp")
        send-mail-function #'smtpmail-send-it
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-send-mail-function #'message-send-mail-with-sendmail
        message-kill-buffer-on-exit t))

(add-hook! eshell-mode #'eat-eshell-mode)
(add-hook! eshell-mode #'eat-eshell-visual-command-mode)

(use-package! protobuf-mode
  :defer t
  :mode "\\.proto\\'")

(use-package! envrc
  :config
  (setq! envrc-async-processing t)
  (envrc-global-mode))

(use-package! sideline
  :after eglot flymake
  :hook '((flymake-mode . sideline-mode))
  :init
  (setq sideline-force-display-if-exceeds t))

(use-package! sideline-flymake
  :after sideline flymake
  :init
  (setq sideline-flymake-display-mode 'point))

(use-package! sideline-eglot
  :after sideline eglot
  :init
  (setq sideline-backends-right '(sideline-flymake sideline-eglot)))

(use-package! eat
  :defer t
  :init
  (load! "+eat-nushell")
  (setq process-adaptive-read-buffering t)
  :config
  (setq-hook! 'eat-mode-hook consult-preview-key nil)
  (add-hook 'eat-exec-hook
            (lambda (&rest _)
              (eat-line-mode)
              (eat-line-load-input-history-from-file nushell-history-file "bash")
              (replace-eat-completions)))
  (add-hook 'eat-update-hook
            (lambda (&rest _)
              (eat-line-mode)
              (eat-line-load-input-history-from-file nushell-history-file "bash")
              (replace-eat-completions)))

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
  (add-to-list 'eat-message-handler-alist '("open" . +eat/nu-open))

  (keymap-set eat-mode-map "<insert-state> C-r" #'consult-history)
  (keymap-set eat-mode-map "<normal-state> C-r" #'consult-history)

  (keymap-set eat-mode-map "<insert-state> C-j" #'eat-next-shell-prompt)
  (keymap-set eat-mode-map "<normal-state> C-j" #'eat-next-shell-prompt)

  (keymap-set eat-mode-map "<insert-state> C-k" #'eat-previous-shell-prompt)
  (keymap-set eat-mode-map "<normal-state> C-k" #'eat-previous-shell-prompt)

  (keymap-set eat-mode-map "<insert-state> <tab>" #'completion-at-point)
  (keymap-set eat-mode-map "<normal-state> <tab>" #'completion-at-point))

(defun +eat/nu-open (&rest args)
  (interactive)
  (apply #'eshell/find-file args))

(defun +eat/here (&optional program)
  (interactive)
  (eat program "new"))

(map! :leader "RET" #'+eat/here)
(map! :leader "<return>" #'+eat/here)

(use-package! nushell-ts-mode
  :commands nushell-ts-mode
  :defer t
  :mode "\\.nu\\'"
  :hook '((nushell-ts-mode . lsp!)))
(add-hook! nushell-ts-mode (+format-with-lsp-mode -1))

(use-package! semel
  :after elisp-mode
  :config
  (setq semel-add-help-echo nil)
  :hook '((emacs-lisp-mode . semel-mode)
          (emacs-lisp-mode . cursor-sensor-mode)))

(use-package! rego-mode
  :defer t
  :mode "\\.rego\\'"
  :init
  (set-eglot-client! rego-mode '("regols"))
  (add-hook! rego-mode #'eglot #'+format-with-lsp-mode)
  :custom
  (rego-format-at-save nil))

(use-package! mermaid-mode
  :defer t
  :mode "\\.mmd\\'"
  :config
  (map! :map mermaid-mode-map
        :localleader
        :nv
        "d c" #'mermaid-compile
        "d f" #'mermaid-compile-file
        "d b" #'mermaid-compile-buffer
        "d r" #'mermaid-compile-region
        "d o" #'mermaid-open-browser
        "d d" #'mermaid-open-doc))

(use-package! ob-mermaid
  :defer t
  :after ob
  :init
  (add-to-list '+org-babel-mode-alist '(mermaid . mermaid)))

(use-package! elisp-benchmarks
  :defer t)
