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
(setq doom-font (font-spec :family "Inconsolata for Powerline" :size 32))

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
(setq! auth-sources '("~/.authinfo.gpg"))
(setenv "PAGER" "cat")

;; KEYBINDINGS
(map! :leader "SPC" #'execute-extended-command)
(map! :leader "p t" #'+treemacs/toggle)
(map! :leader "DEL" #'projectile-find-file)
(map! :nv "TAB" #'evil-indent
      :nv [tab] #'evil-indent)
(map! :leader "g s" #'magit)
(map! :leader "b b" #'switch-to-buffer)
(map! :leader "o u" #'undo-tree-visualize)

;; PACKAGE CONFIG
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

;; set up LSP
(after! lsp
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("yaml.validate" t t)
     ("yaml.hover" t t)
     ("yaml.completion" t t)
     ("yaml.schemaStore.enable" t t)))
  (setq lsp-prefer-capf t)
  (require 'lsp-clients)
  (lsp-treemacs-sync-mode 1)
  (setq lsp-auto-execute-action t)
  (setq lsp-enable-semantic-highlighting t)
  (setq lsp-before-save-edits t)
  (setq lsp-enable-snippet t)
  )
; show lsp sideline on hover ... doom docs mark this as noisy so disable if it causes a problem
(after! lsp-ui
  (setq lsp-ui-sideline-show-hover t))
(add-hook! sh-mode #'lsp)

; rust stuff
(after! rustic
  (setq rustic-lsp-server 'rust-analyzer)
  (setq rustic-format-trigger 'on-save)
  (setq rust-format-on-save t)
   )
(add-hook! rustic-mode lsp-rust-analyzer-inlay-hints-mode)

; groovy stuff
(after! groovy-mode
  (add-to-list 'auto-mode-alist '("Jenkinsfile$" . groovy-mode)))

; yaml-mode
(after! yaml-mode
  (add-to-list 'auto-mode-alist '("\\.yaml\\.j2\\'" . yaml-mode)))

; org-mode
(after! evil-org
  (setq org-want-todo-bindings t)
  (setq org-todo-keywords '((type "TODO(t)" "WORKING(w!)" "BLOCKED(b@/!)" "STALLED(s!)" "|" "DONE(d!)" "DEFERRED(f!)" "CANCELED(c)")))
  (evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading))
)
(add-hook! org-mode visual-line-mode)

; eaf
(use-package! eaf
  :defer t)

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
