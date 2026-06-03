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
(setq doom-font "Iosevka Nerd Font Mono:size=20:weight=light")
(setq doom-symbol-font "Iosevka Nerd Font Mono")
(setq nerd-icons-font-family "Iosevka Nerd Font Mono")
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

(add-hook! 'doom-load-theme-hook
           ;; Ediff: remove fg overrides so syntax highlighting shows through.
           ;; Fine-diff gets brighter bg than current-diff for word-level emphasis.
           (let ((bg (face-background 'default))
                 (red (face-foreground 'error nil t))
                 (green (face-foreground 'success nil t)))
             (custom-set-faces!
               `(ediff-odd-diff-A :foreground unspecified :inherit nil
                 :background ,(doom-blend red bg 0.05) :extend t)
               `(ediff-odd-diff-B :foreground unspecified :inherit nil
                 :background ,(doom-blend green bg 0.05) :extend t)
               `(ediff-even-diff-A :foreground unspecified :inherit nil
                 :background ,(doom-blend red bg 0.05) :extend t)
               `(ediff-even-diff-B :foreground unspecified :inherit nil
                 :background ,(doom-blend green bg 0.05) :extend t)
               `(ediff-current-diff-A :foreground unspecified :inherit nil
                 :background ,(doom-blend red bg 0.2) :extend t)
               `(ediff-current-diff-B :foreground unspecified :inherit nil
                 :background ,(doom-blend green bg 0.2) :extend t)
               `(ediff-fine-diff-A    :foreground unspecified :inherit nil
                 :background ,(doom-blend red bg 0.35) :extend t)
               `(ediff-fine-diff-B    :foreground unspecified :inherit nil
                 :background ,(doom-blend green bg 0.35) :extend t))))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/todo/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)
(menu-bar-mode -1)
(setq confirm-kill-emacs nil)
(setq-default fill-column 120)
(setq nobreak-char-display nil)
(setq projectile-project-search-path `((,(format "%s/src" (getenv "HOME")) . 3)))
(setq initial-major-mode 'fundamental-mode)

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

(add-to-list 'load-path doom-user-dir)

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
(defun +gpgconf-set-agent-env ()
  "Populate SSH_AUTH_SOCK and GPG_AGENT_INFO from one gpgconf call."
  (with-temp-buffer
    (when (zerop (call-process "gpgconf" nil t nil "--list-dirs"))
      (goto-char (point-min))
      (while (re-search-forward "^\\(agent-ssh-socket\\|agent-socket\\):\\(.*\\)$" nil t)
        (pcase (match-string 1)
          ("agent-ssh-socket" (setenv "SSH_AUTH_SOCK"   (match-string 2)))
          ("agent-socket"     (setenv "GPG_AGENT_INFO" (match-string 2))))))))

(run-with-idle-timer 1 nil #'+gpgconf-set-agent-env)
(setenv "TERM" "xterm-256color")
(setenv "PAGER" "bat -f -pp")
(setenv "DOOMPAGER" "bat -f -pp")

(eval-when-compile
  (require '+transient-macros))

(advice-add 'risky-local-variable-p :override #'ignore)
(setq enable-local-variables :all)
(setq enable-local-eval t)

;; Memoize per buffer: scope.el calls `trusted-content-p' once per macro during elisp
;; fontification, and each call file-truename's the nix-store symlink farms in
;; `trusted-content' (~280ms / 60% of a full config.el fontify, the bulk of scroll stalls).
(defvar-local +trusted-content-p--cache nil
  "Memoized `(SIGNATURE . RESULT)' for `trusted-content-p' in this buffer.")
(defun +trusted-content-p--memoize (orig)
  "Cache `trusted-content-p' (ORIG) per buffer, keyed on the inputs it reads."
  (let ((sig (list untrusted-content trusted-content buffer-file-truename)))
    (if (and +trusted-content-p--cache
             (equal (car +trusted-content-p--cache) sig))
        (cdr +trusted-content-p--cache)
      (let ((result (funcall orig)))
        (setq +trusted-content-p--cache (cons sig result))
        result))))
(when (fboundp 'trusted-content-p)
  (advice-add 'trusted-content-p :around #'+trusted-content-p--memoize))
(setq with-editor-emacsclient-executable (executable-find "emacsclient"))
(setopt forge-database-connector 'sqlite-builtin)

;; set up env vars from encrypted sources
(setq auth-sources (list (format "%s/.authinfo.gpg" (getenv "HOME")) (format "%s/.authinfo" (getenv "HOME"))))
(load! "auth-source-rbw")
(auth-source-rbw-enable)
(setq epa-file-encrypt-to '("cass@ndra.io"))
(setq epa-file-select-keys nil)
(add-hook 'emacs-startup-hook
          (lambda () (push epa-file-handler file-name-handler-alist)))
(after! grip-mode
  (let ((credential (auth-source-user-and-password "git.drwholdings.com")))
    (setq grip-github-api-url "https://git.drwholdings.com/api/v3"
          grip-github-user (car credential)
          grip-github-password (cadr credential)
          grip-update-after-change nil)))
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

(remove-hook! 'doom-first-buffer-hook #'global-hl-line-mode)
(add-hook! global-hl-line-modes (hl-line-mode +1))

;; evil
(use-package! evil-collection
  :after evil
  :init
  (setq evil-collection-setup-minibuffer t))

;; UI
(defvar-local +doom-modeline--git-worktree-cache 'unset)
(defvar +doom-modeline-scroll-commands
  '(ultra-scroll ultra-scroll-up ultra-scroll-down
    mwheel-scroll pixel-scroll-precision pixel-scroll-precision-scroll-down
    scroll-up-command scroll-down-command
    evil-scroll-up evil-scroll-down evil-scroll-line-up evil-scroll-line-down
    evil-scroll-page-up evil-scroll-page-down)
  "Commands during which the modeline is served from cache, not recomputed.")
(defvar-local +doom-modeline--scroll-cache nil
  "Cached `doom-modeline-format--main' output, reused for the duration of a scroll.")
(defun +doom-modeline--cache-during-scroll (orig &rest args)
  "Serve a `format-mode-line'-flattened cached modeline (ORIG) during scroll.
ORIG returns a construct of live `:eval' segment forms; flattening to a string
is what stops redisplay re-running every segment on each `posn-at-point' call
mid-scroll."
  (if (memq this-command +doom-modeline-scroll-commands)
      (or +doom-modeline--scroll-cache
          (setq +doom-modeline--scroll-cache (format-mode-line (apply orig args))))
    (setq +doom-modeline--scroll-cache nil)
    (apply orig args)))
(use-package! doom-modeline
  :config
  (setq doom-modeline-lsp nil
        doom-modeline-hud t)
  (advice-add 'doom-modeline--in-git-worktree-p :around
              (lambda (orig)
                (if (eq +doom-modeline--git-worktree-cache 'unset)
                    (setq +doom-modeline--git-worktree-cache (funcall orig))
                  +doom-modeline--git-worktree-cache)))
  ;; ultra-scroll reformats the modeline via posn-at-point every step; cache a flattened string during scroll so segments don't re-run.
  (advice-add 'doom-modeline-format--main :around #'+doom-modeline--cache-during-scroll))

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
  :defer t
  :commands eglot eglot-ensure lsp!
  :init
  (remove-hook! '(eglot-managed-mode-hook doom-modeline-mode-hook) #'doom-modeline-override-eglot)
  :config
  (load! "+eglot")

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
  (setq eglot-semantic-token-modifiers (remove "documentation" (remove "defaultLibrary" eglot-semantic-token-modifiers)))
  (set-popup-rule! "^\\*eglot-help" :size 0.5 :quit t :select t :side 'right))

(custom-set-faces!
  '(font-lock-punctuation-face :inherit font-lock-keyword-face))

(after! rust-mode
  (custom-set-faces!
    '(rust-ampersand-face :inherit font-lock-keyword-face)
    '(rust-builtin-formatting-macro :inherit font-lock-preprocessor-face)))

(after! haskell-mode
  (custom-set-faces!
    '(haskell-operator-face :inherit haskell-keyword-face)
    '(haskell-definition-face :inherit haskell-keyword-face)))

(after! eglot
  (custom-set-faces!
    '(my-font-lock-variable-use-face :foreground "#fda135")
    '(my-font-lock-type-face :foreground "#5c9cff")
    '(my-font-lock-type-parameter-face :foreground "#66d9ef")
    '(my-font-lock-constant-face :inherit font-lock-property-use-face)
    '(eglot-semantic-namespace :foreground "#fb3d81")
    '(eglot-semantic-type :inherit my-font-lock-type-face)
    '(eglot-semantic-class :inherit elisp-symbol-role)
    '(eglot-semantic-enum :inherit elisp-major-mode-name)
    '(eglot-semantic-enumMember :inherit my-font-lock-constant-face :slant italic)
    '(eglot-semantic-interface :inherit elisp-symbol-role)
    '(eglot-semantic-struct :inherit elisp-major-mode-name)
    '(eglot-semantic-typeParameter :inherit my-font-lock-type-parameter-face :slant italic)
    '(eglot-semantic-lifetime :inherit font-lock-preprocessor-face :slant italic)
    '(eglot-semantic-parameter :inherit my-font-lock-variable-use-face :slant italic)
    '(eglot-semantic-variable :inherit font-lock-variable-name-face :slant italic)
    '(eglot-semantic-property :inherit font-lock-property-use-face :slant italic)
    '(eglot-semantic-event :inherit my-font-lock-type-parameter-face)
    '(eglot-semantic-function :inherit font-lock-function-name-face :slant italic)
    '(eglot-semantic-method :inherit font-lock-function-name-face :slant italic)
    '(eglot-semantic-macro :inherit font-lock-preprocessor-face :slant italic)
    '(eglot-semantic-generic :inherit font-lock-variable-name-face :slant italic)
    '(eglot-semantic-number :inherit my-font-lock-constant-face)
    '(eglot-semantic-regexp :inherit font-lock-preprocessor-face)
    '(eglot-semantic-operator :inherit font-lock-keyword-face)
    '(eglot-semantic-const :inherit my-font-lock-constant-face)
    '(eglot-semantic-constant :inherit my-font-lock-constant-face)
    '(eglot-semantic-declaration :underline t :slant normal)
    '(eglot-semantic-definition :underline t :slant normal)
    '(eglot-semantic-static :overline t :slant normal)
    '(eglot-semantic-modification :underline t :slant normal)))

(set-popup-rule! ".*doom eval.*" :action '(display-buffer-no-window))

(use-package! eglot-hover
  :defer t
  :hook '((eglot-managed-mode . eglot-hover-mode)))

;; KEYBINDINGS
(setq mouse-wheel-tilt-scroll t)
(map! :g "<mouse-6>" (lambda () (interactive) (scroll-right 1))
      :g "<mouse-7>" (lambda () (interactive) (scroll-left 1)))
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
(map! :map prog-mode-map :nvi
      "<tab>" #'indent-for-tab-command
      "TAB" #'indent-for-tab-command)

(map! :mode eshell-mode
      :i
      "<tab>" #'completion-at-point
      "TAB" #'completion-at-point)
;; History navigation belongs on the line-mode map -- in semi-char-mode
;; the arrows must be forwarded to the underlying TUI program via
;; `eat-self-input', which is the default in `eat-semi-char-mode-map'.
(map! :map eat-line-mode-map
      :ni
      "<up>" #'eat-line-previous-input
      "<down>" #'eat-line-next-input
      "<tab>" #'completion-at-point
      "TAB" #'completion-at-point
      "C-k" #'eat-previous-shell-prompt
      "C-j" #'eat-next-shell-prompt
      "C-r" #'consult-history)
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
      "9" #'winum-select-window-9)
(map! :nv "J" #'+lookup/definition)
(map! :nv "C-J" #'xref-go-back)
(map! :nv "C-K" #'+lookup/references)
(map! :nv "g b" #'xref-go-back)
(map! :nv "g B" #'xref-go-forward)
(map! :nv "g D" #'+lookup/references)
(map! :nv "g d" #'+lookup/definition)
(map! :nv "g i" #'+lookup/implementations)
(map! :nv "H" #'treemacs)
(map! :nv "L" #'eglot-show-call-hierarchy)
(map! :n  "V" #'evil-visual-line)
(map! :nv "g l" #'consult-line)
(map! :map transient-map
      "<escape>" #'transient-quit-one)
(map! :map magit-mode-map :n "e" #'magit-ediff)

(defun truncated-lines-p ()
  "Non-nil if any line is longer than `window-width' + `window-hscroll'.

Returns t if any line exceeds the right border of the window.
Used for stopping scroll from going beyond the longest line.
Based on `so-long-detected-long-line-p'."
  (save-excursion
    (goto-char (point-min))
    (let* ((window-width
            ;; This computes a more accurate width rather than `window-width', and
            ;; respects `text-scale-mode' font width.
            (/ (window-body-width nil t) (window-font-width)))
           (hscroll-offset
            ;; `window-hscroll' returns columns that are not affected by
            ;; `text-scale-mode'.  Because of that, we have to recompute the correct
            ;; `window-hscroll' by multiplying it with a non-scaled value and
            ;; dividing it with a scaled width value, rounding it to the upper
            ;; boundary.  Since there's no way to get unscaled value, we have to get
            ;; a width of a face that is not scaled by `text-scale-mode', such as
            ;; `window-divider' face.
            (ceiling (/ (* (window-hscroll) (window-font-width nil 'window-divider))
                        (float (window-font-width)))))
           (line-number-width
            ;; Compensate for line number width.  Add support for
            ;; other modes if you use any, like `linum-mode'.
            (if (bound-and-true-p display-line-numbers-mode)
                (- display-line-numbers-width)
              0))
           (threshold (+ window-width hscroll-offset line-number-width
                         -2))) ; -2 to compensate rounding during calculation
      (catch 'excessive
        (while (not (eobp))
          (let ((start (point)))
            (save-restriction
              (narrow-to-region start (min (+ start 1 threshold)
                                           (point-max)))
              (forward-line 1))
            (unless (or (bolp)
                        (and (eobp) (<= (- (point) start)
                                        threshold)))
              (throw 'excessive t))))))))

(define-advice scroll-left (:before-while (&rest _) prevent-overscroll)
  (and truncate-lines
       (not (memq major-mode '(vterm-mode term-mode)))
       (truncated-lines-p)))
(add-hook! 'doom-first-buffer-hook #'horizontal-scroll-bar-mode)

;; TRANSIENT STATES
(use-package! hercules :defer t)

(general-def
  :prefix-map 'custom-paste-map
  "C-j" #'evil-paste-pop-next
  "C-k" #'evil-paste-pop)

(defun +hercules--paste-setup (&rest _)
  "Load hercules + register paste transient on first paste, then self-remove."
  (require 'hercules)
  (hercules-def
   :show-funs '(evil-paste-after evil-paste-before)
   :keymap 'custom-paste-map
   :transient t)
  (advice-remove 'evil-paste-after  #'+hercules--paste-setup)
  (advice-remove 'evil-paste-before #'+hercules--paste-setup))

(advice-add 'evil-paste-after  :before #'+hercules--paste-setup)
(advice-add 'evil-paste-before :before #'+hercules--paste-setup)

(use-package! corfu
  :init
  (setq
   +corfu-want-minibuffer-completion 'aggressive
   +corfu-want-tab-prefer-expand-snippets nil
   +corfu-want-tab-prefer-navigating-snippets nil
   shell-file-name-chars "[]~/A-Za-z0-9+@:_.$#%,={}- "
   shell-file-name-quote-list '(?$ ?\* ?\! ?\" ?\'))

  :config
  (setopt corfu-auto-prefix 0
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
  (setopt corfu-auto-prefix 0)
  ;; The default regex `"delete-backward-char\\'"' requires the command name
  ;; to END with that string, so evil's `evil-delete-backward-char-and-join'
  ;; (and other delete-* variants) don't trigger auto re-completion. Use an
  ;; unanchored pattern that matches anywhere in the command name.
  (add-to-list 'corfu-auto-commands "delete-backward-char"))

(use-package! orderless
  :config
  (setq +vertico-company-completion-styles '(orderless))
  (setq completion-styles '(orderless basic)
        orderless-matching-styles '(orderless-literal-prefix orderless-flex orderless-regexp)))

(after! evil-snipe
  (setopt evil-snipe-scope 'visible)
  (setopt evil-snipe-repeat-scope 'whole-visible)
  (setopt evil-snipe-spillover-scope 'whole-visible))

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
  :after vertico)

;; configure evil
;; make evil-search-word look for symbol rather than word boundaries
(defalias #'forward-evil-word #'forward-evil-symbol)
(setopt evil-symbol-word-search t)

;; don't substitute globally by default
(setopt evil-ex-substitute-global nil)

;; set up smartparens
(after! smartparens
  (show-smartparens-global-mode +1))

;; magit
(after! magit
  (require 'magit-ediff)
  (load! "+multi-file-ediff")
  (multi-file-ediff-magit-setup)


  (transient-append-suffix 'magit-ediff "E"
    '("e" "Dwim" magit-ediff-dwim))
  (transient-remove-suffix 'magit-ediff "E")

  (add-hook! 'multi-file-ediff-after-worktree-hook
    (when (and (fboundp 'envrc-allow)
               (file-exists-p ".envrc"))
      (envrc-allow))))

(use-package! forge
  :after magit
  :config
  (push '("git.drwholdings.com"               ; GITHOST
          "git.drwholdings.com/api/v3"        ; APIHOST
          "git.drwholdings.com"               ; WEBHOST and INSTANCE-ID
          forge-github-repository)            ; CLASS
        forge-alist))
(use-package! code-review
  :after forge
  :config
  (define-advice code-review-forge-pr-at-point
      (:before (&rest _) host-from-forge)
    "Configure code-review GitHub host vars from the forge repo at point."
    (require 'forge)
    (when-let* ((pullreq (or (forge-pullreq-at-point) (forge-current-topic)))
                (repo (forge-get-repository pullreq))
                ((forge-github-repository-p repo))
                (apihost (oref repo apihost)))
      (setq code-review-github-host apihost
            code-review-github-graphql-host
            (if (string-suffix-p "/api/v3" apihost)
                (substring apihost 0 (- (length apihost) 3))  ; "/api/v3" → "/api"
              apihost))))

  ;; ghub renamed `ghub-graphql' → `ghub-query' and changed the callback
  ;; payload from ((data ...)) to (data . X). Restore the old shape.
  (require 'ghub-graphql)
  (defun ghub-graphql--wrap-callback (cb data &rest rest)
    (apply cb (and data (list data)) rest))
  (defun ghub-graphql (query &optional variables &rest args)
    "Compat shim around `ghub-query' for code-review."
    (let ((cb (plist-get args :callback)))
      (when cb
        (setq args (plist-put (copy-sequence args)
                              :callback
                              (apply-partially
                               #'ghub-graphql--wrap-callback cb)))))
    (apply #'ghub-query query variables args))

  (define-keys-and-transient! code-review-mode-map +code-review-menu
                              "Code Review commands."
                              :block "Review"
                              :desc "Approve"                     :n "a"     #'code-review-submit-approve
                              :desc "Request changes"             :n "r"     #'code-review-submit-request-changes
                              :desc "Comment review"              :n "c"     #'code-review-submit-comments
                              :desc "LGTM"                        :n "l"     #'code-review-submit-lgtm
                              :desc "ediff"                       :n "e"     #'multi-file-ediff-code-review

                              :block "Comments"
                              :desc "Add/edit at point"           :n "C"     #'code-review-comment-add-or-edit
                              :desc "Code suggestion"             :n "S"     #'code-review-comment-code-suggestion
                              :desc "Submit replies only"         :n "P"     #'code-review-submit-only-replies
                              :desc "Single diff comment"         :n "d"     #'code-review-submit-single-diff-comment-at-point
                              :desc "Next comment"                :n "C-n"   #'code-review-comment-jump-next
                              :desc "Prev comment"                :n "C-p"   #'code-review-comment-jump-previous

                              :block "Merge"
                              :desc "Merge"                       :n "m m"   #'code-review-merge-merge
                              :desc "Merge rebase"                :n "m r"   #'code-review-merge-rebase
                              :desc "Merge squash"                :n "m s"   #'code-review-merge-squash

                              :row
                              :block "Set"
                              :desc "Feedback"                    :n "s f"   #'code-review-set-feedback
                              :desc "Reviewers"                   :n "s r"   #'code-review-request-reviews
                              :desc "Assignee"                    :n "s a"   #'code-review-set-assignee
                              :desc "Self-assign"                 :n "s y"   #'code-review-set-yourself-assignee
                              :desc "Milestone"                   :n "s m"   #'code-review-set-milestone
                              :desc "Labels"                      :n "s l"   #'code-review-set-label
                              :desc "Title"                       :n "s t"   #'code-review-set-title
                              :desc "Description"                 :n "s d"   #'code-review-set-description

                              :block "Toggle"
                              :desc "All comments"                :n "t a"   #'code-review-toggle-display-all-comments
                              :desc "Diff comments"               :n "t d"   #'code-review-toggle-display-diff-comments
                              :desc "Top-level comments"          :n "t t"   #'code-review-toggle-display-top-level-comments

                              :block "Buffer"
                              :desc "Reload"                      :n "G"     #'code-review-reload)
  (map! :map code-review-mode-map
        :n "?" #'+code-review-menu))

(use-package! terraform-mode
  :defer t
  :init
  (setq terraform-command "tofu"))

(use-package! rustic
  :defer t
  :config
  (setq rustic-format-on-save t))

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

(use-package! nix-ts-mode
  :defer t
  :mode "\\.nix\\'"
  :config
  (sp-with-modes '(nix-mode nix-ts-mode)
    (sp-local-pair "''" "''")))

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
  (set-eglot-client! '(haskell-mode haskell-ts-mode) '("haskell-language-server-wrapper" "-d" "lsp") "haskell-language-server"))

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
  :defer t
  :commands yaml-ts-mode
  :mode "\\.yaml\\(\\.j2\\)?\\'")

(defun helm-ts-mode--full-range-update (orig-fun &optional beg end)
  "In helm-ts-mode, always update ranges for the full buffer.
JIT font-lock calls `treesit-update-ranges' per-chunk, which gives the YAML
parser incomplete ranges and produces broken parses.  Full-buffer range
computation ensures the YAML parser always sees all text regions."
  (if (eq major-mode 'helm-ts-mode)
      (funcall orig-fun (point-min) (point-max))
    (funcall orig-fun beg end)))
(advice-add 'treesit-update-ranges :around #'helm-ts-mode--full-range-update)

(define-derived-mode helm-ts-mode gotmpl-yaml-ts-mode "Helm"
  "Major mode for editing kubernetes helm templates.
Uses tree-sitter with gotmpl as the host language and YAML injected into
text regions between template blocks."
  ;; Run yaml rules first, then gotmpl with :override t so template faces
  ;; win over yaml string face inside block scalars (config: |)
  (setq-local treesit-font-lock-settings
              (let ((gotmpl (mapcar (lambda (s)
                                      (let ((copy (copy-sequence s)))
                                        (setf (nth 3 copy) t)
                                        copy))
                                    (seq-filter (lambda (s) (eq (nth 5 s) 'gotmpl)) treesit-font-lock-settings)))
                    (yaml (seq-filter (lambda (s) (eq (nth 5 s) 'yaml)) treesit-font-lock-settings)))
                (append yaml gotmpl)))
  ;; Disable yaml error feature — yaml fragments between templates may
  ;; produce ERROR roots, but child nodes still highlight correctly
  (treesit-font-lock-recompute-features nil '(error))
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+ *"))
(add-hook! helm-ts-mode #'lsp!)

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

(defun eshell-find-single-file (file-name)
  (let ((file-writeable? (file-writable-p file-name)))
    (if file-writeable?
        (find-file file-name)
      (doom/sudo-find-file file-name))))
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

(setq sendmail-program (executable-find "msmtp")
      send-mail-function #'smtpmail-send-it
      message-sendmail-f-is-evil t
      message-sendmail-extra-arguments '("--read-envelope-from")
      message-send-mail-function #'message-send-mail-with-sendmail
      message-kill-buffer-on-exit t)

(use-package! notmuch
  :commands =notmuch notmuch
  :defer t
  :config
  (set-popup-rule! "^\\*notmuch-hello" :ignore t)
  (setq +notmuch-sync-backend 'mbsync
        +notmuch-mail-folder "~/.local/share/maildir"))

(use-package! notmuch-multi
  :after notmuch
  :config
  (load! "+notmuch.el"))

(use-package! khalel
  :defer t
  :commands khalel-import-events khalel-run-vdirsyncer khalel-edit-calendar-event
  :config
  (load! "+khalel"))

(defun +khalel-import-events-per-calendar ()
  "Per-calendar khalel import (lazy-loaded via `+khalel.el')."
  (interactive)
  (require 'khalel)
  (call-interactively '+khalel-import-events-per-calendar))

(defun +calfw-multi-calendar ()
  "Open calfw with per-calendar color-coded sources (lazy-loaded)."
  (interactive)
  (load! "+calfw-khal")
  (call-interactively '+calfw-multi-calendar))

(map! :leader
      "o c" #'+calfw-multi-calendar
      "o k" #'+khalel-import-events-per-calendar
      "o K" #'khalel-run-vdirsyncer)

;; calfw ships with j/k inverted from vim convention
(map! :after calfw :map calfw-calendar-mode-map
      "j" #'calfw-navi-next-week-command
      "k" #'calfw-navi-previous-week-command
      :leader :n "g r" #'calfw-refresh-calendar-buffer)
;; revert arrow keys to moving point and hjkl to moving between days
(map! :after calfw :map calfw-details-mode-map
      "<left>"  #'left-char
      "<right>" #'right-char
      "h"       #'calfw-details-navi-prev-command
      "l"       #'calfw-details-navi-next-command
      "j"       #'calfw-details-navi-next-item-command
      "k"       #'calfw-details-navi-prev-item-command)

(add-hook! eshell-mode #'eat-eshell-mode)
(add-hook! eshell-mode #'eat-eshell-visual-command-mode)

(use-package! protobuf-mode
  :defer t
  :mode "\\.proto\\'")

(defvar +eglot-after-envrc-hook '())
(defvar-local +eglot-after-envrc-run? nil)
(use-package! envrc
  :defer t
  :hook '((doom-first-input . envrc-global-mode))
  :init
  (setq envrc-async-processing t)
  :config
  (defun +eglot--envrc-settled-p ()
    "Non-nil when no envrc process is running for the current buffer's
env-dir or any ancestor directory of it."
    (let ((dir (ignore-errors (envrc--find-env-dir))))
      (not (and dir
                (seq-some (lambda (proc-dir)
                            (file-in-directory-p dir proc-dir))
                          (hash-table-keys envrc--processes))))))

  (defun +eglot-ensure-connected (&optional tries)
    "Start eglot, or reconnect it, once direnv has fully settled.
The `envrc--status' watcher fires the moment status becomes \\='on, which
is mid-`envrc--apply' -- before `exec-path'/`process-environment' are
installed and before the direnv process leaves `envrc--processes'.  Defer
via a timer and poll until no envrc process remains for this dir or any
parent dir, so eglot launches with the direnv-provided server."
    (let ((tries (or tries 100))
          (buf (current-buffer)))
      (cond
       ((+eglot--envrc-settled-p)
        (when (eglot--lookup-mode major-mode)
          (if (eglot-current-server)
              (eglot-reconnect (eglot-current-server))
            (condition-case-unless-debug oops
                (apply #'eglot--connect (eglot--guess-contact))
              (error (eglot--warn (error-message-string oops)))))))
       ((> tries 0)
        (run-at-time 0.1 nil
                     (lambda ()
                       (when (buffer-live-p buf)
                         (with-current-buffer buf
                           (+eglot-ensure-connected (1- tries)))))))
       (t (message "envrc: gave up waiting for direnv in %s" (buffer-name buf))))))

  (defun +envrc-status-watcher (symbol newval operation where)
    (when (and (equal symbol 'envrc--status)
               (eq operation 'set)
               (bufferp where)
               (buffer-file-name where) ;; only try to start eglot this way in file-visiting buffers
               (equal newval 'on))
      (with-current-buffer where
        (require 'eglot)
        (when (and (eglot--lookup-mode major-mode)
                   (not +eglot-after-envrc-run?))
          ;; make sure hooks are only triggered once
          (setq +eglot-after-envrc-run? t)
          (run-hooks '+eglot-after-envrc-hook)))))
  (add-variable-watcher 'envrc--status #'+envrc-status-watcher)
  (add-hook! '+eglot-after-envrc-hook #'+eglot-ensure-connected))

(use-package! sideline
  :after eglot flymake
  :defer t
  :hook '((flymake-mode . sideline-mode))
  :init
  (setq sideline-force-display-if-exceeds t)

  (advice-add 'sideline--post-command :around
              (lambda (orig)
                (if (memq this-command '(ultra-scroll-up ultra-scroll-down ultra-scroll
                                         mwheel-scroll pixel-scroll-precision
                                         scroll-up-command scroll-down-command))
                    (sideline--delete-ovs)   ; clear overlays, skip the recompute
                  (funcall orig)))))

(use-package! sideline-flymake
  :after sideline flymake
  :defer t
  :init
  (setq sideline-flymake-display-mode 'point))

(use-package! sideline-eglot
  :after sideline eglot
  :defer t
  :init
  (setq sideline-backends-right '(sideline-flymake sideline-eglot)))

(use-package! fish-completion
  :commands global-fish-completion-mode fish-completion-mode
  :defer t
  :config
  (require '+completions)
  (setq fish-completion-fallback-on-bash-p nil)
  (advice-add 'fish-completion--list-completions :override
              #'+fish-completion--list-completions-a))

(use-package! eat
  :defer t
  :commands +eat/here eat
  :init
  (setq process-adaptive-read-buffering t)
  :config
  (load! "+eat")
  (load! "+eat-nushell")
  (setq-hook! 'eat-mode-hook consult-preview-key nil)
  (add-hook! 'eat-mode-hook #'replace-eat-completions)

  (setq nu-executable-path (executable-find "nu"))
  (setq eat-enable-directory-tracking t
        eat-shell (format "%s --config '%s/.config/nushell/emacs-config.nu'" nu-executable-path (getenv "HOME"))
        eat-enable-mouse t
        eat-enable-auto-line-mode t
        eat-enable-kill-from-terminal t
        eat-enable-yank-to-terminal t
        eat-enable-shell-command-history t
        eat-line-input-ring-size 9999999
        eat-term-terminfo-directory (format "%s/.terminfo" (getenv "HOME"))
        eat-enable-directory-tracking t
        eat-enable-shell-prompt-annotation t
        eat-enable-blinking-text nil)
  (after! consult
    (add-to-list 'consult-mode-histories
                 '(eat-mode eat--line-input-ring eat--line-input-ring-index comint-bol)))
  (add-to-list 'eat-semi-char-non-bound-keys [C-r])
  (add-to-list 'eat-message-handler-alist '("open" . +eat/nu-open))
  (add-to-list 'eat-message-handler-alist '("git" . eshell/git))
  (add-to-list 'eat-message-handler-alist '("tee" . +eat/tee))
  (add-to-list 'eat-message-handler-alist '("cat" . +eat/cat))

  (keymap-set eat-mode-map "<insert-state> C-r" #'consult-history)
  (keymap-set eat-mode-map "<normal-state> C-r" #'consult-history)

  (keymap-set eat-mode-map "<insert-state> C-j" #'eat-next-shell-prompt)
  (keymap-set eat-mode-map "<normal-state> C-j" #'eat-next-shell-prompt)

  (keymap-set eat-mode-map "<insert-state> C-k" #'eat-previous-shell-prompt)
  (keymap-set eat-mode-map "<normal-state> C-k" #'eat-previous-shell-prompt)

  (keymap-set eat-mode-map "<insert-state> <tab>" #'completion-at-point)
  (keymap-set eat-mode-map "<normal-state> <tab>" #'completion-at-point)

  (keymap-set eat-line-mode-map "<normal-state> <return>" #'eat-line-send-input)

  (add-hook 'eat--semi-char-mode-hook #'+eat/use-emacs-state-for-tui)
  (add-hook 'eat--char-mode-hook #'+eat/use-emacs-state-for-tui)
  (add-hook 'eat--line-mode-hook #'+eat/restore-evil-state-for-line-mode)
  (add-hook 'eat--line-mode-hook #'+eat/setup-line-mode-nu-highlight)

  (define-key eat-semi-char-mode-map [escape] #'eat-self-input)
  (dolist (i (number-sequence 1 12))
    (let ((base (intern (format "f%d" i))))
      (dolist (mod '("" "C-" "M-" "S-" "C-M-" "C-S-" "M-S-" "C-M-S-"))
        (define-key eat-semi-char-mode-map
                    (vector (intern (concat mod (symbol-name base))))
                    #'eat-self-input))))

  (advice-add 'eat-next-shell-prompt :before #'+eat/skip-current-prompt-end))


(defvar eat-buffer-name "*eat*")
(defun +eat/here (&optional program)
  (interactive)
  (let ((eat-buffer-name
         (format "*eat %s*" (abbreviate-file-name
                             (directory-file-name default-directory)))))
    (eat program "new")))

(map! :leader "RET" #'+eat/here)
(map! :leader "<return>" #'+eat/here)

(use-package! nu-ts-mode
  :commands nu-ts-mode
  :defer t
  :mode "\\.nu\\'"
  :hook '((nu-ts-mode . lsp!)))
(add-hook! nu-ts-mode (+format-with-lsp-mode -1))

(use-package! semel
  :defer t
  :config
  (setq semel-add-help-echo nil)
  :hook '((emacs-lisp-mode . semel-mode)
          (emacs-lisp-mode . cursor-sensor-mode)))

(use-package! rego-mode
  :defer t
  :mode "\\.rego\\'"
  :init
  (set-eglot-client! '(rego-mode) '("regols"))
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

(defun +setup-claude-code-ide ()
  ;; setup the mcp server
  (require 'claude-code-ide)
  (require 'claude-code-ide-mcp-server)
  (setq claude-code-ide-enable-mcp-server t)
  (claude-code-ide-emacs-tools-setup)

  ;; setup the mcp server extensions
  (require 'claude-code-ide-extras-emacs)
  (require 'claude-code-ide-extras-meta)
  (require 'claude-code-ide-extras-projectile)
  (claude-code-ide-extras-meta-setup)
  (claude-code-ide-extras-emacs-setup)
  (claude-code-ide-extras-projectile-setup))

(defun +setup-emacs-mcp ()
  (let* ((project-dir (agent-shell-cwd))
         (session-id (format "agent-shell-%s-%s"
                             (file-name-nondirectory
                              (directory-file-name project-dir))
                             (format-time-string "%Y%m%d-%H%M%S")))
         (buffer (current-buffer)))
    (claude-code-ide-mcp-server-register-session
     session-id project-dir buffer)
    (puthash project-dir session-id claude-code-ide--session-ids)
    (format "http://localhost:%d/mcp/%s"
            (claude-code-ide-mcp-server-ensure-server)
            session-id)))

(use-package! shell-maker
  :defer t)

(use-package! acp
  :defer t)

(use-package! agent-shell
  :defer t
  :commands agent-shell-anthropic-start-claude-code agent-shell
  :config
  (defun +agent-shell-file-completion-at-point ()
    "Like `agent-shell--file-completion-at-point' but only after @."
    (when (save-excursion
            (skip-chars-backward "^ \t\n@")
            (eq (char-before) ?@))
      (agent-shell--file-completion-at-point)))
  (defun +agent-shell-command-completion-at-point ()
    "Like `agent-shell--command-completion-at-point' but only after / at
the start of the line."
    (when (save-excursion
            (skip-chars-backward "^ \t\n/")
            (equal (buffer-substring-no-properties (line-beginning-position) (point)) "/"))
      (agent-shell--command-completion-at-point)))
  (setq-hook! 'agent-shell-mode-hook
    completion-at-point-functions '(+agent-shell-command-completion-at-point +agent-shell-file-completion-at-point))
  (setq agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication
         :api-key (lambda () (identity "unused")))
        agent-shell-anthropic-claude-environment
        (agent-shell-make-environment-variables
         "ANTHROPIC_AUTH_KEY" (auth-source-rbw-get "anthropic-api-key")
         "ANTHROPIC_CUSTOM_HEADERS" (format "x-portkey-api-key: %s\nx-portkey-config: pc-bedroc-55aa53\nx-portkey-metadata: {\"service\": \"claude-code\", \"os\": \"linux\"}" (auth-source-rbw-get "anthropic-api-key"))
         "ANTHROPIC_DEFAULT_OPUS_MODEL" "claude-opus-4-8[1m]"
         "ANTHROPIC_DEFAULT_OPUS_MODE_SUPPORTED_CAPABILITIES" "adaptive_thinking")
        agent-shell-anthropic-claude-acp-command (list (format "%s/.npm-global/bin/claude-agent-acp" (getenv "HOME")))
        agent-shell-anthropic-default-model-id "claude-opus-4-8[1m]"
        agent-shell-display-action
        '((display-buffer-reuse-mode-window display-buffer-in-direction)
          (mode . agent-shell-mode)
          (direction . right)
          (window-width . 0.5))
        agent-shell-mcp-servers
        '(((name . "emacs")
           (type . "http")
           (headers . ())
           (url . (lambda ()
                    (+setup-claude-code-ide)
                    (+setup-emacs-mcp))))
          ((name . "grafana")
           (command . "uvx")
           (args . ("mcp-grafana"))
           (env . (((name . "GRAFANA_URL") (value . "https://grafana.drwholdings.com"))
                   ((name . "GRAFANA_SERVICE_ACCOUNT_TOKEN")
                    (value . (lambda () (auth-source-rbw-get "grafana-svc-token"))))
                   ((name . "GRAFANA_ORG_ID") (value . "7")))))
          ((name . "influxdb")
           (command . (lambda () (format "%s/.npm-global/bin/influxdb-mcp-server" (getenv "HOME")) ))
           (args . ())
           (env . (((name . "INFLUXDB_TOKEN")
                    (value . (lambda () (auth-source-rbw-get "influxdb-token"))))
                   ((name . "INFLUXDB_URL") (value . "http://influxdb-2.production.tito.drw:8086"))
                   ((name . "INFLUXDB_ORG") (value . "TI")))))
          ((name . "up-slack")
           (type . "http")
           (headers . (((name . "x-portkey-api-key") (value . (lambda () (auth-source-rbw-get "anthropic-api-key"))))))
           (url . "https://mcp.ai.drwcloud.com/slack/mcp"))))

  (define-keys-and-transient! agent-shell-mode-map +agent-shell-menu
                              "Agent shell commands."
                              ;;       desc                      prefix           state      key              cmd
                              :block "Navigate"
                              :desc "Next item"                                             "C-j"       #'agent-shell-next-item
                              :desc "Previous item"                                         "C-k"       #'agent-shell-previous-item
                              :desc "Forward block"                                :n       "C-n"       #'agent-shell-ui-forward-block
                              :desc "Backward block"                               :n       "C-p"       #'agent-shell-ui-backward-block
                              :desc "Toggle fragment"                                       "<tab>"     #'agent-shell-ui-toggle-fragment-at-point
                              :desc "Jump to permission"                           :n       "s"         #'agent-shell-jump-to-latest-permission-button-row

                              :block "Compose"
                              :desc "Compose prompt"                                        "C-c C-e"   #'agent-shell-prompt-compose
                              :desc "Search history"                               :i       "C-r"       #'agent-shell-search-history
                              :desc "Send region"              :localleader        :n       "r"         #'agent-shell-send-region
                              :desc "Send file"                :localleader        :n       "f"         #'agent-shell-send-file
                              :desc "Paste image"              :localleader        :n       "p"         #'agent-shell-send-clipboard-image

                              :block "Session"
                              :desc "Cycle mode"                                            "<C-tab>"   #'agent-shell-cycle-session-mode
                              :desc "Set mode"                 :localleader        :n       "M"         #'agent-shell-set-session-mode
                              :desc "Fork session"             :localleader        :n       "y"         #'agent-shell-fork
                              :desc "Restart"                  :localleader        :n       "q"         #'agent-shell-restart
                              :desc "Toggle shell"             :localleader        :n       "o"         #'agent-shell-toggle

                              :row
                              :block "Launch"
                              :desc "Start Claude"             :localleader        :n       "l"         #'agent-shell-anthropic-start-claude-code

                              :block "Debug"
                              :desc "Traffic"                  :localleader        :n       "t"         #'agent-shell-view-traffic
                              :desc "Transcript"               :localleader        :n       "T"         #'agent-shell-open-transcript
                              :desc "Usage"                    :localleader        :n       "u"         #'agent-shell-show-usage)
  (map! :map agent-shell-mode-map :n "?" #'+agent-shell-menu)
  ;; Upgrade SPC l from bootstrap binding to full transient
  (map! :leader "l" #'+agent-shell-menu)

  (load! "agent/+agent-shell-view-on-y")
  (load! "agent/+agent-shell-interrupt-fix")
  (load! "agent/+agent-shell-title-fix")
  (load! "agent/agent-shell-vtable.el")
  (agent-shell-vtable-global-mode)
  (add-hook! 'agent-shell-mode-hook (evil-snipe-local-mode -1))

  ;; fix Opus 4.7 adaptive thinking making it very stupid
  (defun my/claude-acp-session-meta ()
    "Return the _meta alist to inject into Claude Agent session requests."
    '((claudeCode
       . ((options . ((effort . "max")
                      (thinking . ((type . "adaptive")
                                   (display . "summarized")))))))))

  (defun my/claude-acp-inject-session-meta (request)
    "Mutate ACP session REQUEST alist to include Claude _meta.
REQUEST looks like ((:method . STR) (:params . ALIST))."
    (when-let* ((params-cell (assq :params request)))
      (let ((meta-cell (assq '_meta (cdr params-cell)))
            (claude-meta (my/claude-acp-session-meta)))
        (if meta-cell
            (setcdr meta-cell (append (cdr meta-cell) claude-meta))
          (setcdr params-cell
                  (cons (cons '_meta claude-meta) (cdr params-cell))))))
    request)

  (with-eval-after-load 'acp
    (dolist (sym '(acp-make-session-new-request
                   acp-make-session-load-request
                   acp-make-session-resume-request
                   acp-make-session-fork-request))
      (advice-add sym :filter-return #'my/claude-acp-inject-session-meta))))

;; agent-shell's heartbeat rebuilds the header-line ~10x/sec; each rebuild ran
;; `where-is-internal' 3x (a ~5ms keymap search) to recompute keybindings that
;; never change -- ~17ms/beat, a steady CPU drain that makes scrolling janky
;; while the agent is busy. Memoize those lookups (26x faster header update).
(defvar +agent-shell--where-is-cache (make-hash-table :test 'equal)
  "Memoized `where-is-internal' results for agent-shell's header commands.")
(defun +agent-shell--cache-where-is (orig def &optional keymap firstonly &rest args)
  "Cache `where-is-internal' for agent-shell's constant header keybindings."
  (if (and (boundp 'agent-shell-mode-map)
           (eq keymap agent-shell-mode-map)
           (memq def '(agent-shell-set-session-model
                       agent-shell-set-session-mode
                       agent-shell-set-session-thought-level)))
      (let* ((key (list def firstonly))
             (cached (gethash key +agent-shell--where-is-cache :miss)))
        (if (eq cached :miss)
            (puthash key (apply orig def keymap firstonly args)
                     +agent-shell--where-is-cache)
          cached))
    (apply orig def keymap firstonly args)))
(advice-add 'where-is-internal :around #'+agent-shell--cache-where-is)

(use-package! agent-shell-ediff
  :after agent-shell
  :custom
  (agent-shell-ediff-quick-quit t)
  :config
  (agent-shell-ediff-mode 1))

(use-package! agent-shell-dispatch
  :after agent-shell
  :config
  (agent-shell-dispatch-global-mode +1))

;; Bootstrap binding — available before agent-shell loads.
;; Once the package loads, SPC l is upgraded to the full transient menu.
(map! :leader :desc "Start Claude Code" "l l" #'agent-shell-anthropic-start-claude-code)

(use-package! let-completion
  :defer t
  :hook '((emacs-lisp-mode . let-completion-mode)))
