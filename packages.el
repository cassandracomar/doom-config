;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here, run 'doom sync' on
;; the command line, then restart Emacs for the changes to take effect.
;; Alternatively, use M-x doom/reload.
;;
;; WARNING: Disabling core packages listed in ~/.emacs.d/core/packages.el may
;; have nasty side-effects and is not recommended.


;; All of Doom's packages are pinned to a specific commit, and updated from
;; release to release. To un-pin all packages and live on the edge, do:
;(unpin! t)

;; ...but to unpin a single package:
;(unpin! pinned-package)
;; Use it to unpin multiple packages
;(unpin! pinned-package another-pinned-package)


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a particular repo, you'll need to specify
;; a `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, for whatever reason,
;; you can do so here with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

(package! code-review :disable t)
(package! docker-tramp :disable t)
; version bumps
(unpin! lsp-mode)
(package! lsp-mode
  :recipe (:host github :repo "emacs-lsp/lsp-mode") :pin "d230fd1170ead71276bd5b7a2fc5072fea8fc933")
(package! persp-mode :pin "14325c11f7a347717d7c3780f29b24a38c68fbfc")
(package! polymode)

;; new packages
(package! groovy-mode)
(package! fish-completion)
(package! bash-completion)
(package! quark-emacs-tramp :recipe (:host github :repo "PythonNut/quark-emacs" :branch "master"))
(package! su :recipe (:host github :repo "PythonNut/su.el"))
(package! aweshell :recipe (:local-repo "~/src/github.com/manateelazycat/aweshell"))
(package! el-patch)
(package! eshell-git-prompt)
(package! systemd)
(package! hercules)
(package! magit-delta :recipe (:host github :repo "dandavison/magit-delta"))
(package! es-mode)
(package! xterm-color)
(package! direnv)
(package! awscli-capf)
(package! lsp-treemacs)
(package! company-prescient)
(package! lilypond)
(package! ob-lilypond :recipe (:host github :repo "mjago/ob-lilypond"))
(package! jsonnet-mode)
(package! lsp-ivy :pin "9ecf4dd9b1207109802bd1882aa621eb1c385106")
