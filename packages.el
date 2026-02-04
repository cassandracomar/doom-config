;; -*- no-byte-compile: t; lexical-binding: t; -*-
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
;; version bumps
(package! envrc :recipe (:host github :repo "cassandracomar/envrc" :branch "master") :pin "a7c773fac315f640526a6f35cd3c60edaa51b2b4")

;; new packages
(package! fish-completion)
(package! bash-completion)
(package! su :recipe (:host github :repo "PythonNut/su.el") :pin "e097f31b3bbb8581d045d0e684d3f129f90e8085")
(package! eshell-git-prompt)
(package! hercules)
(package! jsonnet-mode)
(package! protobuf-mode)
(package! eat :recipe (:repo "https://codeberg.org/akib/emacs-eat.git") :pin "c8d54d649872bfe7b2b9f49ae5c2addbf12d3b99")
(package! consult-projectile)
(package! rego-mode)
(package! nushell-mode)
(package! nushell-ts-mode)
(package! spacious-padding)
(package! sideline)
(package! sideline-flycheck)
(package! flycheck-projectile)
(package! mermaid-mode)
(package! ob-mermaid)
(package! eglot-hover :recipe (:repo "https://codeberg.org/slotThe/eglot-hover") :pin "44b8c8629a8fb6ef034e9a6b0681ea6db5324924")
(package! elisp-benchmarks)
(package! semel :recipe (:host github :repo "eshelyaron/semel") :pin "649cb73aa8bcc5fe06869fa09e83992639fcf07b")
