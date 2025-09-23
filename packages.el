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
;; (unpin! haskell-mode)
;; (package! lsp-mode
;;   :recipe (:host github :repo "cassandracomar/lsp-mode" :branch "master") :pin "129f5375f617e99389683acda7095e6c9af82840")
;; (unpin! lsp-haskell)
;; (unpin! envrc)
(package! envrc :recipe (:host github :repo "cassandracomar/envrc" :branch "master") :pin "548b48717b5b3642bfc2b7b1376f879b1db1dd9f")
(package! persp-mode :pin "14325c11f7a347717d7c3780f29b24a38c68fbfc")
(package! polymode)
;; (package! haskell-ts-mode :recipe (:host codeberg :repo "pranshu/haskell-ts-mode" :branch "main"))
;; (package! haskell-mode :disable t)

;; (unpin! evil-terminal-cursor-changer)
;; (unpin! nerd-icons-completion)
;; (unpin! nerd-icons)
;; (unpin! nerd-icons-corfu)
;; (unpin! marginalia)
;; (unpin! forge)
;; (unpin! magit)
;; (unpin! transient)
;; (unpin! org)
;; (unpin! corfu)

;; new packages
(package! consult-hoogle :recipe (:repo "https://codeberg.org/rahguzar/consult-hoogle" :branch "main") :pin "384959016022d071464dc6e611e4fcded562834e")
(package! groovy-mode)
(package! fish-completion)
(package! bash-completion)
(package! su :recipe (:host github :repo "PythonNut/su.el") :pin "e097f31b3bbb8581d045d0e684d3f129f90e8085")
(package! el-patch)
(package! eshell-git-prompt)
(package! systemd)
(package! hercules)
(package! es-mode)
(package! xterm-color)
(package! awscli-capf)
;; (package! lsp-treemacs)
(package! jsonnet-mode)
;; (package! lsp-ivy :pin "9ecf4dd9b1207109802bd1882aa621eb1c385106")
(package! telephone-line)
;; (package! jsonnet-language-server :recipe (:host github :repo "grafana/jsonnet-language-server" :files ("editor/emacs/*.el") :includes jsonnet-mode) :pin "546040b5f00a0cc09888aaa08257d7d3c496d7b3")
(package! protobuf-mode :recipe (:host github :repo "protocolbuffers/protobuf" :files ("editors/protobuf-mode.el")) :pin "50e90bb84add455170e315e9a7f075274b8e0ada")
(package! multi-run :recipe (:host github :repo "sagarjha/multi-run") :pin "13d4d923535b5e8482b13ff76185203075fb26a3")
(package! eat :recipe (:repo "https://codeberg.org/akib/emacs-eat.git") :pin "c8d54d649872bfe7b2b9f49ae5c2addbf12d3b99")
(package! lean4-mode :recipe
  (:host github
   :repo "leanprover/lean4-mode"
   :files ("*.el" "data"))
  :pin "1388f9d1429e38a39ab913c6daae55f6ce799479")
;; (package! org-fragtog)
(package! jq-mode :recipe (:host github :repo "ljos/jq-mode") :pin "1f4bf0955fc9f7f03d8b26dc1acf6b68067a57cc")
(package! ligature :pin "6ac1634612dbd42f7eb81ecaf022bd239aabb954")
(package! consult-projectile :recipe (:type git :host gitlab :repo "OlMon/consult-projectile" :branch "master") :pin "400439c56d17bca7888f7d143d8a11f84900a406")
(package! sublimity)
(package! rego-mode)
(package! nushell-mode :recipe (:host github :repo "mrkkrp/nushell-mode") :pin "3499f64e4f750da62c20f513ff1fbdf87bb1eea9")
(package! nushell-ts-mode :recipe (:host github :repo "herbertjones/nushell-ts-mode") :pin "e07ecc59762fab8d5fa35bc6d3f522f74e580a2f")
(package! shx :recipe (:source melpa))
(package! spacious-padding)
(package! sideline)
(package! sideline-flymake)
(package! eglot :recipe (:host github :repo "cassandracomar/eglot") :pin "b16e1cb02f6c6168b22301beeb84e4c19f508abb")
(package! semel :recipe (:host github :repo "eshelyaron/semel") :pin "d41d37c21efe1ba3a906ebca2d5611fcb5e84f5c")
(package! notmuch-notify)
