;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load in.
;; Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find information about all of Doom's modules
;;      and what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c g k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c g d') on a module to browse its
;;      directory (for easy access to its source code).

;; (defadvice! fixed-doom-module-list (fn &rest args)
;;   :around #'doom-module-list
;;   (nreverse (apply fn args)))

;; (setenv "LSP_USE_PLISTS" "true")
(setq! package-native-compile t)
(setq! native-comp-jit-compilation nil)
;; (setq-default native-comp-driver-options
;;               '("-Wl,-w"
;;                 "-Wl,-L/nix/store/zl3aslw7dhrk6wb5nv960hnc2v27l3j5-libgccjit-14-20241116/lib/gcc/aarch64-apple-darwin/14.2.1"
;;                 "-Wl,-L/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/lib"))
(doom! :input
       ;;chinese
       ;;japanese

       :completion
       ;; company             ; the ultimate code completion backend
       (corfu +icons +orderless)
       ;;helm              ; the *other* search engine for love and life
       ;;ido               ; the other *other* search engine...
       ;; (ivy +prescient +icons)               ; a search engine for love and life
       (vertico +icons)

       :ui
                                        ; deft              ; notational velocity for Emacs
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
                                        ;doom-quit         ; DOOM quit-message prompts when you quit Emacs
       ;;fill-column       ; a `fill-column' indicator
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       indent-guides     ; highlighted indent columns
       modeline          ; snazzy, Atom-inspired modeline, plus API
       nav-flash         ; blink the current line after jumping
       ;;neotree           ; a project drawer, like NERDTree for vim
       ophints           ; highlight the region an operation acts on
       (popup +defaults)   ; tame sudden yet inevitable temporary windows
       (ligatures +iosevka +extra)         ; ligatures or substitute text with pretty symbols
       ;;tabs              ; an tab bar for Emacs
       treemacs          ; a project drawer, like neotree but cooler
       ;; unicode           ; extended unicode support for various languages
       (vc-gutter +pretty)         ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       (window-select +numbers)     ; visually switch windows
       workspaces        ; tab emulation, persistence & separate workspaces
       ;;zen               ; distraction-free coding or writing
       ;; (emoji +ascii +github +unicode)
       minimap
       (smooth-scroll)

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       (format +onsave +lsp)  ; automated prettiness
       ;;god               ; run Emacs commands without modifier keys
       ;;lispy             ; vim for lisp, for people who don't like vim
       multiple-cursors  ; editing in many places at once
       ;;objed             ; text object editing for the innocent
       ;;parinfer          ; turn lisp into python, sort of
       rotate-text       ; cycle region at point between text candidates
       snippets          ; my elves. They type so I don't have to
       word-wrap         ; soft wrapping with language-aware indent

       :emacs
       (dired +icons)             ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       (ibuffer +icons)         ; interactive buffer management
       vc                ; version-control and Emacs, sitting in a tree
       undo

       :term
       eshell            ; a consistent, cross-platform shell (WIP)
       shell             ; a terminal REPL for Emacs
       term              ; terminals in Emacs

       :checkers
       (syntax +childframe +icons)              ; tasing you for every semicolon you forget
                                        ;spell             ; tasing you for misspelling mispelling
       ;; grammar           ; tasing grammar mistake every you make

       :tools
       ansible
       (debugger +lsp)          ; FIXME stepping through code, to help you add bugs
       direnv
       docker
       editorconfig      ; let someone else argue about tabs vs spaces
       ;;ein               ; tame Jupyter notebooks with emacs
       (eval +overlay)     ; run code, run (also, repls)
       (lookup +docsets)              ; navigate your code and its documentation
       ;; (lsp +peek)
       (lsp +peek)
       (magit +forge)    ; a git porcelain for Emacs
       make              ; run make tasks from Emacs
       ;;pass              ; password manager for nerds
       ;;pdf               ; pdf enhancements
       ;;prodigy           ; FIXME managing external services & code builders
       ;;rgb               ; creating color strings
       (terraform +lsp)         ; infrastructure as code
       tmux              ; an API for interacting with tmux
       upload            ; map local to remote projects via ssh/ftp
       tree-sitter

       :os
       macos             ; MacOS-specific commands
       (tty +osc)

       :lang
       ;;agda              ; types of types of types of types...
       ;;assembly          ; assembly for fun or debugging
       (cc +lsp)                ; C/C++/Obj-C madness
       ;;clojure           ; java with a lisp
       common-lisp       ; if you've seen one lisp, you've seen them all
       ;;coq               ; proofs-as-programs
       ;;crystal           ; ruby at the speed of c
       ;;csharp            ; unity, .NET, and mono shenanigans
       data              ; config/data formats
       ;;(dart +flutter)   ; paint ui and not much else
       (elixir +lsp +tree-sitter)            ; erlang done right
       ;;elm               ; care for a cup of TEA?
       emacs-lisp        ; drown in parentheses
       (erlang +lsp +tree-sitter)            ; an elegant language for a more civilized age
       ;;ess               ; emacs speaks statistics
       ;;faust             ; dsp, but you get to keep your soul
       ;;fsharp           ; ML stands for Microsoft's Language
       ;;fstar             ; (dependent) types and (monadic) effects and Z3
       (go +lsp +tree-sitter)                ; the hipster dialect
       (haskell +lsp +tree-sitter)  ; a language that's lazier than I am
       ;;hy                ; readability of scheme w/ speed of python
       ;;idris             ;
       ;;(java +meghanada) ; the poster child for carpal tunnel syndrome
       (javascript +lsp +tree-sitter)        ; all(hope(abandon(ye(who(enter(here))))))
       ;;julia             ; a better, faster MATLAB
       ;;kotlin            ; a better, slicker Java(Script)
       (latex +lsp +cdlatex)             ; writing papers in Emacs has never been so fun
       ;; lean
       ;;factor
       ;;ledger            ; an accounting system in Emacs
       lua               ; one-based indices? one-based indices
       (markdown +grip)    ; writing docs for people to ignore
       ;;nim               ; python + lisp at the speed of c
       (nix +lsp)               ; I hereby declare "nix geht mehr!"
       ;;ocaml             ; an objective camel
       ;; (org +dragndrop +gnuplot +hugo +journal +pandoc +pretty)               ; organize your plain life in plain text
       ;;perl              ; write code no one else can comprehend
       ;;php               ; perl's insecure younger brother
       ;;plantuml          ; diagrams for confusing people more
       ;;purescript        ; javascript, but functional
       (python +lsp +poetry +tree-sitter +pyright)            ; beautiful is better than ugly
       ;;qt                ; the 'cutest' gui framework ever
       ;;racket            ; a DSL for DSLs
       ;;rest              ; Emacs as a REST client
       ;;rst               ; ReST in peace
       (ruby +rails +lsp)     ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       (rust +lsp +tree-sitter)              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       ;;scala             ; java, but good
       ;;scheme            ; a fully conniving family of lisps
       sh                ; she sells {ba,z,fi}sh shells on the C xor
       ;;sml
       ;;solidity          ; do you need a blockchain? No.
       ;;swift             ; who asked for emoji variables?
       ;;terra             ; Earth and Moon in alignment for performance.
       ;;web               ; the tubes
       (yaml +lsp +tree-sitter)

       :email
       (mu4e +gmail +org)
       ;;notmuch
       ;;(wanderlust +gmail)

       :app
       calendar
       ;;irc               ; how neckbeards socialize
       ;;(rss +org)        ; emacs as an RSS reader
       ;;twitter           ; twitter client https://twitter.com/vnought

       :config
       ;;literate
       (default +bindings +smartparens))


;; (setq native-comp-jit-compilation nil)
;; (setq native-comp-jit-compilation-deny-list nil)
;; (setq inhibit-automatic-native-compilation nil)
;; (after! (doom-packages straight)
;;   (setq straight--native-comp-available t))
