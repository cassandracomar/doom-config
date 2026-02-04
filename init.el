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

(setq! package-native-compile t)
(setq! native-comp-jit-compilation nil)
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
       ;; workspaces        ; tab emulation, persistence & separate workspaces
       ;;zen               ; distraction-free coding or writing
       ;; (emoji +ascii +github +unicode)
       minimap
       smooth-scroll

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       ;; fold              ; (nigh) universal code folding
       (format +onsave +lsp)  ; automated prettiness
       ;;god               ; run Emacs commands without modifier keys
       ;;lispy             ; vim for lisp, for people who don't like vim
       ;; multiple-cursors  ; editing in many places at once
       ;;objed             ; text object editing for the innocent
       ;;parinfer          ; turn lisp into python, sort of
       ;; rotate-text       ; cycle region at point between text candidates
       ;; snippets          ; my elves. They type so I don't have to
       word-wrap         ; soft wrapping with language-aware indent

       :emacs
       (dired +icons)             ; making dired pretty [functional]
       ;; electric          ; smarter, keyword-based electric-indent
       ;; (ibuffer +icons)         ; interactive buffer management
       ;; vc                ; version-control and Emacs, sitting in a tree
       (undo +tree)

       :term
       eshell            ; a consistent, cross-platform shell (WIP)
       ;; shell             ; a terminal REPL for Emacs
       ;; term              ; terminals in Emacs

       :checkers
       (syntax +childframe +icons +flymake)              ; tasing you for every semicolon you forget
                                        ;spell             ; tasing you for misspelling mispelling
       ;; grammar           ; tasing grammar mistake every you make

       :tools
       ansible
       ;; (debugger +lsp)          ; FIXME stepping through code, to help you add bugs
       direnv
       docker
       editorconfig      ; let someone else argue about tabs vs spaces
       ;;ein               ; tame Jupyter notebooks with emacs
       (eval +overlay)     ; run code, run (also, repls)
       (lookup +docsets)              ; navigate your code and its documentation
       (lsp +eglot +booster)
       (magit +forge)    ; a git porcelain for Emacs
       ;; make              ; run make tasks from Emacs
       ;;pass              ; password manager for nerds
       ;;pdf               ; pdf enhancements
       (terraform +lsp)         ; infrastructure as code
       ;; tmux              ; an API for interacting with tmux
       ;; upload            ; map local to remote projects via ssh/ftp
       tree-sitter

       :os
       ;; macos             ; MacOS-specific commands
       (tty +osc)

       :lang
       ;;agda              ; types of types of types of types...
       ;;assembly          ; assembly for fun or debugging
       (cc +lsp +tree-sitter)                ; C/C++/Obj-C madness
       (clojure +lsp +tree-sitter)           ; java with a lisp
       ;; common-lisp       ; if you've seen one lisp, you've seen them all
       ;;coq               ; proofs-as-programs
       ;;crystal           ; ruby at the speed of c
       ;;csharp            ; unity, .NET, and mono shenanigans
       data              ; config/data formats
       ;;(dart +flutter)   ; paint ui and not much else
       ;; (elixir +lsp)            ; erlang done right
       ;;elm               ; care for a cup of TEA?
       emacs-lisp        ; drown in parentheses
       ;; (erlang +lsp)            ; an elegant language for a more civilized age
       ;;ess               ; emacs speaks statistics
       ;;faust             ; dsp, but you get to keep your soul
       ;;fsharp           ; ML stands for Microsoft's Language
       ;;fstar             ; (dependent) types and (monadic) effects and Z3
       (go +lsp +tree-sitter)                ; the hipster dialect
       (haskell +lsp)  ; a language that's lazier than I am
       ;;hy                ; readability of scheme w/ speed of python
       ;;idris             ;
       ;;(java +meghanada) ; the poster child for carpal tunnel syndrome
       (javascript +lsp +tree-sitter)        ; all(hope(abandon(ye(who(enter(here))))))
       ;;julia             ; a better, faster MATLAB
       ;;kotlin            ; a better, slicker Java(Script)
       ;; (latex +lsp +cdlatex)             ; writing papers in Emacs has never been so fun
       ;; lean
       ;;factor
       ;;ledger            ; an accounting system in Emacs
       lua               ; one-based indices? one-based indices
       (markdown +grip)    ; writing docs for people to ignore
       ;;nim               ; python + lisp at the speed of c
       (nix +lsp +tree-sitter)               ; I hereby declare "nix geht mehr!"
       ;;ocaml             ; an objective camel
       (org +pandoc +pretty)               ; organize your plain life in plain text
       ;;perl              ; write code no one else can comprehend
       ;;php               ; perl's insecure younger brother
       ;;plantuml          ; diagrams for confusing people more
       ;;purescript        ; javascript, but functional
       (python +lsp +poetry +pyright +tree-sitter)            ; beautiful is better than ugly
       ;;qt                ; the 'cutest' gui framework ever
       ;;racket            ; a DSL for DSLs
       ;;rest              ; Emacs as a REST client
       ;;rst               ; ReST in peace
       (ruby +rails +lsp +tree-sitter)     ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
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
       (mu4e +mbsync +org)
       ;; (notmuch +afew +org)
       ;;(wanderlust +gmail)

       :app
       ;; calendar
       ;;irc               ; how neckbeards socialize
       ;;(rss +org)        ; emacs as an RSS reader
       ;;twitter           ; twitter client https://twitter.com/vnought

       :config
       ;;literate
       (default +bindings +smartparens))
