;; +eglot.el -*- lexical-binding: t; -*-

;; require cl libraries for fixing eglot
(require 'cl-lib)
(require 'cl-macs)
(eval-when-compile (require 'eglot))
(declare-function WorkDoneProgress nil nil)

;; uncomment to debug lsp events
;; (cl-callf plist-put eglot-events-buffer-config :size 0)
(setq eglot-events-buffer-config '(:size 0 :format full))
(add-hook! terraform-mode
  (setq-local completion-at-point-functions #'eglot-completion-at-point))
(set-eglot-client! '(terraform-mode :language-id "opentofu") '("tofu-ls" "serve") "tofu-ls")
(set-eglot-client! '(nix-mode nix-ts-mode) '("nixd" "--semantic-tokens=true") "nixd")
(set-eglot-client! '(haskell-mode haskell-ts-mode) '("haskell-language-server-wrapper" "-d" "lsp") "haskell-language-server")
(set-eglot-client! '(jsonnet-mode :language-id "jsonnet") '("jsonnet-language-server" "-l" "debug") "jsonnet-language-server")
(set-eglot-client! '(helm-ts-mode) '("helm_ls" "serve"))
(set-eglot-client! '(nu-ts-mode) '("nu" "--lsp") "nu")
(set-eglot-client! '(javascript-mode typescript-ts-mode tsx-ts-mode js-jsx-mode) '("deno" "lsp") "deno-ls")
(set-eglot-client! '(yaml-mode yaml-ts-mode) '("yaml-schema-router") "yaml-language-server")

(setq-default
 jsonrpc-default-request-timeout 30
 terraform-format-on-save t
 eglot-confirm-server-initiated-edits nil
 eglot-workspace-configuration '(:rust-analyzer (:procMacro (:enable t :attributes (:enable t))
                                                 :check (:command "clippy")
                                                 :lens (:references (:adt (:enable t)
                                                                     :enumVariant (:enable t)
                                                                     :method (:enable t)
                                                                     :trait (:enable t)
                                                                     :run (:enable t)
                                                                     :updateTest (:enable t)))
                                                 :semanticHighlighting (:comments (:enable t)
                                                                        :doc (:comment (:inject (:enable t)))
                                                                        :operator (:specialization (:enable t))
                                                                        :punctuation (:enable t
                                                                                      :separate (:macro (:bang t))
                                                                                      :specialization (:enable t)))
                                                 :inlayHints (:bindingModeHints (:enable t)
                                                              :chainingHints (:enable t)
                                                              :closingBraceHints (:enable t)
                                                              :closureCaptureHints (:enable t)
                                                              :closureReturnTypeHints (:enable t)
                                                              :discriminantHints (:enable t)
                                                              :expressionAdjustmentHints (:enable t)
                                                              :genericParameterHints (:const (:enable t)
                                                                                      :lifetime (:enable t)
                                                                                      :type (:enable t))
                                                              :implicitDrops (:enable t)
                                                              :implicitSizedBoundHints (:enable t)
                                                              :impliedDynTraitHints (:enable t)
                                                              :lifetimeElisionHints (:enable t
                                                                                     :useParameterNames t)
                                                              :parameterHints (:enable t
                                                                               :missingArguments (:enable t))
                                                              :typeHints (:enable t
                                                                          :hideInferredTypes t
                                                                          :hideNamedConstructor t))
                                                 :completion (:termSearch (:enable t))
                                                 :hover (:actions (:references (:enable t)))
                                                 :typing (:triggerChars "=.{(><")
                                                 :workspace (:symbol (:search (:scope "workspace_and_dependencies"
                                                                               :kind "all_symbols"))))
                                 :gopls (:usePlaceholders t
                                         :completeUnimported t
                                         :gofumpt t                          ;; stricter gofmt; drop if you don't want it
                                         :staticcheck t                      ;; runs staticcheck via gopls; needs gopls >= 0.15
                                         :semanticTokens t
                                         :hoverKind "FullDocumentation"
                                         :linkTarget "pkg.go.dev"
                                         :experimentalPostfixCompletions t
                                         :directoryFilters ["-**/node_modules" "-**/vendor"]
                                         :analyses (:fieldalignment t        ;; warn on bad struct layout; noisy, optional
                                                    :nilness t
                                                    :shadow t
                                                    :unusedparams t
                                                    :unusedwrite t
                                                    :useany t
                                                    :unusedvariable t)
                                         :codelenses (:gc_details t
                                                      :generate t
                                                      :regenerate_cgo t
                                                      :run_govulncheck t     ;; surfaces CVEs in deps
                                                      :test t
                                                      :tidy t
                                                      :upgrade_dependency t
                                                      :vendor t)
                                         :hints (:assignVariableTypes t
                                                 :compositeLiteralFields t
                                                 :compositeLiteralTypes t
                                                 :constantValues t
                                                 :functionTypeParameters t
                                                 :parameterNames t
                                                 :rangeVariableTypes t)
                                         :buildFlags []                      ;; add e.g. ["-tags=integration"] per-project via .dir-locals.el
                                         :env (:GOFLAGS ""))
                                 :nixd (:formatting (:command ["nix" "fmt" "--"])
                                        :nixpkgs (:expr "import (builtins.getFlake \"github:nixos/nixpkgs\") {}")
                                        :options
                                        (:nixos (:expr "(builtins.getFlake (builtins.toString ./.)).nixosConfigurations.yew.options")
                                         :home-manager (:expr "(builtins.getFlake (builtins.toString ./.)).nixosConfigurations.yew.options.home-manager.users.type.getSubOptions []")))
                                 :nil (:formatting (:command ["alejandra"])
                                       :nix (:flake (:autoEvalInputs t)))
                                 :haskell (:sessionLoading "multipleComponents"
                                           :formattingProvider "fourmolu"
                                           :plugin (:semanticTokens (:globalOn t)
                                                    :ghcide-type-lenses (:codeLensOn t
                                                                         :config (:localBindingInlayHintOn t
                                                                                  :mode "always"))
                                                    :tactics (:globalOn t)
                                                    :hlint (:globalOn t)
                                                    :rename (:config (:crossModule t)
                                                             :usePrepare :json-false
                                                             :diff t)))
                                 :helm-ls (:logLevel "debug"
                                           :yamlls (:enabled t
                                                    :path "yaml-schema-router"
                                                    :config (:completion t
                                                             :hover t
                                                             :validate t
                                                             :format (:enable t))))
                                 :yaml (:completion t
                                        :hover t
                                        :validate t
                                        :format (:enable t)
                                        :maxItemsComputed 5000)
                                 :tofu-ls (:validation (:enableEnhancedValidation t))
                                 :ruff (:lint (:preview t)
                                        :format (:preview t))))

(defvar +eglot-post-load-hook '()
  "Hook run after a managed server settles (e.g. finishes indexing).
Each function is called with one argument, the eglot SERVER, in each of the
server's displayed buffers (with that buffer current).  Run debounced per
server -- on `$/progress' end and on `workspace/inlayHint/refresh' -- so an
indexing burst collapses into a single run once the server goes quiet.  Use it
for UI that eglot pulls lazily and won't re-render on its own.")

(defvar +eglot--post-load-timers (make-hash-table :test 'eq)
  "Per-server debounce timers for `+eglot-post-load-hook'.")

(defun +eglot--run-post-load (server)
  "Debounced: run `+eglot-post-load-hook' in each displayed buffer of SERVER."
  (let ((existing (gethash server +eglot--post-load-timers)))
    (when (timerp existing) (cancel-timer existing)))
  (puthash server
           (run-at-time
            0.5 nil
            (lambda ()
              (remhash server +eglot--post-load-timers)
              (when (jsonrpc-running-p server)
                (dolist (buf (eglot--managed-buffers server))
                  (eglot--when-buffer-window buf
                    (run-hook-with-args '+eglot-post-load-hook server))))))
           +eglot--post-load-timers))

(defun +eglot-refresh-inlay-hints-h (_server)
  "Re-pull inlay hints for the current buffer's visible windows."
  (when (bound-and-true-p eglot-inlay-hints-mode)
    (dolist (win (get-buffer-window-list nil nil t))
      (eglot--update-hints-1 (window-start win) (window-end win t)))))

(defun +eglot-refresh-flymake-h (_server)
  "Re-run flymake backends in the current buffer."
  (when (bound-and-true-p flymake-mode)
    (flymake-start)))

(add-hook '+eglot-post-load-hook #'+eglot-refresh-inlay-hints-h)
(add-hook '+eglot-post-load-hook #'+eglot-refresh-flymake-h)

(defun +eglot--advertise-inlayhint-refresh (caps)
  "Add `workspace.inlayHint.refreshSupport' to eglot client CAPS."
  (plist-put caps :workspace
             (plist-put (plist-get caps :workspace)
                        :inlayHint '(:refreshSupport t)))
  caps)
(advice-add 'eglot-client-capabilities :filter-return #'+eglot--advertise-inlayhint-refresh)

(cl-defmethod eglot-handle-request
  (server (_method (eql workspace/inlayHint/refresh)))
  "Run post-load refreshers when SERVER requests an inlay-hint refresh."
  (+eglot--run-post-load server)
  nil)

(cl-defmethod eglot-handle-notification :after
  (server (_method (eql $/progress)) &key _token value)
  "Run `+eglot-post-load-hook' once SERVER goes quiet after reporting progress.
Hints (and other lazily-rendered UI) requested mid-indexing come back empty or
stale; refreshing when a progress sequence ends fills them in without a scroll."
  (eglot--dbind ((WorkDoneProgress) kind) value
    (when (equal kind "end")
      (+eglot--run-post-load server))))

(map! :leader
      "c x" #'consult-flymake
      "p x" #'consult-flymake-project)
(map! :nv "g b" #'xref-go-back)
(map! :nv "g B" #'xref-go-forward)
(map! :nv "g D" #'+lookup/references)
(map! :nv "g d" #'+lookup/definition)
(map! :nv "g i" #'+lookup/implementations)
