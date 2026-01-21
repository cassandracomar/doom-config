;; +eglot.el -*- lexical-binding: t; -*-

;; require cl libraries for fixing eglot
(require 'cl-lib)
(require 'cl-macs)

;; uncomment to debug lsp events
;; (cl-callf plist-put eglot-events-buffer-config :size 2000000)
(add-hook! terraform-mode
  (setq-local completion-at-point-functions #'eglot-completion-at-point))
(set-eglot-client! '(terraform-mode :language-id "opentofu") '("tofu-ls" "serve") "tofu-ls")
(set-eglot-client! '(nix-mode nix-ts-mode) '("nixd" "--semantic-tokens=true") "nixd")
(set-eglot-client! '(haskell-mode haskell-ts-mode) '("haskell-language-server-wrapper" "-d" "lsp") "haskell-language-server")

(setq-default
 jsonrpc-default-request-timeout 30
 terraform-format-on-save t
 eglot-confirm-server-initiated-edits nil
 eglot-workspace-configuration '(:rust-analyzer (:procMacro (:enable t)
                                                 :lens (:references (:adt (:enable t)
                                                                     :enumVariant (:enable t)
                                                                     :method (:enable t)
                                                                     :trait (:enable t)
                                                                     :run (:enable t)
                                                                     :updateTest (:enable t)))
                                                 :semanticHighlighting (:punctuation (:enable t
                                                                                      :separate (:macro (:bang t))
                                                                                      :specialization (:enable t)))
                                                 :inlayHints (:bindingModeHints (:enable t)
                                                              :chainingHints (:enable t)
                                                              :closingBraceHints (:enable t)
                                                              :discriminantHints (:enable t)
                                                              :closureReturnTypeHints (:enable t)
                                                              :expressionAdjustmentHints (:enable t)
                                                              :genericParameterHints (:const (:enable t)
                                                                                      :lifetime (:enable t)
                                                                                      :type (:enable t))
                                                              :lifetimeElisionHints (:enable t)
                                                              :parameterHints (:enable t)
                                                              :typeHints (:enable t)))
                                 :nixd (:formatting (:command ["alejandra"])
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
                                 :yaml-language-server (:yaml (:schemaStore (:enable t)))
                                 :tofu-ls (:validation (:enableEnhancedValidation t))))
(set-popup-rule! "^\\*eglot-help" :size 0.5 :quit t :select t :side 'right)

(defvar +eglot-post-load-hook
  '()
  "hooks to run after the server has finished loading the project.
each hook is run for each project buffer.")

(defun +eglot-reload-inlay-hints ()
  (eglot--update-hints (point-min) (point-max)))

(add-hook! +eglot-post-load
  (run-with-idle-timer 1 nil #'+eglot-reload-inlay-hints)
  (run-with-idle-timer 1 nil #'font-lock-fontify-buffer))

;; or without doom:
;;
;; (add-hook +eglot-post-load-hook
;;           (lambda ()
;;             (eglot--semantic-tokens-mode +1)
;;             (eglot--semantic-tokens-queue-update)))

(cl-defmethod eglot-handle-notification :after
  (server (_method (eql $/progress)) &key _token value)
  "wait for the server to finish loading the project before attempting to
render inlay hints and semantic tokens. because eglot doesn't wait for the
server to finish loading/indexing the project completely before running most of
the available hooks, it gets back an empty set of inlay hints/semantic tokens
initially. these UI elements do update after an edit to the document via
`eglot--document-changed-hook' -- however, this isn't a great substitute for
just refreshing these UI elements after the server has loaded.

configure the refreshes to take place post-load via `+eglot-post-load-hook'"
  ;; if your server provides a specific token for specific kinds of $/progress events,
  ;; you can wrap this in a `(when (equal token "$TOKEN") ...)'
  ;; e.g. rust-analyzer uses "rustAnalyzer/Indexing"
  (cl-flet* ((run-post-load-hooks (buf)
               (eglot--when-buffer-window
                   buf
                 (run-hooks '+eglot-post-load-hook)))
             (refreshf ()
               (let ((buffers (eglot--managed-buffers server)))
                 (dolist (buf buffers)
                   (run-post-load-hooks buf)))))
    (eglot--dbind ((WorkDoneProgress) kind _title _percentage _message) value
      (pcase kind
        ("end" (refreshf))))))

;; (defvar +eglot-after-envrc-hook '())
;; (defvar-local +eglot-after-envrc-run? nil)
;; (defun +eglot-ensure-connected ()
;;   (when (and (eglot-managed-p)
;;              (not (eglot-current-server)))
;;     (lsp!)))

;; (defun +envrc-status-watcher (symbol newval operation where)
;;   (when (and (equal symbol 'envrc--status)
;;              (eq operation 'set)
;;              (bufferp where)
;;              (buffer-file-name where) ;; only try to start eglot this way in file-visiting buffers
;;              (equal newval 'on))
;;     (with-current-buffer where
;;       (when (and (eglot--lookup-mode major-mode)
;;                  (not +eglot-after-envrc-run?))
;;         ;; make sure hooks are only triggered once
;;         (setq +eglot-after-envrc-run? t)
;;         (run-hooks '+eglot-after-envrc-hook)))))
;; (add-variable-watcher 'envrc--status #'+envrc-status-watcher)
;; (add-hook! '+eglot-after-envrc-hook #'+eglot-ensure-connected)

(map! :leader
      "c x" #'consult-flymake
      "p x" #'consult-flymake-project)
(map! :nv "g b" #'xref-go-back)
(map! :nv "g B" #'xref-go-forward)
(map! :nv "g D" #'+lookup/references)
(map! :nv "g d" #'+lookup/definition)
(map! :nv "g i" #'+lookup/implementations)
