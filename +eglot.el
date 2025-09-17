;; +eglot.el -*- lexical-binding: t; -*-

;; require cl libraries for fixing eglot
(require 'cl-lib)
(require 'cl-macs)

;; uncomment to debug lsp events
;; (cl-callf plist-put eglot-events-buffer-config :size 2000000)
(set-eglot-client! 'nix-mode '("nixd" "--semantic-tokens=true"))
(set-eglot-client! 'haskell-mode '("haskell-language-server-wrapper" "-d" "lsp"))
(set-eglot-client! '(terraform-mode :language-id "opentofu") '("tofu-ls" "serve"))
(set-eglot-client! 'rustic-mode '("rust-analyzer"))
(add-hook! terraform-mode
  (setq-local completion-at-point-functions #'eglot-completion-at-point))
(setq-default
 jsonrpc-default-request-timeout 30
 terraform-format-on-save t
 eglot-confirm-server-initiated-edits nil
 eglot-semantic-tokens-use-delta t
 ;; eglot-advertise-cancellation t
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
                                        :nix-central (:expr "import (builtins.getFlake \"/Users/ccomar/src/git.drwholdings.com/up-platform-infrastructure/nix-central\")")
                                        :nixpkgs (:expr "import (builtins.getFlake \"/Users/ccomar/src/git.drwholdings.com/up-platform-infrastructure/nix-central\").inputs.nixpkgs")
                                        :options (:nix-central (:expr "import (builtins.getFlake \"/Users/ccomar/src/git.drwholdings.com/up-platform-infrastructure/nix-central\")")))
                                 :nil (:formatting (:command ["alejandra"])
                                       :nix (:flake (:autoEvalInputs t)))
                                 :haskell (:sessionLoading "multipleComponents"
                                           :formattingProvider "fourmolu"
                                           :plugin (:semanticTokens (:globalOn t)
                                                    :ghc-ide-type-lenses (:codeLensOn t
                                                                          :config (:localBindingInlayHintOn t
                                                                                   :mode "always"))
                                                    :tactics (:globalOn t)
                                                    :hlint (:globalOn t)
                                                    :rename (:config (:crossModule t)
                                                             :usePrepare :json-false
                                                             :diff t)))
                                 :yaml-language-server (:yaml (:schemaStore (:enable t)))
                                 :tofu-ls (:validation (:enableEnhancedValidation t))
                                 ))
(set-popup-rule! "^\\*eglot-help" :size 0.5 :quit t :select t :side 'right)

;; (defmacro idle-timer! (secs repeat &rest forms)
;;   "run forms when the editor has been idle for `secs' and `repeat'. the number
;; of repetitions is infinite if `repeat' is `t', 0 if `nil', and `count' if a
;; number is provided."
;;   `(run-with-idle-timer ,secs ,repeat (lambda () ,@forms)))

;; (defvar +eglot-idle-timers
;;   (make-hash-table))

;; (defcustom +eglot-idle-timer-period 5
;;   "length of the quiet period required to refresh idle timers"
;;   :type 'number :group 'eglot-local)

;; (defun +eglot--timer-hash-key (&optional buf)
;;   (if (null buf)
;;       (buffer-name (current-buffer))
;;     (if (bufferp buf)
;;         (buffer-name buf)
;;       buf)))

;; (defun +eglot--get-buffer-idle-timers (&optional buf)
;;   (let* ((buffer-key (+eglot--timer-hash-key buf))
;;          (buffer-timers (gethash buffer-key +eglot-idle-timers)))
;;     (when (null buffer-timers)
;;       (setq buffer-timers (make-hash-table)))
;;     buffer-timers))

;; (defmacro +eglot--with-buffer-idle-timers (server &rest forms)
;;   `(let* ((buffer-key (+eglot--timer-hash-key))
;;           (buffer-timers (+eglot--get-buffer-idle-timers))
;;           (server-timers (gethash ,server buffer-timers)))
;;      (when (null server-timers)
;;        (setq server-timers (make-hash-table)))
;;      ,@forms
;;      (unless (null server-timers) (puthash server server-timers buffer-timers))
;;      (puthash buffer-key buffer-timers +eglot-idle-timers)))


;; (defmacro plist-get! (plist &rest keys)
;;   `(cl-reduce (lambda (r a) (plist-get r a)) (list ,@keys) :initial-value ,plist))

;; (defun collect-idle-timers (server)
;;   (let* ((buffer-timers (+eglot--get-buffer-idle-timers))
;;          (timers-by-key (gethash server buffer-timers)))
;;     (hash-table-values timers-by-key)))

;; (defun +eglot--register-idle-timer (server key timer)
;;   (+eglot--with-buffer-idle-timers
;;    server
;;    (let ((curr-timer (gethash key server-timers)))
;;      (when curr-timer (cancel-timer curr-timer))
;;      (puthash key timer server-timers))))

;; (defun +eglot--cancel-idle-timer (server key)
;;   (+eglot--with-buffer-idle-timers
;;    server
;;    (let ((curr-timer (gethash key server-timers)))
;;      (cancel-timer curr-timer)
;;      (remhash key server-timers))))

;; (defun +eglot--cancel-idle-timers (lsp-server &rest _)
;;   (let* ((server-info (eglot--server-info lsp-server))
;;          (server (plist-get server-info :name)))
;;     (ignore-errors
;;       (+eglot--with-buffer-idle-timers
;;        server
;;        (let ((timers (collect-idle-timers server)))
;;          (dolist (timer timers)
;;            (cancel-timer timer))
;;          (remhash server buffer-timers)
;;          (setq server-timers nil))) )))

;; (defun +eglot--cancel-all-idle-timers-buffer (&optional buf)
;;   (let* ((buffer-name (+eglot--timer-hash-key buf))
;;          (buffer-timers (+eglot--get-buffer-idle-timers buf))
;;          (servers (hash-table-keys buffer-timers)))
;;     (dolist (server servers)
;;       (let* ((server-timers (gethash server buffer-timers))
;;              (keys (hash-table-keys server-timers)))
;;         (dolist (key keys)
;;           (+eglot--cancel-idle-timer server key))))
;;     (remhash buffer-name +eglot-idle-timers)))

;; (defmacro +eglot--run-with-idle-timer (server key &optional ip &rest forms)
;;   (let ((idle-period `(or ,ip +eglot-idle-timer-period)))
;;     `(+eglot--register-idle-timer
;;       ,server
;;       ,key
;;       (idle-timer! ,idle-period t ,@forms))))

;; (defmacro +eglot-run-when-idle (key buf &rest body)
;;   `(let* ((server (eglot--current-server-or-lose))
;;           (server-info (eglot--server-info server))
;;           (server-name (plist-get server-info :name)))
;;      (+eglot--run-with-idle-timer server-name ,key +eglot-idle-timer-period (with-current-buffer ,buf ,@body))))


;; ;; make sure timers get cleaned up with the buffer
;; (advice-add #'eglot-shutdown :before #'+eglot--cancel-idle-timers)
;; (add-hook! kill-buffer #'+eglot--cancel-all-idle-timers-buffer)

;; ;; add the timers when eglot is started on a buffer
;; (add-hook! eglot-managed-mode
;;   (idle-timer! 1 nil
;;                (let ((buffers (eglot--managed-buffers (eglot--current-server-or-lose)))
;;                      (new-buffer?
;;                       (lambda (b)
;;                         (not (member (buffer-name b) (hash-table-keys +eglot-idle-timers))))))
;;                  (dolist (buf (-filter new-buffer? buffers))
;;                    (eglot--when-buffer-window buf
;;                      (+eglot-run-when-idle :inlayHints buf (eglot-inlay-hints-mode +1))
;;                      (+eglot-run-when-idle :semanticTokens buf
;;                                            (eglot--semantic-tokens-mode +1)
;;                                            (eglot--semantic-tokens-queue-update)))))))

;; (defun +eglot-semantic-tokens--fontifier (begin end)
;;   (eglot--semantic-tokens-highlight-range begin end)
;;   `(jit-lock-bounds ,begin . ,end))

;; (defun +eglot-semantic-tokens--font-lock-init ()
;;   "Call this from your mode hook to switch semantic token font-lock on."
;;   (if (ignore-errors (eglot--TextDocumentIdentifier))
;;       (progn
;;         (jit-lock-register '+eglot-semantic-tokens--fontifier t)
;;         (setq-local font-lock-defaults
;;                     '(nil nil nil nil
;;                       (font-lock-fontify-region-function
;;                        . eglot--semantic-tokens-highlight-region)
;;                       (font-lock-unfontify-region-function
;;                        . eglot--semantic-tokens-unhighlight-region)
;;                       (font-lock-fontify-buffer-function
;;                        . eglot--semantic-tokens-highlight-full/delta))))
;;     (message "avoiding +eglot-semantic-tokens--font-lock-init for buffer without file")))

;; (add-hook! (haskell-mode rustic-mode nix-mode) #'+eglot-semantic-tokens--font-lock-init)
;; (add-hook! 'eglot-managed-mode-hook
;;   (progn
;;     (eglot--semantic-tokens-mode +1)
;;     (eglot--semantic-tokens-queue-update)))

(defvar +eglot-post-load-hook
  '()
  "hooks to run after the server has finished loading the project.
each hook is run for each project buffer.")

;; (add-hook! +eglot-post-load
;;   (run-with-idle-timer 1 nil #'eglot-inlay-hints-mode +1)
;;   (eglot--semantic-tokens-queue-update))

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
    (eglot--dbind ((WorkDoneProgress) kind title percentage message) value
                  (pcase kind
                    ("end" (refreshf))))))

(map! :leader
      "c x" #'flymake-show-project-diagnostics
      "p x" #'flymake-show-project-diagnostics)
