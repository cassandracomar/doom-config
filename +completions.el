;;; +completions.el -*- lexical-binding: t; -*-

;; Shared completion infrastructure used by both eshell and eat (nushell).
;; Loaded via `(require '+completions)' from either side, whichever triggers
;; first; subsequent requires are no-ops.
;;
;; Provides:
;;   - fish-completion override that attaches descriptions as `pcomplete-help'
;;     / `pcomplete-annotation' text properties so corfu and friends can
;;     display them.
;;   - rich `nix' completion: parses `NIX_GET_COMPLETIONS' output, augments
;;     subcommand candidates with descriptions from `nix __dump-cli', and
;;     exposes a doc-buffer fetcher that lazily resolves `<ref>.meta.description'
;;     for package candidates.
;;   - eshell pcomplete handler (`pcomplete/eshell-mode/nix') + bypass advice
;;     that lets fish-completion drive completion for commands without an
;;     explicit `pcomplete/eshell-mode/<cmd>' override.
;;   - filter-return advice on `pcomplete-completions-at-point' that appends
;;     `:company-doc-buffer' so corfu-popupinfo can pull per-candidate docs.

(use-package! fish-completion
  :commands global-fish-completion-mode fish-completion-mode
  :defer t
  :config
  (setq fish-completion-fallback-on-bash-p nil)

  (defun +fish-completion--list-completions-a (raw-prompt)
    "Like `fish-completion--list-completions' but preserve fish's per-completion
descriptions as `pcomplete-annotation' / `pcomplete-help' text properties so
corfu and friends can display them."
    (let ((candidates (fish-completion--list-completions-with-desc raw-prompt)))
      (when candidates
        (mapcar (lambda (e)
                  (let* ((parts (split-string e "\t"))
                         (name (car parts))
                         (desc (cadr parts)))
                    (if (and desc (not (string-empty-p desc)))
                        (propertize name
                                    'pcomplete-annotation (concat " " desc)
                                    'pcomplete-help desc)
                      name)))
                (split-string candidates "\n" t)))))
  (advice-add 'fish-completion--list-completions :override
              #'+fish-completion--list-completions-a))

(defun +eshell-bypass-pcomplete-cmd-a (orig-fn command)
  "Bypass global `pcomplete/<cmd>' shims in eshell with fish-completion-mode on,
but allow eshell-specific `pcomplete/eshell-mode/<cmd>' overrides."
  (if (and (eq major-mode 'eshell-mode) fish-completion-mode)
      (let ((sym (intern-soft (concat "pcomplete/eshell-mode/" command))))
        (and sym (fboundp sym) sym))
    (funcall orig-fn command)))
(advice-add 'pcomplete-find-completion-function :around #'+eshell-bypass-pcomplete-cmd-a)

(defvar +nix--cli-spec-cache nil
  "Cached parse of `nix __dump-cli' JSON, keyed by `nix' executable path.")

(defun +nix--cli-spec ()
  "Return parsed `nix __dump-cli' as a hash-table tree, lazily cached."
  (let ((path (executable-find "nix")))
    (unless (equal path (car +nix--cli-spec-cache))
      (setq +nix--cli-spec-cache
            (cons path
                  (with-temp-buffer
                    (when (zerop (call-process "nix" nil t nil "__dump-cli"))
                      (goto-char (point-min))
                      (ignore-errors
                        (json-parse-buffer :object-type 'hash-table)))))))
    (cdr +nix--cli-spec-cache)))

(defun +nix--subcommand-description (subcommand-path candidate)
  "Look up CANDIDATE's description in nix's CLI spec under SUBCOMMAND-PATH.
SUBCOMMAND-PATH is the list of resolved subcommands preceding CANDIDATE."
  (when-let* ((spec (+nix--cli-spec))
              (node (gethash "args" spec)))
    (catch 'done
      (dolist (cmd subcommand-path)
        (let ((cmds (gethash "commands" node)))
          (unless (and cmds (gethash cmd cmds))
            (throw 'done nil))
          (setq node (gethash cmd cmds))))
      (let ((cmds (gethash "commands" node)))
        (when (and cmds (gethash candidate cmds))
          (gethash "description" (gethash candidate cmds)))))))

(defun +nix-completions (args n)
  "Compute `nix' completions for ARGS at zero-based position N.
ARGS is the full list of arg strings (with `nix' at index 0); N is the
index of the arg currently being completed.

Returns a plist `(:candidates CS :header H :stub S)' where:
  CS  list of propertized candidate strings.  Each candidate carries the
      `nix-full-ref' text property (full installable, e.g.
      `nixpkgs#hello'), and -- when a description is known -- the
      `pcomplete-annotation' / `pcomplete-help' properties used by corfu
      and the doc-buffer pipeline.
  H   nix's completion-type hint (e.g. `\"filenames\"', `\"attrs\"').
  S   the partial currently being completed (suffix of ARGS[N] from
      the last `#'/`.'/`:'/`?').

To allow corfu to filter as the user types or backspaces within a path
segment, we ask nix for the FULL sibling set at the parent level (by
truncating the current arg at the last `#'/`.'/`:'/`?' before sending)
and report the partial as the stub.  Descriptions for subcommands fall
back to `nix __dump-cli' when nix doesn't include them inline."
  (let* ((current-arg (or (nth n args) ""))
         (stub-start (let ((p (string-match "[#.:?][^#.:?]*\\'" current-arg)))
                       (if p (1+ p) 0)))
         (stub-prefix (substring current-arg 0 stub-start))
         (stub (substring current-arg stub-start))
         (query-args (append (cl-subseq args 1 n) (list stub-prefix)))
         (process-environment
          (cons (format "NIX_GET_COMPLETIONS=%d" n) process-environment))
         (output (with-output-to-string
                   (apply #'call-process "nix" nil (list standard-output nil) nil
                          query-args)))
         (lines (split-string output "\n" t))
         (header (car lines))
         (subcommand-path (cl-subseq args 1 n))
         ;; Subcommand descriptions live in `nix __dump-cli' (multi-MB JSON,
         ;; ~hundreds of ms to load + parse on first hit).  That spec only
         ;; describes SUBCOMMANDS, so for `filenames'/`attrs' completions
         ;; (flake refs, packages, file paths -- typically 10k+ candidates)
         ;; every per-candidate lookup is wasted work returning nil.  Skip
         ;; the spec entirely in that case; package descriptions arrive
         ;; lazily via `+nix--fetch-meta-description' from the doc-buffer.
         (describe-via-cli-spec (not (member header '("filenames" "attrs"))))
         (candidates
          (mapcar (lambda (line)
                    (let* ((parts (split-string line "\t"))
                           (full-name (car parts))
                           (short-name (if (and (not (string-empty-p stub-prefix))
                                                (string-prefix-p stub-prefix full-name))
                                           (substring full-name (length stub-prefix))
                                         full-name))
                           (desc (or (and (cadr parts)
                                          (not (string-empty-p (cadr parts)))
                                          (cadr parts))
                                     (and describe-via-cli-spec
                                          (+nix--subcommand-description
                                           subcommand-path short-name)))))
                      (apply #'propertize short-name
                             'nix-full-ref full-name
                             (when desc
                               (list 'pcomplete-annotation (concat " " desc)
                                     'pcomplete-help desc)))))
                  (cdr lines))))
    (list :candidates candidates :header header :stub stub)))

(defun pcomplete/eshell-mode/nix ()
  "Complete `nix' via `+nix-completions', wrapping its result in pcomplete
protocol.  Backspacing past the last `#'/`.'/`:'/`?' boundary inside a
flake/attr ref exits completion-in-region and triggers a fresh CAPF call
for the next parent level."
  (let* ((args pcomplete-args)
         (n (1- (length args)))
         (result (+nix-completions args n))
         (header (plist-get result :header))
         (stub (plist-get result :stub))
         (candidates (plist-get result :candidates)))
    (when (member header '("filenames" "attrs"))
      ;; `pcomplete-exit-function' is let-bound by
      ;; `pcomplete-completions-at-point'; override it locally so we don't
      ;; mutate the global `pcomplete-termination-string'.
      (setq pcomplete-exit-function (lambda (&rest _) nil)))
    (dolist (_ (cddr args))
      (pcomplete-here))
    (pcomplete-here candidates stub t)))

(defvar +pcomplete-doc-cache (make-hash-table :test 'equal)
  "Session-local cache of completion documentation by candidate string.")

(defun +nix--fetch-meta-description (ref)
  "Fetch `<REF>.meta.description' via `nix eval --raw'.
Returns nil on failure or empty result.  Cached in `+pcomplete-doc-cache'."
  (let ((cached (gethash ref +pcomplete-doc-cache 'miss)))
    (if (not (eq cached 'miss))
        cached
      (let ((desc (with-temp-buffer
                    (when (zerop (call-process
                                  "nix" nil (list (current-buffer) nil) nil
                                  "eval" "--raw"
                                  (format "%s.meta.description" ref)))
                      (let ((s (string-trim (buffer-string))))
                        (and (not (string-empty-p s)) s))))))
        (puthash ref desc +pcomplete-doc-cache)
        desc))))

(defvar +pcomplete-doc--last-buffer nil
  "Most recent buffer returned by `+pcomplete-doc-buffer', killed on next call.")

(defun +pcomplete-doc-buffer (cand)
  "Return a doc buffer for CAND, used as `:company-doc-buffer' for pcomplete.
Checks `pcomplete-help' text property first; for `flake#attr'-style candidates
without an inline description, lazily fetches `.meta.description' via nix.

Uses a freshly-generated buffer per call (with `inhibit-buffer-hooks') so that
`erase-buffer'/`insert' side-effects don't leak into corfu's render pipeline.
The previous buffer is killed when a new one is created to avoid leaks."
  (when (stringp cand)
    (when-let* ((full-ref (or (get-text-property 0 'nix-full-ref cand) cand))
                (doc (or (get-text-property 0 'pcomplete-help cand)
                         (and (string-match-p "#" full-ref)
                              (+nix--fetch-meta-description full-ref)))))
      (when (buffer-live-p +pcomplete-doc--last-buffer)
        (kill-buffer +pcomplete-doc--last-buffer))
      (let ((buf (generate-new-buffer " *pcomplete-doc*" t)))
        (with-current-buffer buf
          (insert doc))
        (setq +pcomplete-doc--last-buffer buf)
        buf))))

(defun +pcomplete-add-doc-buffer-a (result)
  "Append `:company-doc-buffer' to `pcomplete-completions-at-point' result
in eshell-mode with fish-completion-mode active, so corfu-popupinfo can
lazily fetch per-candidate descriptions."
  (when (and result (listp result)
             (eq major-mode 'eshell-mode)
             (bound-and-true-p fish-completion-mode))
    (setq result (append result (list :company-doc-buffer
                                      #'+pcomplete-doc-buffer))))
  result)
(advice-add 'pcomplete-completions-at-point :filter-return
            #'+pcomplete-add-doc-buffer-a)

(provide '+completions)
