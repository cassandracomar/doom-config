;;; +eat-nushell.el -*- lexical-binding: t; -*-

(require '+completions)

(load "s")
(load "dash")

(setq nushell-history-file (shell-command-to-string "nu -c '$nu.history-path | print -n'"))
(defcustom carapace-nushell-quoted-arg-chars "~/A-Za-z0-9\\+@:_\\.\\$#%,={} -"
  "regex matching an unquoted argument")
(defcustom carapace-nushell-unquoted-arg-chars "~/A-Za-z0-9\\+@:_\\.\\$#%,={}-"
  "regex matching an unquoted argument")
(defcustom carapace-nushell-quoted-arg-regex
  (format "['\"`][%s]+?['\"`][%s]*"
          carapace-nushell-quoted-arg-chars
          carapace-nushell-quoted-arg-chars)
  "regex matching a quoted argument")
(defcustom carapace-nushell-unquoted-arg-regex (format "[%s]" carapace-nushell-unquoted-arg-chars)
  "regex matching a quoted argument")
(setq carapace-nushell-quoted-arg-chars "~/A-Za-z0-9\\+@:_\\.\\$#%,={} -")
(setq carapace-nushell-quoted-arg-regex
      (format "['\"`][%s]+?['\"`][%s]*"
              carapace-nushell-quoted-arg-chars
              carapace-nushell-quoted-arg-chars))

(setq carapace-completion-command (executable-find "carapace"))
(defun carapace-completion--fish-fallback (raw-prompt)
  "Return a hash-table of fish completions for RAW-PROMPT.
Extracts per-candidate descriptions so they flow into the `annotation'
case of `carapace-nushell-backend' (and on to corfu / corfu-popupinfo).
Two sources are checked, in order:
1. The `pcomplete-help' text property, set by `+fish-completion--list-completions-a'
   in `+eshell.el' when that override is loaded.
2. A literal `name<TAB>description' split, in case the override isn't active
   yet (e.g. fish-completion hasn't been required by the eshell side)."
  (let* ((prompt (if (equal raw-prompt "")
                     " "
                   raw-prompt))
         (completions (fish-completion--list-completions prompt)))
    (cl-reduce (lambda (table comp)
                 (let* ((tab-pos (string-search "\t" comp))
                        (name (if tab-pos
                                  (substring comp 0 tab-pos)
                                (substring-no-properties comp)))
                        (desc (or (get-text-property 0 'pcomplete-help comp)
                                  (and tab-pos (substring comp (1+ tab-pos)))))
                        ;; Fish returns directory completions with a trailing
                        ;; `/'; leave the user mid-path in that case.  All
                        ;; other fish completions (flags, files, commands)
                        ;; get the default space terminator.
                        (terminator (if (string-suffix-p "/" name) "" " ")))
                   (puthash (concat name " ")
                            `(:display ,name :value ,name :terminator ,terminator
                              ,@(and desc (not (string-empty-p desc))
                                     (list :description desc)))
                            table))
                 table)
               completions
               :initial-value (make-hash-table :test #'equal :size (length completions)))))

(defun carapace-completion--call (command &rest args)
  "Return the output of the call to COMMAND ARGS as a string."
  (with-output-to-string
    (with-current-buffer standard-output
      (apply #'call-process
             command
             nil '(t nil) nil
             args))))

(defun carapace-completion--list-completions-with-desc (shell raw-prompt)
  (carapace-completion--fish-fallback raw-prompt))

(defun carapace-nushell--line-offset (offset-bounds)
  (cl-flet ((offset-f (bound) (+ (comint-line-beginning-position) bound)))
    (pcase offset-bounds
      (`(,offset-beg . ,offset-end)
       (let ((beg (offset-f offset-beg))
             (end (offset-f offset-end)))
         `(,beg . ,end))))))

(defun carapace-nushell--point-in-arg-p (arg-bounds)
  (pcase (carapace-nushell--line-offset arg-bounds)
    (`(,beg . ,end)
     (and (>= (point) beg)
          (<= (point) end)))))

(defun carapace-nushell--unquoted-arg-at-point ()
  (let* ((rawbeg (save-excursion (search-backward-regexp "\\(^\\|\s\\)")))
         (rawend (save-excursion (search-forward-regexp "\\($\\|\s\\)")))
         ;; handle inclusivity of bounds
         (beg (if (eql (char-after rawbeg) ?\s) (+ rawbeg 1) rawbeg))
         (end rawend)
         (bol (comint-line-beginning-position))
         (eol (pos-eol)))
    (list
     (if (< beg bol) bol beg)
     (if (> end eol) eol end))))

(defun carapace-nushell--arg (raw-prompt)
  (let* ((quoted-args  (nreverse (s-matched-positions-all carapace-nushell-quoted-arg-regex raw-prompt)))
         (quoted-arg-offset-at-point (cl-first (seq-filter #'carapace-nushell--point-in-arg-p quoted-args))))
    (pcase (carapace-nushell--line-offset quoted-arg-offset-at-point)
      (`(,beg . ,end) (list beg end))
      (_ (carapace-nushell--unquoted-arg-at-point)))))

(defun carapace-nushell--raw-prompt (&optional eol)
  "extract the current prompt for completion up to the provided eol marker"
  (let ((end (or eol (pos-eol))))
    (buffer-substring-no-properties (comint-line-beginning-position) end)))

(defun +eat-nushell--parse-args (raw-prompt)
  "Return the list of args parsed from RAW-PROMPT.
Splits on whitespace, dropping leading/trailing whitespace; if RAW-PROMPT
ends in whitespace, appends \"\" so the caller can treat the position
after that whitespace as a fresh arg under completion."
  (let ((tokens (split-string raw-prompt "[ \t]+" t)))
    (if (string-match-p "[ \t]\\'" raw-prompt)
        (append tokens (list ""))
      tokens)))

(defun +eat-nushell--nix-completions (raw-prompt)
  "Build the carapace-nushell candidates hash for `nix' completion of RAW-PROMPT.
Delegates to `+nix-completions' for the NIX_GET_COMPLETIONS query and
`nix __dump-cli' description enrichment, then re-shapes the result so
each candidate is keyed by its FULL completion text (e.g.
`nixpkgs#hello' for a flake/attr ref, or just `build' for a subcommand).
Using the full text -- rather than the short name with a stub-prefix
stripped -- keeps the completion region non-empty for cape's dynamic
table (cape.el:353 only calls the table when `beg < end') and lets
corfu/orderless filter cleanly against the user-typed prefix."
  (let* ((args (+eat-nushell--parse-args raw-prompt))
         (n (1- (length args)))
         (result (+nix-completions args n))
         (header (plist-get result :header))
         (table (make-hash-table :test #'equal)))
    (dolist (cand (plist-get result :candidates))
      (let* ((short (substring-no-properties cand))
             (desc (get-text-property 0 'pcomplete-help cand))
             (full-ref (get-text-property 0 'nix-full-ref cand))
             (key (or full-ref short))
             ;; Pick the terminator pcomplete-style.  `attrs' header (e.g.
             ;; `nixpkgs#hello' / `nixpkgs#hello.dependencies'): user might
             ;; drill in with `.', emit nothing.  `filenames' header is
             ;; overloaded -- it covers both bare flake refs and actual
             ;; paths -- so we further split: candidates that look like
             ;; filesystem paths emit nothing, everything else is a flake
             ;; ref and gets `#' appended so the user can immediately start
             ;; an attr path.  Non-attrs/non-filenames (subcommands etc.)
             ;; get the default space.
             (terminator
              (cond
               ((equal header "attrs") "")
               ((equal header "filenames")
                (if (string-match-p "\\`\\(/\\|\\./\\|\\.\\./\\|~/\\)" key)
                    ""
                  "#"))
               (t " "))))
        (puthash (concat key " ")
                 `(:display ,key :value ,key :terminator ,terminator
                   ,@(when desc (list :description desc))
                   ,@(when full-ref (list :nix-full-ref full-ref)))
                 table)))
    table))

(defvar-local carapace-nushell--active-completions nil)
(defun carapace-nushell--completions (prompt &optional no-refresh)
  (if no-refresh
      (or carapace-nushell--active-completions
          (carapace-nushell--completions prompt nil))
    (let* ((args (+eat-nushell--parse-args prompt))
           (cmd (car args))
           (current-arg (or (car (last args)) ""))
           (candidates
            ;; Dispatch nix completion to the shared NIX_GET_COMPLETIONS
            ;; pipeline (flake refs + attrs + lazy .meta.description in
            ;; the doc-buffer).  But `nix __get-completions' does NOT
            ;; enumerate subcommand flags; fish's nix completion plugin
            ;; does (with descriptions), so when the partial starts with
            ;; `-' we let fish handle it.  Likewise fall back to fish
            ;; when we're still completing the `nix' command name itself
            ;; (less than two args parsed).
            (cond
             ((and (equal cmd "nix")
                   (>= (length args) 2)
                   (not (string-prefix-p "-" current-arg)))
              (+eat-nushell--nix-completions prompt))
             (t
              (carapace-completion--list-completions-with-desc 'nushell prompt)))))
      (setq-local carapace-nushell--active-completions candidates))))

(defvar +eat-nushell-doc--last-buffer nil
  "Most recent buffer returned by `+eat-nushell-doc-buffer', killed on next call.")

(defun +eat-nushell-doc-buffer (cand-key)
  "Return a doc buffer for CAND-KEY in the active carapace-nushell hash.
Reads `:description' from the candidate's plist first; for `flake#attr'
candidates with no inline description, lazily fetches `.meta.description'
via `+nix--fetch-meta-description'.  The previous buffer is killed on
each call to avoid leaking buffers across corfu-popupinfo invocations."
  (when-let* ((table carapace-nushell--active-completions)
              (cand (gethash cand-key table))
              (doc (or (plist-get cand :description)
                       (when-let ((full-ref (plist-get cand :nix-full-ref)))
                         (and (string-match-p "#" full-ref)
                              (+nix--fetch-meta-description full-ref))))))
    (when (buffer-live-p +eat-nushell-doc--last-buffer)
      (kill-buffer +eat-nushell-doc--last-buffer))
    (let ((buf (generate-new-buffer " *eat-nushell-doc*" t)))
      (with-current-buffer buf
        (insert doc))
      (setq +eat-nushell-doc--last-buffer buf)
      buf)))

(defun carapace-nushell-backend (action &optional arg &rest _)
  (let ((completion-prompt (carapace-nushell--raw-prompt (point))))
    (pcase action
      ('prefix (ignore-errors
                 (pcase-let* ((quote-count (s-count-matches "`" completion-prompt))
                              (quoted-prompt (if (cl-oddp quote-count)
                                                 (s-concat completion-prompt "`")
                                               completion-prompt))
                              (`(,beg ,end) (carapace-nushell--arg quoted-prompt))
                              (real-end (if (cl-oddp quote-count)
                                            (- end 1)
                                          end))
                              (prefix (buffer-substring-no-properties beg (point)))
                              (unquoted-prefix (s-replace-regexp "['\"`]" "" prefix))
                              (suffix (buffer-substring-no-properties (point) real-end)))
                   ;; Always return the full arg as the prefix.  Candidates
                   ;; include the boundary-prefix (e.g. "nixpkgs#hello"), so
                   ;; corfu/orderless filters cleanly against the user-typed
                   ;; text and on selection the whole arg gets replaced.
                   ;; Returning the stub-only would collapse the region to
                   ;; zero width, which `cape--dynamic-table' bails out on
                   ;; (cape.el:353) -- no candidates would be produced.
                   (list unquoted-prefix suffix))))
      ('candidates (when-let* ((candidates (carapace-nushell--completions completion-prompt)))
                     ;; Return every key; orderless / completion-styles will
                     ;; filter against the prefix.  Pre-filtering here is both
                     ;; redundant and wrong-shaped (substring vs. style).
                     (hash-table-keys candidates)))

      ('annotation (let* ((candidates (carapace-nushell--completions completion-prompt t))
                          (cand (gethash arg candidates)))
                     (plist-get cand :description)))
      ('doc-buffer (+eat-nushell-doc-buffer arg))
      ('post-completion
       ;; Read terminator preference BEFORE clearing the cache.  Each
       ;; candidate carries `:terminator' in its plist (set at table-build
       ;; time): `""' for paths/attrs (user is mid-path), `"#"' for flake
       ;; refs (drill into attr set), `" "' for everything else.  Falls
       ;; back to a space when no terminator was set.
       (let* ((cand (and carapace-nushell--active-completions
                         (gethash arg carapace-nushell--active-completions)))
              (terminator (or (plist-get cand :terminator) " ")))
         (setq-local carapace-nushell--active-completions nil)

         ;; remove the inserted completion for requoting
         (delete-char (- (length arg)))

         ;; clean up quotes that weren't part of the prefix but were part of the current arg
         (if (eql (char-before (point)) ?`) (delete-char -1))
         (if (eql (char-after (point)) ?`) (delete-char 1))

         (let* ((unquoted-arg (s-replace-regexp "['\"`]" "" (s-trim arg)))
                (requoted-arg (if (s-contains? " " unquoted-arg)
                                  (s-wrap unquoted-arg "`")
                                unquoted-arg)))
           (insert requoted-arg)
           ;; Re-route boundary chars that open a new completion context
           ;; through `unread-command-events' so the next command-loop
           ;; tick runs `self-insert-command' on them -- that's what
           ;; `corfu-auto-commands' matches, so the popup at the new
           ;; level fires naturally.  Direct `insert' would skip the
           ;; trigger.  Two flavors:
           ;;   - `terminator' is `#': we never inserted it (flake refs
           ;;     have no `#' in the candidate), just push it.
           ;;   - candidate already ends in `/' (fish directory): pop
           ;;     the slash back off so the synthetic self-insert can
           ;;     re-add it.
           (cond
            ((string-suffix-p "/" requoted-arg)
             (delete-char -1)
             (push ?/ unread-command-events))
            ((member terminator '("#"))
             (push (aref terminator 0) unread-command-events))
            (t (insert terminator)))))))))

(defun +eat-nushell-empty-arg-capf ()
  "Capf that handles the empty-arg position (e.g. `nix build ').
`cape--dynamic-table' guards on `(< beg end)' and never calls the
underlying completion function when the region is empty; pcomplete
(used in eshell) doesn't have this guard because it returns candidates
inline in the capf result.  This capf does the same: when the current
arg is empty, it pulls candidates from `carapace-nushell--completions'
directly and returns them inline so corfu can show them.  For non-empty
args this returns nil and the cape-wrapped backend handles things."
  (let* ((raw (carapace-nushell--raw-prompt (point)))
         (args (+eat-nushell--parse-args raw))
         (current-arg (or (car (last args)) "")))
    (when (and (string-empty-p current-arg)
               (>= (length args) 2))
      (when-let* ((table (carapace-nushell--completions raw))
                  (keys (hash-table-keys table)))
        (list (point) (point) keys
              :exclusive 'no
              :annotation-function
              (lambda (cand)
                (when-let* ((c (gethash cand table)))
                  (plist-get c :description)))
              :company-doc-buffer #'+eat-nushell-doc-buffer
              :exit-function
              (lambda (x _status)
                (carapace-nushell-backend 'post-completion x)))))))

(defun +eat-nushell--cache-valid-p (old new)
  "Return non-nil if the cape candidate cache for OLD remains valid for NEW.
Used as the cache-validation function passed to `cape-company-to-capf'.
The cache stays valid while OLD and NEW share the same `boundary
parent' -- the substring up to (and including) the last
`#'/`.'/`:'/`?'/`/'/`-'.  Within a boundary segment, orderless re-filters
the cached candidate set as the user types or backspaces; crossing a
boundary requires re-querying because the candidate SET, not just the
visible subset, changes.  Examples that should invalidate:
  `nixpkgs#hello.dependencies' -> `nixpkgs#hello' (cross `.')
  `nixpkgs#hello'              -> `'              (cross `#')
  `/etc/passwd'                -> `/var'          (cross `/')
  `nixpkgs#hello'              -> `-'             (cross into flag context)
  `-h'                         -> `--'            (short flag -> long flag)"
  (cl-flet ((parent (s)
              (let ((p (string-match "[-#.:?/][^-#.:?/]*\\'" s)))
                (if p (substring s 0 (1+ p)) ""))))
    (equal (parent old) (parent new))))

(defun replace-eat-completions ()
  (fish-completion-mode -1)
  (corfu-mode +1)
  (setq-local completion-at-point-functions
              ;; Two capfs in order:
              ;; 1. `+eat-nushell-empty-arg-capf' fires only when the partial
              ;;    arg is empty (e.g. `nix build ') -- cape's dynamic table
              ;;    won't help us there because it guards on `(< beg end)'.
              ;; 2. The cape-wrapped company backend handles everything else,
              ;;    with descriptions and lazy doc-buffer.
              (list #'+eat-nushell-empty-arg-capf
                    (cape-company-to-capf #'carapace-nushell-backend
                                          #'+eat-nushell--cache-valid-p))))
