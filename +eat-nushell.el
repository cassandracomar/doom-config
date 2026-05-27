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

(defvar +eat-nushell--commands nil
  "Hash table mapping nushell command name -> plist with command details.
Populated once via a background `nu -c \"scope commands | to json\"' call.
Each entry mirrors a row from `scope commands' (name, signatures,
description, examples, type, etc.) so we can drive command, flag, and
argument completion off a single source.")

(defvar +eat-nushell--commands-loading nil
  "Live process while the command cache is being populated, or nil.")

(defun +eat-nushell--escape-control-chars-in-strings ()
  "Repair JSON in the current buffer by escaping in-string control bytes.
nushell's `to json' emits raw 0x00-0x1F bytes (e.g. ESC inside OSC 8
hyperlinks in command descriptions) which Emacs's `json-parse-buffer'
rejects.  Walk the buffer, track whether we're inside a quoted string,
and rewrite any control byte found inside one as its `\\uXXXX' escape."
  (goto-char (point-min))
  (let ((in-string nil))
    (while (not (eobp))
      (let ((c (char-after)))
        (cond
         ((and in-string (eq c ?\\))
          (forward-char 2))
         ((eq c ?\")
          (setq in-string (not in-string))
          (forward-char 1))
         ((and in-string (<= c #x1f))
          (delete-char 1)
          (insert (format "\\u%04x" c)))
         (t (forward-char 1)))))))

(defun +eat-nushell--ingest-commands (proc-buf)
  "Parse `scope commands | to json' output in PROC-BUF into the cache.
Uses :keyword keys so callers can `(plist-get cmd :name)' etc."
  (with-current-buffer proc-buf
    (+eat-nushell--escape-control-chars-in-strings)
    (goto-char (point-min))
    (let* ((cmds (ignore-errors
                   (json-parse-buffer
                    :object-type 'plist
                    :array-type 'list
                    :null-object nil
                    :false-object nil))))
      (when cmds
        (let ((tbl (make-hash-table :test 'equal :size (length cmds))))
          (dolist (c cmds)
            (puthash (plist-get c :name) c tbl))
          (setq +eat-nushell--commands tbl))))))

(defcustom +eat-nushell-commands-source-files
  '("~/.config/nushell/emacs-config.nu")
  "Nushell files to `source' before sampling `scope commands'.
These should expose any user-defined commands (e.g. `git status',
`alias ls = eza', `nh os upgrade') that the eat-launched nushell would
see, so completion offers them too.  Defaults to `emacs-config.nu',
the same entry point eat passes via `--config' -- it chains in
`config.nu', `default-config.nu', and `eat-config.nu', so every alias
and def in your interactive shell is also in the cache.

Anything that needs a TTY (e.g. `eat enable_integration's
`term query') will detect `$nu.is-interactive' as false in this
non-interactive `nu -c' invocation and short-circuit, so listing the
full chain here is safe."
  :type '(repeat string)
  :group '+eat-nushell)

(defun +eat-nushell-load-commands (&optional callback)
  "Load `scope commands' into `+eat-nushell--commands'.
Pre-sources `+eat-nushell-commands-source-files' so user-defined
commands (aliases, defs from your `~/.config/nushell/eat-config.nu')
end up in the cache.  Runs synchronously -- the nushell call typically
finishes in ~50ms, far cheaper than the bookkeeping for async.
Interactively (or with prefix arg) forces a refresh even if the cache
is already populated.  CALLBACK is invoked at the end if non-nil
(matches the old async signature so existing callers keep working).

Spawns nu under a PTY (`:connection-type 'pty') so `tty' and other
TTY-dependent forms in `config.nu' (`gpg-connect-agent', etc.) don't
abort the source chain.  Without a PTY they fail silently and skip
the rest of the config -- losing the `default-config.nu' defs/aliases
(`git cc', `nh os upgrade', `alias ls = eza', ...) we want cached."
  (interactive)
  (unless (and (not current-prefix-arg) +eat-nushell--commands)
    (let* ((source-stmts
            (mapconcat (lambda (f) (format "source %s" (expand-file-name f)))
                       +eat-nushell-commands-source-files
                       "; "))
           (nu-cmd (if (string-empty-p source-stmts)
                       "scope commands | to json"
                     (format "%s; scope commands | to json" source-stmts)))
           (proc-buf (generate-new-buffer " *eat-nushell-commands*" t))
           (proc (make-process
                  :name "eat-nushell-commands"
                  :buffer proc-buf
                  :command (list (executable-find "nu") "-c" nu-cmd)
                  :connection-type 'pty
                  :noquery t)))
      (unwind-protect
          (progn
            (while (process-live-p proc)
              (accept-process-output proc 0.1))
            (when (and (eq (process-exit-status proc) 0)
                       (> (with-current-buffer proc-buf (buffer-size)) 0))
              (+eat-nushell--ingest-commands proc-buf)))
        (when (buffer-live-p proc-buf) (kill-buffer proc-buf)))
      (when callback (funcall callback)))))

;; Kick the cache off as soon as this file loads -- it takes ~50ms so we
;; lose nothing by starting at startup, and the result is ready by the
;; time the user has an eat buffer to complete in.  Safe to call again
;; via `M-x +eat-nushell-load-commands' with a prefix arg to refresh.
(when (executable-find "nu")
  (+eat-nushell-load-commands))

(setq carapace-completion-command (executable-find "carapace"))
(defun carapace-completion--fish-fallback (raw-prompt)
  "Return a hash-table of fish completions for RAW-PROMPT.
Extracts per-candidate descriptions so they flow into the `annotation'
case of `carapace-nushell-backend' (and on to corfu / corfu-popupinfo).
Two sources are checked, in order:
1. The `pcomplete-help' text property, set by `+fish-completion--list-completions-a'
   in `+eshell.el' when that override is loaded.
2. A literal `name<TAB>description' split, in case the override isn't active
   yet (e.g. fish-completion hasn't been required by the eshell side).

Candidates beginning with `$' are dropped: those are bash-style env-var
references (`$PATH', `$HOME', ...) that don't exist in nushell, where env
access is via `$env.PATH'.  Our own `+eat-nushell-env-capf' supplies the
correct names; letting fish polute the completion table here just
confuses corfu."
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
                   (unless (string-prefix-p "$" name)
                     (puthash (concat name " ")
                              `(:display ,name :value ,name :terminator ,terminator
                                ,@(and desc (not (string-empty-p desc))
                                       (list :description desc)))
                              table)))
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

(defun +eat-nushell--active-segment-via-ast ()
  "Return (BEG . END) of the active pipe_element using the live nu AST.
END is point.  BEG is the start of the innermost enclosing
`pipe_element'; if no `pipe_element' contains point yet (e.g. the user
just typed `{' or `|' and the parser inserted a MISSING node), falls
back to the innermost `block' / `val_closure' / `nu_script' so command
completion still anchors at the right scope.  nil when the parser
isn't ready.

Looking up the node at point fails when point sits at the END of the
input, because the parser tends to anchor MISSING nodes there
(closing brace not yet typed).  Falling back to `(1- pt)' gets the
node for the character we just typed, which is what we actually want."
  (when (and (bound-and-true-p +eat/-nu-parser)
             (treesit-parser-p +eat/-nu-parser)
             (memq +eat/-nu-parser (treesit-parser-list)))
    (let* ((pt (point))
           (lookup-pos (if (> pt (point-min)) (1- pt) pt))
           (cur (treesit-node-at lookup-pos 'nu))
           best)
      (while cur
        (let ((type (treesit-node-type cur)))
          (cond
           ((equal type "pipe_element")
            (setq best (treesit-node-start cur))
            (setq cur nil))
           ((and (not best) (member type '("block" "val_closure" "nu_script")))
            (setq best (treesit-node-start cur))
            (setq cur (treesit-node-parent cur)))
           (t
            (setq cur (treesit-node-parent cur))))))
      (when best
        (cons best pt)))))

(defun +eat-nushell--active-pipeline-segment (raw-prompt)
  "Return the substring of RAW-PROMPT for the active command position.
Prefer the tree-sitter AST when available (see
`+eat-nushell--active-segment-via-ast'); fall back to a string scan
that splits on `|', `&&', `;', `{', `(' outside quotes.  Leading
whitespace is trimmed; trailing is preserved so a trailing space still
signals a fresh-arg position."
  (let ((ast-region (+eat-nushell--active-segment-via-ast)))
    (cond
     ((and ast-region
           ;; Sanity: only trust the AST if its beg fits inside this
           ;; raw-prompt's line.  Raw-prompt comes from
           ;; `(comint-line-beginning-position)' upwards, AST coordinates
           ;; are absolute buffer positions -- map them through.
           (>= (car ast-region) (comint-line-beginning-position)))
      (let* ((line-beg (comint-line-beginning-position))
             (offset (- (car ast-region) line-beg)))
        (when (<= offset (length raw-prompt))
          (replace-regexp-in-string
           "\\`[ \t]+" "" (substring raw-prompt offset)))))
     (t
      (let ((len (length raw-prompt))
            (i 0)
            (in-quote nil)
            (seg-start 0))
        (while (< i len)
          (let ((c (aref raw-prompt i)))
            (cond
             (in-quote
              (cond
               ((eq c ?\\) (cl-incf i 2))
               ((eq c in-quote) (setq in-quote nil) (cl-incf i))
               (t (cl-incf i))))
             ((memq c '(?\' ?\" ?\`))
              (setq in-quote c)
              (cl-incf i))
             ((memq c '(?| ?\; ?\{ ?\())
              (setq seg-start (1+ i))
              (cl-incf i))
             ((and (eq c ?&)
                   (< (1+ i) len)
                   (eq (aref raw-prompt (1+ i)) ?&))
              (setq seg-start (+ i 2))
              (cl-incf i 2))
             (t (cl-incf i)))))
        (replace-regexp-in-string
         "\\`[ \t]+" "" (substring raw-prompt seg-start)))))))

(defun +eat-nushell--parse-args (raw-prompt)
  "Return the list of args parsed from RAW-PROMPT.
Splits on whitespace, dropping leading/trailing whitespace; if RAW-PROMPT
ends in whitespace, appends \"\" so the caller can treat the position
after that whitespace as a fresh arg under completion.  Pipeline
separators (`|', `&&', `;') reset the parse: only the active segment
contributes args, so `ls | from <cursor>' yields `(\"from\" \"\")'."
  (let* ((segment (+eat-nushell--active-pipeline-segment raw-prompt))
         (tokens (split-string segment "[ \t]+" t)))
    (if (string-match-p "[ \t]\\'" segment)
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

(defun +eat-nushell--has-descendants-p (head)
  "Return non-nil if any cached command starts with HEAD + a space.
Used to recognise multi-level prefixes like `nh home' even when the
prefix itself isn't a real command -- only `nh home upgrade' is in the
cache, but `nh home' is still a meaningful subcommand position to
complete from."
  (when +eat-nushell--commands
    (let ((prefix (concat head " "))
          (found nil))
      (catch 'done
        (maphash (lambda (name _)
                   (when (string-prefix-p prefix name)
                     (setq found t)
                     (throw 'done nil)))
                 +eat-nushell--commands))
      found)))

(defun +eat-nushell--head-command (args)
  "Return the longest prefix of ARGS that's a recognised nu command head.
A prefix counts as recognised if it's a real entry in
`+eat-nushell--commands' OR if any cached command starts with that
prefix (multi-level case: `nh home' isn't itself a command, but
`nh home upgrade' is, so `nh home' is still a valid completion anchor).
Falls back to (car args) when nothing matches; nil if the cache
isn't ready or args is empty."
  (when (and +eat-nushell--commands args)
    (let ((found nil))
      (cl-loop for n downfrom (length args) to 1
               for candidate = (mapconcat #'identity (cl-subseq args 0 n) " ")
               when (or (gethash candidate +eat-nushell--commands)
                        (+eat-nushell--has-descendants-p candidate))
               return (setq found candidate))
      (or found (car args)))))

(defun +eat-nushell--subcommand-candidates (head)
  "Return alist of (suffix . cmd-plist) for nu commands starting with HEAD + \" \".
Each suffix is the FIRST word after HEAD; e.g. for HEAD=\"git\" you get
\"status\", \"log\", \"cc\".  For commands nested deeper (\"nh os
upgrade\"), the intermediate prefix (\"os\") is offered too so
completion can drill down stepwise -- the plist for an intermediate is
a fresh prefix marker rather than a real `scope commands' entry.
Empty list when nothing matches."
  (when +eat-nushell--commands
    (let ((prefix (concat head " "))
          (table (make-hash-table :test #'equal)))
      (maphash
       (lambda (name cmd)
         (when (string-prefix-p prefix name)
           (let* ((tail (substring name (length prefix)))
                  (space (string-match " " tail)))
             (if space
                 ;; Multi-word descendant: surface the first-word prefix once.
                 (let ((first-word (substring tail 0 space)))
                   (unless (gethash first-word table)
                     (puthash first-word
                              (list :description
                                    (format "%s subcommand prefix"
                                            (concat prefix first-word)))
                              table)))
               ;; Direct child: use the real cmd plist.
               (puthash tail cmd table)))))
       +eat-nushell--commands)
      (let (out)
        (maphash (lambda (k v) (push (cons k v) out)) table)
        out))))

(defun +eat-nushell--flag-candidates (head)
  "Return alist of (FLAG-STRING . cmd-plist) for the named/switch params of HEAD.
HEAD must be a known nu command name; missing or unknown returns nil.
Each FLAG-STRING is `--<long>' (or `-<short>' if no long form exists).
The returned plist for the entry is a fresh plist containing the
parameter's :description so the completion UI can surface it; the full
cmd is fetched lazily by callers via `+eat-nushell--commands' if needed."
  (when-let* ((+eat-nushell--commands)
              (cmd (gethash head +eat-nushell--commands))
              (sigs (plist-get cmd :signatures)))
    (let (out)
      ;; Each value under :signatures is a list of parameter plists; iterate
      ;; them all in case different input shapes expose different flags.
      (cl-loop for (_input-shape params) on sigs by #'cddr
               do (dolist (param params)
                    (let ((kind (plist-get param :parameter_type))
                          (long (plist-get param :parameter_name))
                          (short (plist-get param :short_flag))
                          (desc  (plist-get param :description)))
                      (when (member kind '("named" "switch"))
                        (cond
                         ((and long (> (length long) 0))
                          (push (cons (concat "--" long)
                                      (list :description desc :short short))
                                out))
                         ((and short (> (length short) 0))
                          (push (cons (concat "-" short)
                                      (list :description desc))
                                out))))))
               finally return (cl-delete-duplicates out :key #'car :test #'equal)))))

(defun +eat-nushell--root-prefixes ()
  "Return list of distinct first-word prefixes used by multi-word commands.
E.g. \"git\", \"eat\", \"from\", \"to\" for `git status', `eat tee', etc."
  (when +eat-nushell--commands
    (let (set)
      (maphash (lambda (name _)
                 (when-let ((sp (string-match " " name)))
                   (push (substring name 0 sp) set)))
               +eat-nushell--commands)
      (cl-delete-duplicates set :test #'equal))))

(defun +eat-nushell--flag-value-completions (head flag-name)
  "Return the list of value candidates declared for HEAD's FLAG-NAME.
Each `scope commands' parameter can carry a `:completion' field listing
enum-style values (e.g. `--theme' for `table' has [basic compact ...]).
Returns nil when HEAD isn't a known command or the flag has no enum."
  (when-let* ((cmd (gethash head +eat-nushell--commands))
              (sigs (plist-get cmd :signatures)))
    (catch 'found
      (cl-loop for (_input params) on sigs by #'cddr do
               (dolist (p params)
                 (when (equal (plist-get p :parameter_name) flag-name)
                   (when-let ((vals (plist-get p :completion)))
                     (throw 'found vals))))))))

(defun +eat-nushell--previous-flag-name (args)
  "If the arg before the current one looks like `--flag', return the flag name.
Returns the stripped name (no leading dashes); nil otherwise.  Used to
detect `cmd --flag <cursor>' positions where the next argument is a
named-parameter value."
  (when (>= (length args) 2)
    (let ((prev (nth (- (length args) 2) args)))
      (cond
       ((and (string-prefix-p "--" prev) (> (length prev) 2))
        (substring prev 2))
       ((and (string-prefix-p "-" prev) (= (length prev) 2)
             ;; Map short flag back to long via flag-candidates -- we don't
             ;; have a short->long index, so skip for now; named values are
             ;; almost always invoked with --long form anyway.
             nil)
        nil)))))

(defun +eat-nushell--add-var-candidates (table)
  "Add `$name' candidates to TABLE for the nu built-ins + in-scope locals.
Variables are syntactically valid wherever a value is expected -- both
command and argument positions -- so we surface them in both pools and
let orderless filter on the typed prefix.  Skips entries that already
exist in TABLE so earlier (more authoritative) candidates win."
  (cl-flet ((add (name desc)
              (let ((display (concat "$" name)))
                (unless (gethash (concat display " ") table)
                  (puthash (concat display " ")
                           `(:display ,display :value ,display :terminator " "
                             :description ,desc)
                           table)))))
    (dolist (v +eat-nushell--builtin-vars)
      (add v "nu built-in variable"))
    (dolist (v (+eat-nushell--local-var-names))
      (add v "local variable"))))

(defun +eat-nushell--builtin-completions (prompt)
  "Compute completion candidates contributed by the nu builtin cache.
Returns a hashtable shaped like `carapace-completion--fish-fallback' --
keys are `\"<value> \"' (display + trailing space), values are plists
with :display :value :terminator :description.  Empty hashtable if the
cache is cold or the prompt position doesn't admit builtin completions.

Local + built-in nu variables (`$foo', `$env', `$in', `$it', `$nu')
are added to both command and argument positions so they show up
alongside command/path candidates without requiring a `$' prefix to
trigger; orderless does the visual filtering as the user types."
  (let ((table (make-hash-table :test #'equal))
        (args (+eat-nushell--parse-args prompt))
        (current-arg (or (car (last (+eat-nushell--parse-args prompt))) "")))
    (when (and +eat-nushell--commands (> (length args) 0))
      (cond
       ;; Flag position: `cmd ... -<cursor>' or `cmd ... --<cursor>'.  Look up
       ;; the longest command prefix that's a builtin and offer its flags.
       ((string-prefix-p "-" current-arg)
        (let* ((cmd-args (butlast args))
               (head (+eat-nushell--head-command cmd-args)))
          (dolist (flag (+eat-nushell--flag-candidates head))
            (puthash (concat (car flag) " ")
                     `(:display ,(car flag) :value ,(car flag)
                       :terminator " "
                       ,@(when-let ((d (plist-get (cdr flag) :description)))
                           (list :description d)))
                     table))))
       ;; Flag-value position: `cmd --flag <cursor>'.  If the previous arg is
       ;; a known long flag with `:completion' enum values, offer them.
       ((when-let* ((flag-name (+eat-nushell--previous-flag-name args))
                    (cmd-args (butlast args 2))
                    (head (+eat-nushell--head-command cmd-args))
                    (values (+eat-nushell--flag-value-completions
                             head flag-name)))
          (dolist (v values)
            (puthash (concat v " ")
                     `(:display ,v :value ,v :terminator " "
                       :description ,(format "value for --%s" flag-name))
                     table))
          t))
       ;; First-arg position: still completing the head command.  Offer
       ;; single-word builtins plus root prefixes of multi-word commands
       ;; (e.g. `git', `eat') so the user can drill into subcommands, AND
       ;; in-scope variables (locals + nu builtins) -- a value reference
       ;; is syntactically valid here (`$x | ...').
       ((= (length args) 1)
        (maphash
         (lambda (name cmd)
           (unless (string-match-p " " name)
             (puthash (concat name " ")
                      `(:display ,name :value ,name :terminator " "
                        ,@(when-let ((d (plist-get cmd :description)))
                            (list :description d)))
                      table)))
         +eat-nushell--commands)
        (dolist (root (+eat-nushell--root-prefixes))
          (unless (gethash (concat root " ") table)
            (puthash (concat root " ")
                     `(:display ,root :value ,root :terminator " ")
                     table)))
        (+eat-nushell--add-var-candidates table))
       ;; Subcommand / positional-argument position: `cmd <cursor>' where
       ;; `cmd' has multi-word children OR positional args.  Offer
       ;; subcommands AND in-scope vars (variables are valid args).
       (t
        (let* ((cmd-args (butlast args))
               (head (+eat-nushell--head-command cmd-args)))
          (dolist (sub (+eat-nushell--subcommand-candidates head))
            (puthash (concat (car sub) " ")
                     `(:display ,(car sub) :value ,(car sub) :terminator " "
                       ,@(when-let ((d (plist-get (cdr sub) :description)))
                           (list :description d)))
                     table)))
        (+eat-nushell--add-var-candidates table))))
    table))

(defun +eat-nushell--merge-tables (primary secondary)
  "Merge SECONDARY into PRIMARY (a hash table), returning PRIMARY.
Entries already in PRIMARY win; missing keys are filled in from SECONDARY.
Used to layer nu builtins on top of fish-completion fallback results."
  (maphash (lambda (k v)
             (unless (gethash k primary)
               (puthash k v primary)))
           secondary)
  primary)

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
      ;; Layer nu-builtin candidates on top: fish wins where it has an entry
      ;; (it knows external commands, paths, etc.); we add nu's commands,
      ;; subcommands, and flags where fish has nothing.
      (+eat-nushell--merge-tables candidates
                                  (+eat-nushell--builtin-completions prompt))
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
           ;; Re-route the terminator through `unread-command-events' so
           ;; the next command-loop tick runs `self-insert-command' on it
           ;; -- that's what `corfu-auto-commands' matches, so the popup
           ;; at the next level fires naturally.  Direct `insert' skips
           ;; the trigger and leaves the user without subcommand/arg
           ;; completion until they type another char.  Three flavors:
           ;;   - candidate already ends in `/' (fish directory): pop
           ;;     the slash back off so the synthetic self-insert can
           ;;     re-add it and trigger completion.
           ;;   - `terminator' is `#': never inserted (flake refs don't
           ;;     have `#' in the candidate), so just push it.
           ;;   - everything else (including the default space): push
           ;;     the terminator char so corfu re-triggers for the next
           ;;     argument / subcommand position.
           (cond
            ((string-suffix-p "/" requoted-arg)
             (delete-char -1)
             (push ?/ unread-command-events))
            ((> (length terminator) 0)
             (push (aref terminator 0) unread-command-events)))))))))

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

(defun +eat-nushell--shell-pid ()
  "Return the PID of the live nu process in this eat buffer, or nil."
  (when-let* ((term (bound-and-true-p eat-terminal))
              (proc (eat-term-parameter term 'eat--process)))
    (and (process-live-p proc) (process-id proc))))

(defun +eat-nushell--env-names ()
  "Return the env-var names set in this eat buffer's live nu process.
Reads /proc/<pid>/environ; on Linux the kernel refreshes that file
when the process mutates its environment (e.g. `$env.X = ...',
`load-env'), so `cd' + direnv changes show up between completions."
  (when-let* ((pid (+eat-nushell--shell-pid))
              (path (format "/proc/%s/environ" pid))
              ((file-readable-p path)))
    (with-temp-buffer
      (insert-file-contents path)
      (let (out)
        (dolist (e (split-string (buffer-string) "\0" t))
          (when-let ((eq-pos (string-search "=" e)))
            (push (substring e 0 eq-pos) out)))
        (nreverse out)))))

(defun +eat-nushell--local-var-names ()
  "Collect nu var names in scope at point inside the line-mode input.
Walks ancestors of point and gathers binders by lexical scope:
  - `let' / `mut' / `const' declared earlier in the same enclosing block
  - `parameter' names (closure / def args) when point is inside the body
  - `for' loop var when point is inside the for body
Returns nil if the parser isn't ready or no bindings are in scope."
  (when (and (bound-and-true-p +eat/-nu-parser)
             (treesit-parser-p +eat/-nu-parser)
             (memq +eat/-nu-parser (treesit-parser-list)))
    (let ((pt (point))
          (names nil))
      (cl-labels
          ((collect-id (node)
             (when node
               (push (treesit-node-text node t) names)))
           (collect-params-of (parent)
             (when-let ((params (treesit-node-child-by-field-name
                                 parent "parameters")))
               (dolist (p (treesit-node-children params))
                 (when (equal (treesit-node-type p) "parameter")
                   (collect-id (treesit-node-child-by-field-name
                                p "param_name"))))))
           (point-in (node field)
             (when-let ((b (treesit-node-child-by-field-name node field)))
               (and (>= pt (treesit-node-start b))
                    (<= pt (treesit-node-end b))))))
        (let ((cur (treesit-node-at pt 'nu)))
          (while cur
            (pcase (treesit-node-type cur)
              ;; Top-level script or any block: collect let/mut/const that
              ;; ended before point at this level.
              ((or "nu_script" "block")
               (dolist (child (treesit-node-children cur))
                 (when (and (member (treesit-node-type child)
                                    '("stmt_let" "stmt_mut" "stmt_const"))
                            (< (treesit-node-end child) pt))
                   (collect-id (treesit-node-child-by-field-name
                                child "var_name")))))
              ;; Closure: params bind only inside the body (the body is a
              ;; child block; we already walk into it via `treesit-node-at',
              ;; so being inside this val_closure means point is in body).
              ("val_closure"
               (collect-params-of cur))
              ;; `for x in ... { ... }': bind `x' only when point is in body.
              ("ctrl_for"
               (when (point-in cur "body")
                 (collect-id (treesit-node-child-by-field-name cur "var_name"))))
              ;; `def foo [a b] { ... }': bind params only in body.
              ("decl_def"
               (when (point-in cur "body")
                 (collect-params-of cur))))
            (setq cur (treesit-node-parent cur)))))
      (cl-delete-duplicates (cl-remove nil names) :test #'equal))))

(defconst +eat-nushell--builtin-vars '("env" "in" "it" "nu")
  "Nushell built-in variables that are always valid wherever `$x' is.
`$env' is the env-var table, `$in' is the input pipeline value,
`$it' is the legacy single-element shorthand still accepted in some
contexts, `$nu' is the process metadata record.  Surfaced alongside
lexically-scoped locals so the user doesn't have to remember which
class a name belongs to.")

(defun +eat-nushell-local-var-capf ()
  "Capf for `$var' completions: nu locals plus the built-in `$env'/`$in'/...
Fires whenever the partial starts with `$' -- works in both command
position (e.g. `($local<TAB>)') and argument position (e.g. `cmd
$local<TAB>'), since either is a valid use site for a variable
reference.  Skips when the partial is `$env.X' (that case is owned by
`+eat-nushell-env-capf' which sees the live process env)."
  (let ((line-beg (comint-line-beginning-position)))
    (save-excursion
      (let* ((end (point))
             (name-beg (save-excursion
                         (skip-chars-backward "A-Za-z0-9_-" line-beg)
                         (point))))
        (when (and (> name-beg line-beg)
                   (eq (char-before name-beg) ?$)
                   ;; Skip `$env.X' -- env-capf handles it.
                   (not (save-excursion
                          (goto-char name-beg)
                          (looking-at "env\\."))))
          (let* ((locals (+eat-nushell--local-var-names))
                 (candidates
                  (append +eat-nushell--builtin-vars
                          (cl-set-difference
                           locals +eat-nushell--builtin-vars
                           :test #'equal))))
            (list name-beg end candidates
                  :exclusive 'no
                  :annotation-function
                  (lambda (c)
                    (if (member c +eat-nushell--builtin-vars) " nu-builtin" " local"))
                  :company-kind (lambda (_) 'variable))))))))

(defun +eat-nushell-env-capf ()
  "Capf that completes env-var names after `$env.'.
Reads the live nu process's environment via `/proc/<pid>/environ', so
runtime env mutations (assignments, hook-driven loads, direnv) are
reflected.  The capf is non-exclusive: when the user is mid-name,
`completion-at-point' will also consult downstream capfs, but env
matches sort first because we own the local region."
  (let ((line-beg (comint-line-beginning-position)))
    (save-excursion
      ;; Walk back from point until we leave the current identifier.
      (let* ((end (point))
             (name-beg (save-excursion
                         (skip-chars-backward "A-Za-z0-9_" line-beg)
                         (point))))
        (when (and (>= name-beg (+ line-beg 5))
                   (string= (buffer-substring-no-properties
                             (- name-beg 5) name-beg)
                            "$env."))
          (when-let ((names (+eat-nushell--env-names)))
            (list name-beg end names
                  :exclusive 'no
                  :annotation-function (lambda (_) " env")
                  :company-kind (lambda (_) 'variable))))))))

(defun replace-eat-completions ()
  (fish-completion-mode -1)
  (corfu-mode +1)
  (setq-local completion-at-point-functions
              ;; Four capfs in order:
              ;; 1. `+eat-nushell-env-capf' completes `$env.X' from the live
              ;;    process's `/proc/<pid>/environ'.
              ;; 2. `+eat-nushell-local-var-capf' completes `$var' from the
              ;;    input region's let/mut/const/def/closure-param names,
              ;;    plus `env' so `$env.<X>' is always reachable in one go.
              ;; 3. `+eat-nushell-empty-arg-capf' fires only when the partial
              ;;    arg is empty (e.g. `nix build ') -- cape's dynamic table
              ;;    won't help us there because it guards on `(< beg end)'.
              ;; 4. The cape-wrapped company backend handles everything else,
              ;;    layering fish externals + nu builtins/subcommands/flags.
              (list #'+eat-nushell-env-capf
                    #'+eat-nushell-local-var-capf
                    #'+eat-nushell-empty-arg-capf
                    (cape-company-to-capf #'carapace-nushell-backend
                                          #'+eat-nushell--cache-valid-p))))
