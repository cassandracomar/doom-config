;;; +eat-nushell.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'comint)
(require 'json)
(require 'ring)
(require 'subr-x)

(defvar +eat-nushell-shared-history-ring nil
  "Single input ring shared across all eat buffers running nushell.")

(defun +eat-nushell--nushell-buffer-p ()
  "Heuristic for whether this eat buffer is running nushell."
  (and (bound-and-true-p eat-shell)
       (string-match-p "\\bnu\\b" eat-shell)))

(defun +eat-nushell--share-input-ring (&rest _)
  "Share Nushell input history between Eat buffers.
Run after `eat--line-populate-input-ring' so Eat has already loaded the
history file before the rings are merged."
  (when (and (bound-and-true-p eat--line-input-ring)
             (+eat-nushell--nushell-buffer-p))
    (cond
     ((null +eat-nushell-shared-history-ring)
      (setq +eat-nushell-shared-history-ring eat--line-input-ring))
     (t
      (let ((seen (make-hash-table :test 'equal)))
        (dolist (cmd (ring-elements +eat-nushell-shared-history-ring))
          (puthash cmd t seen))
        (dolist (cmd (nreverse (ring-elements eat--line-input-ring)))
          (unless (gethash cmd seen)
            (ring-insert +eat-nushell-shared-history-ring cmd))))))
    (setq-local eat--line-input-ring +eat-nushell-shared-history-ring)))

(advice-add 'eat--line-populate-input-ring :after #'+eat-nushell--share-input-ring)

(defcustom +eat-nushell-commands-source-files
  '("~/.config/nushell/emacs-config.nu")
  "Nushell configuration entry points visible to Eat.
The first entry is passed to `nu --config' for completion queries."
  :type '(repeat string)
  :group '+eat-nushell)

(defconst +eat-nushell--json-begin "__EAT_NUSHELL_JSON_BEGIN__")
(defconst +eat-nushell--json-end "__EAT_NUSHELL_JSON_END__")

(defun +eat-nushell--escape-control-chars-in-strings ()
  "Escape raw JSON control bytes inside strings in the current buffer."
  (goto-char (point-min))
  (let ((in-string nil))
    (while (not (eobp))
      (let ((char (char-after)))
        (cond
         ((and in-string (eq char ?\\))
          (forward-char 2))
         ((eq char ?\")
          (setq in-string (not in-string))
          (forward-char 1))
         ((and in-string (<= char #x1f))
          (delete-char 1)
          (insert (format "\\u%04x" char)))
         (t
          (forward-char 1)))))))

(defun +eat-nushell--parse-json (json)
  "Parse JSON after repairing raw control characters in strings."
  (with-temp-buffer
    (insert json)
    (+eat-nushell--escape-control-chars-in-strings)
    (goto-char (point-min))
    (json-parse-buffer
     :object-type 'plist
     :array-type 'list
     :null-object nil
     :false-object nil)))

(defun +eat-nushell--config-json (expression &optional environment)
  "Evaluate EXPRESSION under the configured Nushell and decode its JSON.
Return `(:ok t :value VALUE)' on success and nil when Nu fails or emits
an invalid payload.  ENVIRONMENT is a list of additional process
environment entries."
  (when-let* ((nu (executable-find "nu"))
              (config-file (car +eat-nushell-commands-source-files)))
    (let* ((config (expand-file-name config-file))
           (command
            (format
             (concat "let eat_result = (%s); "
                     "print -n '%s'; "
                     "$eat_result | to json -r | print -n; "
                     "print -n '%s'")
             expression +eat-nushell--json-begin +eat-nushell--json-end))
           (process-environment (append environment process-environment))
           (proc-buf (generate-new-buffer " *eat-nushell-json*" t))
           (proc (make-process
                  :name "eat-nushell-json"
                  :buffer proc-buf
                  :command (list nu "--config" config "--no-history" "-c" command)
                  :connection-type 'pty
                  :coding 'utf-8-unix
                  :noquery t)))
      (unwind-protect
          (progn
            (while (process-live-p proc)
              (accept-process-output proc 0.1))
            (when (eq (process-exit-status proc) 0)
              (with-current-buffer proc-buf
                (let* ((output (buffer-string))
                       (begin (string-match
                               (regexp-quote +eat-nushell--json-begin)
                               output))
                       (payload-start
                        (and begin (+ begin (length +eat-nushell--json-begin))))
                       (end (and payload-start
                                 (string-match
                                  (regexp-quote +eat-nushell--json-end)
                                  output payload-start))))
                  (when end
                    (condition-case nil
                        (list :ok t
                              :value (+eat-nushell--parse-json
                                      (substring output payload-start end)))
                      (error nil)))))))
        (when (buffer-live-p proc-buf)
          (kill-buffer proc-buf))))))

(defun +eat-nushell--raw-prompt (&optional end)
  "Return the current Eat input through END, or through point."
  (buffer-substring-no-properties
   (comint-line-beginning-position)
   (or end (point))))

(defun +eat-nushell--prompt-byte-substring (prompt start end)
  "Return the UTF-8 byte range START..END from PROMPT as an Emacs string."
  (decode-coding-string
   (substring (encode-coding-string prompt 'utf-8) start end)
   'utf-8))

(defun +eat-nushell--args-from-flat-ast (prompt rows)
  "Return argv for the active command in PROMPT from flattened AST ROWS.
The last call node is the command at the cursor.  Deduplicate semantic
rows sharing a source span and restore a trailing empty argument, which
has no AST node of its own."
  (let ((index 0)
        call-index)
    (dolist (row rows)
      (when (member (plist-get row :shape)
                    '("shape_external" "shape_internalcall"))
        (setq call-index index))
      (cl-incf index))
    (when call-index
      (let ((seen (make-hash-table :test #'equal))
            args)
        (dolist (row (nthcdr call-index rows))
          (when-let* ((span (plist-get row :span))
                      (start (plist-get span :start))
                      (end (plist-get span :end))
                      (content (plist-get row :content))
                      ((stringp content)))
            (let ((key (cons start end)))
              (unless (gethash key seen)
                (puthash key t seen)
                (setq args (nconc args (list content)))))))
        (when (string-match-p "[ \t]\\'" prompt)
          (setq args (append args (list ""))))
        args))))

(defun +eat-nushell--nix-completion-header (args)
  "Return Nix's completion-type header for the current position in ARGS."
  (when-let* (((equal (car args) "nix"))
              ((> (length args) 1))
              (nix (executable-find "nix")))
    (let* ((n (1- (length args)))
           (current-arg (or (nth n args) ""))
           (stub-start (let ((pos (string-match
                                   "[#.:?][^#.:?]*\\'" current-arg)))
                         (if pos (1+ pos) 0)))
           (stub-prefix (substring current-arg 0 stub-start))
           (query-args (append (cl-subseq args 1 n) (list stub-prefix)))
           (process-environment
            (cons (format "NIX_GET_COMPLETIONS=%d" n) process-environment)))
      (with-temp-buffer
        (when (zerop (apply #'call-process nix nil t nil query-args))
          (goto-char (point-min))
          (buffer-substring-no-properties (point) (line-end-position)))))))

(defun +eat-nushell--completion-terminator (args nix-header value normalized kind)
  "Choose the synthetic terminator for a completed VALUE after ARGS."
  (cond
   ((string-suffix-p "/" normalized) "")
   ((or (equal kind "cell-path")
        (equal kind "variable")
        (string-prefix-p "$" value))
    "")
   ((equal (car args) "nix")
    (pcase nix-header
      ("attrs" "")
      ("filenames"
       (if (string-match-p "\\`\\(?:/\\|\\./\\|\\.\\./\\|~/\\)" value)
           ""
         "#"))
      (_ " ")))
   (t " ")))

(defvar-local +eat-nushell--completion-span nil
  "Replacement span returned by the latest `commandline complete' call.")

(defvar-local +eat-nushell--active-completions nil
  "Candidate metadata from the latest completion query.")

(defun +eat-nushell--commandline-completions (prompt)
  "Return configured Nu's detailed completions for PROMPT as a hash table.
The same Nu call also returns `ast --flatten', which supplies the active
argv used for Nix terminator classification."
  (when-let* ((result
               (+eat-nushell--config-json
                (concat
                 "do { let prompt = $env.EAT_NUSHELL_PROMPT; "
                 "{ completions: ($prompt | commandline complete --detailed), "
                 "ast: (ast $prompt --flatten) } }")
                (list (concat "EAT_NUSHELL_PROMPT=" prompt))))
              ((plist-get result :ok))
              (payload (plist-get result :value)))
    (let* ((completions (plist-get payload :completions))
           (ast (plist-get payload :ast))
           (args (+eat-nushell--args-from-flat-ast prompt ast))
           (nix-header (+eat-nushell--nix-completion-header args))
           (spans (delq nil
                        (mapcar (lambda (completion)
                                  (and (listp completion)
                                       (plist-get completion :span)))
                                completions)))
           ;; CAPF exposes one replacement region.  Normalize mixed Nu spans
           ;; to their union while preserving prompt text outside each
           ;; candidate's own replacement span.
           (span-start (and spans
                            (apply #'min
                                   (mapcar (lambda (span)
                                             (plist-get span :start))
                                           spans))))
           (span-end (and spans
                          (apply #'max
                                 (mapcar (lambda (span)
                                           (plist-get span :end))
                                         spans))))
           (span (and spans (list :start span-start :end span-end)))
           (table (make-hash-table :test #'equal
                                   :size (length completions))))
      (dolist (completion completions)
        (let* ((recordp (and (listp completion)
                             (keywordp (car completion))))
               (value (if recordp (plist-get completion :value) completion))
               (description (and recordp (plist-get completion :description)))
               (candidate-span (and recordp (plist-get completion :span)))
               (kind (and recordp (plist-get completion :kind)))
               (type (and recordp (plist-get completion :type))))
          (when (stringp value)
            (let* ((candidate-start (and candidate-span
                                         (plist-get candidate-span :start)))
                   (candidate-end (and candidate-span
                                       (plist-get candidate-span :end)))
                   (normalized
                    (if (and span candidate-span)
                        (concat
                         (+eat-nushell--prompt-byte-substring
                          prompt span-start candidate-start)
                         value
                         (+eat-nushell--prompt-byte-substring
                          prompt candidate-end span-end))
                      value)))
              (puthash
               normalized
               `(:display ,value
                 :value ,normalized
                 :terminator ,(+eat-nushell--completion-terminator
                               args nix-header value normalized kind)
                 ,@(when (and (stringp description)
                              (not (string-empty-p description)))
                     (list :description description))
                 ,@(when (stringp kind) (list :kind kind))
                 ,@(when (stringp type) (list :type type)))
               table)))))
      (setq-local +eat-nushell--completion-span span)
      table)))

(defvar +eat-nushell-doc--last-buffer nil
  "Most recent completion documentation buffer.")

(defun +eat-nushell-doc-buffer (candidate)
  "Return a documentation buffer for CANDIDATE."
  (when-let* ((entry (gethash candidate +eat-nushell--active-completions))
              (description (plist-get entry :description)))
    (when (buffer-live-p +eat-nushell-doc--last-buffer)
      (kill-buffer +eat-nushell-doc--last-buffer))
    (let ((buffer (generate-new-buffer " *eat-nushell-doc*" t)))
      (with-current-buffer buffer
        (insert description))
      (setq +eat-nushell-doc--last-buffer buffer)
      buffer)))

(defun +eat-nushell--span-position (prompt byte-offset)
  "Translate BYTE-OFFSET in UTF-8 PROMPT to an Eat buffer position."
  (let* ((bytes (encode-coding-string prompt 'utf-8))
         (offset (min byte-offset (length bytes)))
         (chars (decode-coding-string (substring bytes 0 offset) 'utf-8)))
    (+ (comint-line-beginning-position) (length chars))))

(defun +eat-nushell--finish-completion (candidate _status table)
  "Inject CANDIDATE's terminator as an input event after completion."
  (when-let* ((entry (gethash candidate table))
              (terminator (plist-get entry :terminator)))
    (cond
     ;; Requeue an existing slash so Corfu sees an insertion event and
     ;; opens the next directory level.
     ((string-suffix-p "/" candidate)
      (delete-char -1)
      (push ?/ unread-command-events))
     ((not (string-empty-p terminator))
      (push (aref terminator 0) unread-command-events))))
  (setq-local +eat-nushell--active-completions nil
              +eat-nushell--completion-span nil))

(defun +eat-nushell--company-kind (candidate table)
  "Return a Corfu-compatible kind for CANDIDATE in TABLE.
Nushell calls directories `directory' while Corfu expects `folder'.
External completers may report every path as a generic string value, so
infer those candidates from the filesystem when possible."
  (when-let* ((entry (gethash candidate table)))
    (let ((kind (plist-get entry :kind))
          (value (plist-get entry :display)))
      (cond
       ((equal kind "directory") 'folder)
       ((equal kind "file") 'file)
       ((and (stringp value)
             (or (string-suffix-p "/" value)
                 (file-directory-p value)))
        'folder)
       ((and (stringp value) (file-exists-p value)) 'file)
       ((stringp kind) (intern kind))))))

(defun +eat-nushell-capf ()
  "Complete the Eat input with configured Nu's `commandline complete'."
  (let* ((prompt (+eat-nushell--raw-prompt))
         (table (+eat-nushell--commandline-completions prompt))
         (span +eat-nushell--completion-span)
         (candidates (and table (hash-table-keys table))))
    (when (and candidates span)
      (let ((beg (+eat-nushell--span-position prompt (plist-get span :start)))
            (end (+eat-nushell--span-position prompt (plist-get span :end))))
        (setq-local +eat-nushell--active-completions table)
        (list beg end candidates
              :exclusive 'no
              :annotation-function
              (lambda (candidate)
                (plist-get (gethash candidate table) :description))
              :company-kind
              (lambda (candidate)
                (+eat-nushell--company-kind candidate table))
              :company-doc-buffer #'+eat-nushell-doc-buffer
              :exit-function
              (lambda (candidate status)
                (+eat-nushell--finish-completion candidate status table)))))))

(defun replace-eat-completions ()
  "Install Nushell-native completion in the current Eat buffer."
  (fish-completion-mode -1)
  (corfu-mode +1)
  (setq-local completion-at-point-functions (list #'+eat-nushell-capf)))
