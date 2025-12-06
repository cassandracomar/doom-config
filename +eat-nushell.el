;;; +eat-nushell.el -*- lexical-binding: t; -*-

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
  (let* ((prompt (if (equal raw-prompt "") " " raw-prompt))
         (completions (fish-completion--list-completions prompt)))
    (cl-reduce (lambda (table comp)
                 (puthash (s-concat comp " ") `(:display ,comp :value ,comp) table)
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
  (if-let* ((quote-count (s-count-matches "`" raw-prompt))
            (prompt (s-replace "`" "'" (if (cl-oddp quote-count) (s-concat raw-prompt "`") raw-prompt)))
            (tokens (split-string-shell-command prompt))
            (command (car tokens))
            (raw-completions (apply #'carapace-completion--call
                                    `(,carapace-completion-command ,command ,(symbol-name shell) ,@tokens)))
            (parsed (ignore-errors (json-parse-string raw-completions :object-type 'plist :array-type 'array))))
      (when parsed
        (cl-reduce (lambda (table comp)
                     (cl-destructuring-bind (&key value &allow-other-keys) comp
                       (puthash (s-replace-regexp "['\"`]" "" value) comp table)
                       table))
                   parsed
                   :initial-value (make-hash-table :test #'equal :size (length parsed))))
    (carapace-completion--fish-fallback raw-prompt)))

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
         (end (if (eql (char-before rawend) ?\s) (- rawend 1) rawend))
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

(defvar-local carapace-nushell--active-completions nil)
(defun carapace-nushell--completions (prompt &optional no-refresh)
  (if no-refresh
      (or carapace-nushell--active-completions
          (carapace-nushell--completions prompt nil))
    (let ((candidates (carapace-completion--list-completions-with-desc 'nushell prompt)))
      (setq-local carapace-nushell--active-completions candidates))))

(defun carapace-nushell-backend (action &optional arg &rest _)
  (let ((completion-prompt (carapace-nushell--raw-prompt (point))))
    (pcase action
      ('prefix (ignore-errors
                 (pcase-let* ((quote-count (s-count-matches "`" completion-prompt))
                              (quoted-prompt (if (cl-oddp quote-count)
                                                 (s-concat completion-prompt "`")
                                               completion-prompt))
                              (`(,beg ,end) (carapace-nushell--arg quoted-prompt))
                              (real-end (if (cl-oddp quote-count) (- end 1) end))
                              (prefix (buffer-substring-no-properties beg (point)))
                              (unquoted-prefix (s-replace-regexp "['\"`]" "" prefix))
                              (suffix (buffer-substring-no-properties (point) real-end)))
                   (list unquoted-prefix suffix))))
      ('candidates (when-let* ((candidates (carapace-nushell--completions completion-prompt))
                               (possible (hash-table-keys candidates))
                               (unquoted-arg (s-replace-regexp "['\"`]" "" arg))
                               (pred (lambda (key _cand) (s-contains? unquoted-arg key))))
                     (seq-filter (lambda (key) (funcall pred key (gethash key candidates))) possible)))

      ('annotation (let* ((candidates (carapace-nushell--completions completion-prompt t))
                          (cand (gethash arg candidates)))
                     (plist-get cand :description)))
      ('post-completion
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
         (insert requoted-arg))))))

(defun replace-eat-completions ()
  (fish-completion-mode -1)
  (corfu-mode +1)
  (setq-local completion-at-point-functions
              (cape-company-to-capf #'carapace-nushell-backend (lambda (&rest _) carapace-nushell--active-completions))))
