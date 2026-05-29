;;; +eat.el -*- lexical-binding: t; -*-

(require 'eat)
(require 'subr-x)
(require 'evil)
(require 'treesit)
(require 'jit-lock)

(declare-function eshell-find-single-file "config")

(defvar-local +eat/-nu-parser nil
  "Nushell tree-sitter parser scoped to eat-line-mode input region.")

(defun +eat/-line-input-region ()
  "Return (BEG . END) of the live eat line-mode input region, or nil.
The input region is everything between `eat-term-end' (where the
rendered prompt finishes) and `point-max' (where the user is typing).
Returns nil if line-mode isn't active or the user hasn't typed anything."
  (when (and (bound-and-true-p eat--line-mode)
             (bound-and-true-p eat-terminal))
    (let* ((beg-m (eat-term-end eat-terminal))
           (beg (and beg-m (marker-position beg-m))))
      (when (and beg (< beg (point-max)))
        (cons beg (point-max))))))

(defun +eat/-sync-nu-parser-ranges (&rest _)
  "Keep `+eat/-nu-parser' aligned with the live input region.
When the user hasn't typed anything yet (or line-mode isn't active),
park the parser on `(point-min . point-min)' so it stays alive but
doesn't fontify anything.

Also flushes font-lock over the whole input region, so JIT re-fontifies
*every* character of the active token on each keystroke -- otherwise
tree-sitter's view of `l' (single-char identifier) and then `ls' (known
builtin) get different highlight faces, but JIT only re-runs over the
just-typed `s', leaving `l' with its stale face."
  (when (and +eat/-nu-parser
             (treesit-parser-p +eat/-nu-parser)
             (memq +eat/-nu-parser (treesit-parser-list)))
    (let ((region (+eat/-line-input-region)))
      (treesit-parser-set-included-ranges
       +eat/-nu-parser
       (if region
           (list region)
         (list (cons (point-min) (point-min)))))
      (when region
        (with-silent-modifications
          (put-text-property (car region) (cdr region) 'fontified nil))))))

(defun +eat/setup-line-mode-nu-highlight ()
  "Wire nushell tree-sitter highlighting onto the eat line-mode input.
Creates a single nu parser whose included ranges track the input
region, so the rendered prompt and earlier terminal output are left
alone -- only the user's pending input gets fontified.  Relies on
nu-ts-mode being available for `nu-ts-mode--font-lock-settings';
explicitly require it (nu-ts-mode is `:defer t :mode \"\\\\.nu\\\\'\"' so
it isn't autoloaded by eat alone)."
  (require 'nu-ts-mode nil t)
  (when (and (treesit-ready-p 'nu)
             (boundp 'nu-ts-mode--font-lock-settings))
    (unless (and +eat/-nu-parser
                 (treesit-parser-p +eat/-nu-parser)
                 (memq +eat/-nu-parser (treesit-parser-list)))
      (setq +eat/-nu-parser (treesit-parser-create 'nu)))
    (setq-local treesit-primary-parser +eat/-nu-parser
                treesit-font-lock-settings nu-ts-mode--font-lock-settings
                treesit-font-lock-feature-list
                '((comment definition)
                  (keyword type string)
                  (variable literal)
                  (function builtin identifier operator punctuation error))
                treesit-language-at-point-function (lambda (_pos) 'nu)
                font-lock-fontify-region-function #'treesit-font-lock-fontify-region)
    (jit-lock-mode 1)
    (jit-lock-register #'treesit-font-lock-fontify-region)
    (+eat/-sync-nu-parser-ranges)
    (add-hook 'after-change-functions #'+eat/-sync-nu-parser-ranges nil t)
    (add-hook 'eat-update-hook #'+eat/-sync-nu-parser-ranges nil t)
    (font-lock-flush)))

(defun +eat/use-emacs-state-for-tui ()
  "Use evil emacs-state in eat's raw-input modes.
Semi-char and char modes forward keystrokes to the TUI; evil's normal-state
bindings (`:', `q', `i', ...) and insert-state's ESC->normal transition
would intercept those keys.  emacs-state passes everything through to the
underlying keymap stack, which is what we want."
  (when (and (bound-and-true-p evil-mode)
             (or (bound-and-true-p eat--semi-char-mode)
                 (bound-and-true-p eat--char-mode))
             (not (eq evil-state 'emacs)))
    (evil-emacs-state)))

(defun +eat/restore-evil-state-for-line-mode ()
  "Return to evil insert-state when eat enters line-mode.
Line-mode behaves like a regular Emacs buffer where modal editing makes
sense; `+eat/use-emacs-state-for-tui' flips to emacs-state during
semi-char/char-mode sessions, so flip back here so the user can submit
input in evil insert (and switch to normal-state if they want)."
  (when (and (bound-and-true-p evil-mode)
             (bound-and-true-p eat--line-mode)
             (eq evil-state 'emacs))
    (evil-insert-state)))

(defun +eat/skip-current-prompt-end (&rest _)
  "If point is on an `eat--shell-prompt-end' character, walk past the run.
Lets `eat-next-shell-prompt' continue past the current prompt instead of
stopping at the property-change one position over."
  (when (get-text-property (point) 'eat--shell-prompt-end)
    (goto-char (or (next-single-property-change
                    (point) 'eat--shell-prompt-end)
                   (point-max)))))

(defun nu/find-file (&rest args)
  (mapcar #'eshell-find-single-file args))

(defun +eat/nu-open (&rest args)
  (interactive)
  (apply #'nu/find-file args))

(defun +eat--face-spec-attribute (spec attribute)
  "Return the first specified ATTRIBUTE among SPEC, a `face' property value.
SPEC may be a face symbol, an anonymous face plist, or a list mixing
those (earlier entries win).  Returns `unspecified' when nothing in SPEC
sets ATTRIBUTE."
  (cond
   ((null spec) 'unspecified)
   ((and (symbolp spec) (facep spec)) (face-attribute spec attribute nil t))
   ((keywordp (car-safe spec))
    (if (plist-member spec attribute) (plist-get spec attribute) 'unspecified))
   ((consp spec)
    (catch 'done
      (dolist (s spec 'unspecified)
        (let ((v (+eat--face-spec-attribute s attribute)))
          (unless (memq v '(unspecified nil)) (throw 'done v))))))
   (t 'unspecified)))

(defun +eat--color->sgr (color layer)
  "Render COLOR (a name or hex string) as a truecolor SGR fragment.
LAYER is 38 for foreground or 48 for background.  Returns nil for an
unknown color."
  (when (stringp color)
    (when-let* ((cv (color-values color)))
      (format "%d;2;%d;%d;%d" layer
              (round (/ (nth 0 cv) 257.0))
              (round (/ (nth 1 cv) 257.0))
              (round (/ (nth 2 cv) 257.0))))))

(defun +eat--face->sgr (spec)
  "Return a reset-prefixed ANSI SGR escape string rendering face SPEC.
Translates foreground color, weight, slant and underline; background is
intentionally left to the terminal."
  (let ((params (list "0"))
        (fg (+eat--face-spec-attribute spec :foreground))
        (weight (+eat--face-spec-attribute spec :weight))
        (slant (+eat--face-spec-attribute spec :slant))
        (underline (+eat--face-spec-attribute spec :underline)))
    (when-let* (((not (memq fg '(unspecified nil))))
                (s (+eat--color->sgr fg 38)))
      (push s params))
    (when (memq weight '(bold semi-bold extra-bold ultra-bold heavy black))
      (push "1" params))
    (when (memq slant '(italic oblique)) (push "3" params))
    (when (and underline (not (memq underline '(unspecified nil)))) (push "4" params))
    (format "\e[%sm" (string-join (nreverse params) ";"))))

(defun +eat--propertized->ansi (string)
  "Translate STRING's `face' text properties into inline ANSI SGR codes.
Walks maximal runs of constant `face', emitting an SGR sequence only
when the rendered attributes change, and resets at the end."
  (let ((n (length string)) (pos 0) (out nil) (last nil))
    (while (< pos n)
      (let* ((face (get-text-property pos 'face string))
             (end (or (next-single-property-change pos 'face string) n))
             (sgr (+eat--face->sgr face)))
        (unless (equal sgr last) (push sgr out) (setq last sgr))
        (push (substring-no-properties string pos end) out)
        (setq pos end)))
    (push "\e[0m" out)
    (string-join (nreverse out))))

(defun +eat/cat (buffer-or-file)
  "Send BUFFER-OR-FILE's contents back to the eat-launched shell as an OSC reply.
Invoked from nushell via `eat cat <buffer-or-file>' -- inverse of `eat
tee'.  BUFFER-OR-FILE is resolved as a live buffer first; failing that,
as a readable file, which is opened buried with `find-file-noselect' (so
its major mode fontifies it) and left open.  The chosen buffer is
fontified and its `face' text properties are translated to ANSI SGR
escape codes (see `+eat--propertized->ansi'), so the shell renders the
contents the way they look in Emacs.  The shell uses `term query' to
send the request OSC and block reading the reply, so we send the result
as `OSC 51 e;K;<base64> ST'.  When neither a buffer nor a readable file
matches we send an empty payload, which the shell sees as a zero-length
string."
  (let* ((buf (or (get-buffer buffer-or-file)
                  (and (file-regular-p buffer-or-file)
                       (find-file-noselect buffer-or-file))))
         (content (if buf
                      (with-current-buffer buf
                        (font-lock-ensure)
                        (+eat--propertized->ansi
                         (buffer-substring (point-min) (point-max))))
                    ""))
         (b64 (base64-encode-string (encode-coding-string content 'utf-8) t)))
    (eat-term-send-string eat-terminal
                          (format "\e]51;e;K;%s\e\\" b64))))

(defun +eat/tee (buffer-name mode data)
  "Write DATA into BUFFER-NAME, creating it if necessary.
Invoked from nushell via `eat tee [-a] <buffer>' -- mirrors eshell's
`>#<buffer>' redirection but tee-style (the data continues down the
pipeline).  MODE is the string \"append\" (accumulate across calls,
matching `tee -a') or \"replace\" (clear the buffer first, matching the
default POSIX `tee').  In both cases the buffer is displayed in another
window so the user can see the output land."
  (let ((buf (get-buffer-create buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (when (equal mode "replace")
          (erase-buffer))
        (goto-char (point-max))
        (insert data)))
    (display-buffer buf)))
