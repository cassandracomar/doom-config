;;; +transient-macros.el --- map!-style macros for keymaps + transient menus -*- lexical-binding: t; -*-

;;; Commentary:

;; Provides `define-keys!', `define-transient!', and
;; `define-keys-and-transient!' — macros that accept a flat,
;; keyword-driven binding spec (similar to Doom's `map!') and
;; generate both keymap bindings and transient prefix menus.

;;; Code:

(eval-when-compile (require 'cl-lib))

(eval-and-compile

  (defun +parse-key-groups (args)
    "Parse flat keyword-driven ARGS into rows of grouped binding specs.

Each binding is: KEYWORDS... KEY CMD
  KEY — a string (the keybinding)
  CMD — a non-keyword symbol or #\\='symbol (the command)
  KEYWORDS — everything before KEY:
    :desc STRING — description for the transient menu
    any other keyword — passed through to `map!' (e.g. :n :nv :localleader)

:block STRING starts a new column group.
:row starts a new row of columns (for transient layout).

Returns a list of rows, where each row is a list of groups,
and each group is (NAME . BINDINGS)."
    (let (rows current-row current-group current-name)
      (cl-flet ((flush-group ()
                  (when current-name
                    (push (cons current-name (nreverse current-group)) current-row)
                    (setq current-group nil current-name nil)))
                (flush-row ()
                  (when current-row
                    (push (nreverse current-row) rows)
                    (setq current-row nil))))
        (while args
          (cond
           ((eq (car args) :row)
            (pop args)
            (flush-group)
            (flush-row))
           ((eq (car args) :block)
            (pop args)
            (flush-group)
            (setq current-name (pop args)))
           (t
            ;; Collect tokens until CMD
            (let (items)
              (while (and args
                          (not (and (symbolp (car args)) (not (keywordp (car args)))))
                          (not (and (consp (car args)) (eq (caar args) 'function))))
                (push (pop args) items))
              (let* ((cmd (pop args))
                     (items (nreverse items))
                     (key (car (last items)))
                     (kw-section (butlast items))
                     desc states)
                (while kw-section
                  (let ((kw (pop kw-section)))
                    (if (eq kw :desc)
                        (setq desc (pop kw-section))
                      (push kw states))))
                (push (if states (list key desc cmd (nreverse states))
                        (list key desc cmd))
                      current-group))))))
        (flush-group)
        (flush-row))
      (nreverse rows)))

  (defun +all-groups (rows)
    "Flatten ROWS into a single list of groups."
    (apply #'append rows))

  (defun +unquote-function (cmd)
    "Extract symbol from CMD, handling #\\='sym -> (function sym) -> sym."
    (if (and (consp cmd) (eq (car cmd) 'function))
        (cadr cmd)
      cmd))

  (defun +groups-to-transient-row (groups)
    "Convert GROUPS to a transient row vector."
    (apply #'vector
           (cl-loop for group in groups
                    collect (apply #'vector (car group)
                                   (cl-loop for b in (cdr group)
                                            collect (list (nth 0 b) (nth 1 b)
                                                         (+unquote-function (nth 2 b)))))))))

(defmacro define-keys! (keymap &rest body)
  "Define keybindings in KEYMAP from BODY.
See `+parse-key-groups' for the input format."
  (declare (indent 1))
  (let ((all-groups (+all-groups (+parse-key-groups body))))
    `(progn
       ,@(cl-loop for group in all-groups
                  nconc (cl-loop for b in (cdr group)
                                 for states = (nth 3 b)
                                 if states
                                 collect `(map! :map ,keymap ,@states ,(nth 0 b) ,(nth 2 b))
                                 else collect `(map! :map ,keymap ,(nth 0 b) ,(nth 2 b)))))))

(defmacro define-transient! (transient-name docstring &rest body)
  "Generate a transient prefix TRANSIENT-NAME.
DOCSTRING is the transient's doc string; BODY is parsed by `+parse-key-groups'."
  (declare (indent 1))
  (let ((rows (+parse-key-groups body)))
    `(transient-define-prefix ,transient-name ()
       ,docstring
       ,@(cl-loop for row in rows
                  collect (+groups-to-transient-row row)))))

(defmacro define-keys-and-transient! (keymap transient-name docstring &rest body)
  "Define keybindings in KEYMAP and a transient prefix TRANSIENT-NAME.
DOCSTRING is the transient's doc string; BODY is parsed by `+parse-key-groups'."
  (declare (indent 2))
  (let ((rows (+parse-key-groups body)))
    `(progn
       ,@(cl-loop for group in (+all-groups rows)
                  nconc (cl-loop for b in (cdr group)
                                 for states = (nth 3 b)
                                 if states
                                 collect `(map! :map ,keymap ,@states ,(nth 0 b) ,(nth 2 b))
                                 else collect `(map! :map ,keymap ,(nth 0 b) ,(nth 2 b))))
       (transient-define-prefix ,transient-name ()
         ,docstring
         ,@(cl-loop for row in rows
                    collect (+groups-to-transient-row row))))))

(provide '+transient-macros)
;;; +transient-macros.el ends here
