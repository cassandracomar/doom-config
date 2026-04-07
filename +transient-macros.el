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
    "Parse flat keyword-driven ARGS into grouped binding specs.

Each binding is: KEYWORDS... KEY CMD
  KEY — a string (the keybinding)
  CMD — a non-keyword symbol or #\\='symbol (the command)
  KEYWORDS — everything before KEY:
    :desc STRING — description for the transient menu
    any other keyword — Evil state for `map!' (e.g. :n :nv :nvi)

:block STRING starts a new group.

Keywords can appear in any order.  The binding boundary is detected
by finding a non-keyword symbol or (function sym) form (CMD);
the string before it is KEY."
    (let (groups current-group current-name)
      (while args
        (if (eq (car args) :block)
            (progn
              (pop args)
              (when current-name
                (push (cons current-name (nreverse current-group)) groups))
              (setq current-name (pop args) current-group nil))
          ;; Collect tokens until we hit CMD: a non-keyword symbol
          ;; or a (function sym) form from #'sym
          (let (items)
            (while (and args
                        (not (and (symbolp (car args)) (not (keywordp (car args)))))
                        (not (and (consp (car args)) (eq (caar args) 'function))))
              (push (pop args) items))
            (let* ((cmd (pop args))
                   (items (nreverse items))
                   (key (car (last items)))
                   (kw-section (butlast items))
                   desc state)
              (while kw-section
                (let ((kw (pop kw-section)))
                  (if (eq kw :desc)
                      (setq desc (pop kw-section))
                    (setq state kw))))
              (push (if state (list key desc cmd state)
                      (list key desc cmd))
                    current-group)))))
      (when current-name
        (push (cons current-name (nreverse current-group)) groups))
      (nreverse groups)))

  (defun +unquote-function (cmd)
    "Extract symbol from CMD, handling #\\='sym -> (function sym) -> sym."
    (if (and (consp cmd) (eq (car cmd) 'function))
        (cadr cmd)
      cmd)))

(defmacro define-keys! (keymap &rest body)
  "Define keybindings in KEYMAP from BODY.
See `+parse-key-groups' for the input format."
  (declare (indent 1))
  (let ((groups (+parse-key-groups body)))
    `(progn
       ,@(cl-loop for group in groups
                  nconc (cl-loop for b in (cdr group)
                                 for state = (nth 3 b)
                                 if (keywordp state)
                                 collect `(map! :map ,keymap ,state ,(nth 0 b) ,(nth 2 b))
                                 else collect `(map! :map ,keymap ,(nth 0 b) ,(nth 2 b)))))))

(defmacro define-transient! (transient-name docstring &rest body)
  "Generate a transient prefix TRANSIENT-NAME.
DOCSTRING is the transient's doc string; BODY is parsed by `+parse-key-groups'."
  (declare (indent 1))
  (let ((groups (+parse-key-groups body)))
    `(transient-define-prefix ,transient-name ()
       ,docstring
       [,@(cl-loop for group in groups
                   collect (apply #'vector (car group)
                                  (cl-loop for b in (cdr group)
                                           collect (list (nth 0 b) (nth 1 b) (+unquote-function (nth 2 b))))))])))

(defmacro define-keys-and-transient! (keymap transient-name docstring &rest body)
  "Define keybindings in KEYMAP and a transient prefix TRANSIENT-NAME.
DOCSTRING is the transient's doc string; BODY is parsed by `+parse-key-groups'."
  (declare (indent 2))
  (let ((groups (+parse-key-groups body)))
    `(progn
       ,@(cl-loop for group in groups
                  nconc (cl-loop for b in (cdr group)
                                 for state = (nth 3 b)
                                 if (keywordp state)
                                 collect `(map! :map ,keymap ,state ,(nth 0 b) ,(nth 2 b))
                                 else collect `(map! :map ,keymap ,(nth 0 b) ,(nth 2 b))))
       (transient-define-prefix ,transient-name ()
         ,docstring
         [,@(cl-loop for group in groups
                     collect (apply #'vector (car group)
                                    (cl-loop for b in (cdr group)
                                             collect (list (nth 0 b) (nth 1 b) (+unquote-function (nth 2 b))))))]))))

(provide '+transient-macros)
;;; +transient-macros.el ends here
