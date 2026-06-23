;;; +markdown-table.el --- fix markdown table rendering -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;  fix markdown table rendering
;;
;;; Code:

(require 'color)

(defface +agent-shell-markdown-table-inline-code
  '((t :inherit default))
  "Inline code face used inside agent-shell markdown tables."
  :group 'agent-shell-markdown)

(defun +agent-shell-markdown-table-zebra-face ()
  "Return a subtle table stripe background based on `default'."
  (let* ((bg (face-background 'default nil t))
         (rgb (color-name-to-rgb bg))
         (luminance (+ (* 0.2126 (nth 0 rgb))
                       (* 0.7152 (nth 1 rgb))
                       (* 0.0722 (nth 2 rgb))))
         (darkp (< luminance 0.5))
         (shifted (if darkp
                      (mapcar (lambda (c) (+ c (* (- 1.0 c) 0.08))) rgb)
                    (mapcar (lambda (c) (* c 0.92)) rgb))))
    (list :background (apply #'color-rgb-to-hex (append shifted '(2))))))

(defun +agent-shell-markdown-table-apply-faces ()
  "Use subtle default-derived faces for agent-shell markdown tables."
  (let ((fg (face-foreground 'default nil t))
        (bg (face-background 'default nil t)))
    (set-face-attribute 'agent-shell-markdown-table-header nil
                        :foreground fg :background bg :inherit 'bold)
    (set-face-attribute 'agent-shell-markdown-table-border nil
                        :foreground fg :background bg :inherit nil)
    (apply #'set-face-attribute 'agent-shell-markdown-table-zebra nil
           (append (+agent-shell-markdown-table-zebra-face)
                   (list :foreground 'unspecified
                         :inherit nil)))
    (set-face-attribute '+agent-shell-markdown-table-inline-code nil
                        :foreground
                        (face-attribute 'agent-shell-markdown-inline-code
                                        :foreground nil t)
                        :background 'unspecified
                        :inherit 'default)))

(defun +agent-shell-markdown-table-apply-row-face (string row-face)
  "Apply ROW-FACE to unfaced table text in STRING.

This keeps inline code, links, and other already-rendered Markdown spans
from being flattened by zebra striping."
  (when row-face
    (let ((pos 0)
          (end (length string)))
      (while (< pos end)
        (let* ((next-face (or (next-single-property-change pos 'face string)
                              end))
               (next-font-lock-face
                (or (next-single-property-change pos 'font-lock-face string)
                    end))
               (next (min next-face next-font-lock-face)))
          (unless (or (get-text-property pos 'face string)
                      (get-text-property pos 'font-lock-face string))
            (add-face-text-property pos next row-face t string))
          (setq pos next)))))
  string)

(defun +agent-shell-markdown-table-style-inline-code (string)
  "Apply agent-shell inline-code rendering to table cell STRING."
  (if (string-search "`" string)
      (let ((start 0)
            (parts nil))
        (while (string-match "`\\([^`\n]+\\)`" string start)
          (push (substring string start (match-beginning 0)) parts)
          (let ((code (match-string 1 string)))
            (add-face-text-property 0 (length code)
                                    '+agent-shell-markdown-table-inline-code
                                    nil code)
            (add-text-properties
             0 (length code)
             '(font-lock-face +agent-shell-markdown-table-inline-code
               agent-shell-markdown-frozen t
               rear-nonsticky (agent-shell-markdown-frozen))
             code)
            (push code parts))
          (setq start (match-end 0)))
        (push (substring string start) parts)
        (apply #'concat (nreverse parts)))
    string))

(defun +agent-shell-markdown-table-inline-code-only-p (string)
  "Return non-nil if STRING has only table-safe inline-code faces."
  (let ((pos 0)
        (end (length string))
        (saw-face nil)
        (only-table-inline-code t))
    (while (< pos end)
      (let* ((face (get-text-property pos 'face string))
             (next (or (next-single-property-change pos 'face string)
                       end)))
        (when face
          (setq saw-face t)
          (unless (eq face '+agent-shell-markdown-table-inline-code)
            (setq only-table-inline-code nil)))
        (setq pos next)))
    (and saw-face only-table-inline-code)))

(defun +agent-shell-markdown--text-has-face-p (orig text)
  "Ignore table-safe inline-code faces for table metric decisions."
  (and (funcall orig text)
       (not (+agent-shell-markdown-table-inline-code-only-p text))))

(defun +agent-shell-markdown--parse-table-row (cells)
  "Render inline code in parsed table CELLS before width calculation."
  (mapcar (lambda (cell)
            (let ((copy (copy-sequence cell)))
              (setf (alist-get :content copy)
                    (+agent-shell-markdown-table-style-inline-code
                     (alist-get :content copy)))
              copy))
          cells))

(defun +agent-shell-markdown--render-table-data-row (orig &rest args)
  "Render table rows without letting ROW-FACE override inline Markdown."
  (let* ((row-face (plist-get args :row-face))
         (args-without-row-face (plist-put (copy-sequence args) :row-face nil))
         (rendered (apply orig args-without-row-face)))
    (+agent-shell-markdown-table-apply-row-face rendered row-face)))

(provide '+markdown-table)
;;; +markdown-table.el ends here
