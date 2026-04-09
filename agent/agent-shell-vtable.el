;;; agent-shell-vtable.el --- Render markdown tables as vtable widgets -*- lexical-binding: t; -*-

;; Author: Cassandra Comar
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools

;;; Commentary:
;;
;; Replaces markdown-overlays table rendering with Emacs's built-in vtable
;; package for proper cursor navigation through table cells.
;;
;; During streaming, tables use the original overlay-based rendering.
;; Once output completes (shell-maker-finish-output), overlay tables are
;; replaced with vtable widgets that support normal cursor movement.
;;
;; See https://github.com/xenodium/agent-shell/issues/493

;;; Code:

(require 'vtable)
(require 'color)
(require 'markdown-overlays-tables)

(defvar shell-maker--busy)

(defun agent-shell-vtable--zebra-face ()
  "Return a face spec with a subtly shifted default background.
Lightens on dark themes, darkens on light themes."
  (let* ((bg (face-background 'default))
         (rgb (color-name-to-rgb bg))
         (luminance (+ (* 0.2126 (nth 0 rgb))
                       (* 0.7152 (nth 1 rgb))
                       (* 0.0722 (nth 2 rgb))))
         (darkp (< luminance 0.5))
         (shifted (if darkp
                      (mapcar (lambda (c) (+ c (* (- 1.0 c) 0.08))) rgb)
                    (mapcar (lambda (c) (* c 0.92)) rgb))))
    (list :background (apply #'color-rgb-to-hex (append shifted '(2))))))

(defun agent-shell-vtable--cleanup ()
  "Delete all vtable widget regions and overlays from the buffer."
  (remove-overlays (point-min) (point-max) 'agent-shell-vtable t)
  (let ((inhibit-read-only t)
        (inhibit-modification-hooks t))
    (save-excursion
      (let ((pos (point-min)))
        (while (setq pos (text-property-any pos (point-max) 'agent-shell-vtable t))
          (let ((end (or (text-property-not-all pos (point-max) 'agent-shell-vtable t)
                         (point-max))))
            (delete-region pos end)))))))

(defun agent-shell-vtable--render-table (table)
  "Render a single TABLE as a vtable widget."
  (let* ((preprocessed (markdown-overlays--preprocess-table table))
         (processed-rows (plist-get preprocessed :processed-rows))
         (separator-row (map-elt table :separator-row))
         (header-cells nil)
         (data-rows nil))
    ;; Separate header from data rows
    (dolist (row-entry processed-rows)
      (let* ((row (car row-entry))
             (cells (cdr row-entry))
             (row-num (map-elt row :num))
             (is-separator (map-elt row :separator)))
        (cond
         (is-separator nil)
         ((and separator-row (< row-num separator-row))
          (setq header-cells cells))
         (cells
          (push cells data-rows)))))
    (setq data-rows (nreverse data-rows))
    (when (or header-cells data-rows)
      (let* ((ncols (max (length (or header-cells (car data-rows))) 1))
             (col-names (if header-cells
                            (mapcar #'substring-no-properties header-cells)
                          (cl-loop for i from 1 to ncols
                                   collect (format "Column %d" i))))
             (columns (mapcar (lambda (name)
                                (make-vtable-column :name name :align 'left))
                              col-names))
             (table-start (map-elt table :start))
             (table-end (map-elt table :end))
             (line-prefix (get-text-property table-start 'line-prefix)))
        ;; Create invisible overlay covering raw markdown including
        ;; trailing newline so the vtable starts on a fresh line.
        (let* ((end-with-nl (if (and (< table-end (point-max))
                                     (eq (char-after table-end) ?\n))
                                (1+ table-end)
                              table-end))
               (ov (make-overlay table-start end-with-nl)))
          (overlay-put ov 'invisible 'agent-shell-vtable)
          (overlay-put ov 'evaporate t)
          (overlay-put ov 'agent-shell-vtable t))
        ;; Insert vtable after the hidden region
        (let ((inhibit-read-only t)
              (inhibit-modification-hooks t))
          (save-excursion
            (goto-char (if (and (< table-end (point-max))
                                (eq (char-after table-end) ?\n))
                           (1+ table-end)
                         table-end))
            (let ((insert-start (point)))
              (make-vtable
               :columns columns
               :objects data-rows
               :getter (lambda (object index _table)
                         (or (nth index object) ""))
               :displayer (lambda (value _index max-width _table)
                            (let ((s (if (stringp value) value
                                       (format "%s" value))))
                              (if (> (string-width s) max-width)
                                  (truncate-string-to-width s max-width nil nil "…")
                                s)))
               :separator-width 3
               :use-header-line nil)
              ;; vtable--insert moves point back to data start (after header);
              ;; find actual end via the 'vtable text property.
              (let ((vtable-end
                     (or (text-property-not-all
                          insert-start (point-max) 'vtable
                          (get-text-property insert-start 'vtable))
                         (point-max)))
                    (data-start (point)))
                (put-text-property insert-start vtable-end
                                   'agent-shell-vtable t)
                (when line-prefix
                  (put-text-property insert-start vtable-end
                                     'line-prefix line-prefix))
                ;; Header bold and zebra striping via overlays
                ;; (text property faces get stripped by shell-maker hooks).
                (when (> data-start insert-start)
                  (let ((ov (make-overlay insert-start (1- data-start))))
                    (overlay-put ov 'face 'bold)
                    (overlay-put ov 'agent-shell-vtable t)
                    (overlay-put ov 'evaporate t)))
                (when markdown-overlays--table-zebra-stripe
                  (save-excursion
                    (goto-char data-start)
                    (let ((row-num 0))
                      (while (< (point) vtable-end)
                        (when (= (mod row-num 2) 1)
                          (let ((ov (make-overlay (line-beginning-position)
                                                  (line-end-position))))
                            (overlay-put ov 'face (agent-shell-vtable--zebra-face))
                            (overlay-put ov 'agent-shell-vtable t)
                            (overlay-put ov 'evaporate t)))
                        (setq row-num (1+ row-num))
                        (forward-line 1)))))))))))))

(defun agent-shell-vtable--on-finish (&rest _)
  "Replace overlay-based table rendering with vtable widgets.
Called after `shell-maker-finish-output' when streaming is complete."
  (when agent-shell-vtable-mode
    ;; Clean up any stale vtable text from previous completions
    (agent-shell-vtable--cleanup)
    ;; Remove the overlay-based table decorations
    (remove-overlays (point-min) (point-max) 'invisible 'markdown-overlays-tables)
    ;; Find and render all tables as vtable widgets
    (let ((tables (markdown-overlays--find-tables)))
      (when tables
        (unless (and (listp buffer-invisibility-spec)
                     (memq 'agent-shell-vtable buffer-invisibility-spec))
          (add-to-invisibility-spec 'agent-shell-vtable))
        (dolist (table (reverse tables))
          (agent-shell-vtable--render-table table))))))

(defun agent-shell-vtable--maybe-enable ()
  "Enable `agent-shell-vtable-mode' in agent-shell buffers."
  (when (derived-mode-p 'agent-shell-mode)
    (agent-shell-vtable-mode 1)))

;;;###autoload
(define-minor-mode agent-shell-vtable-mode
  "Buffer-local minor mode to render markdown tables as vtable widgets.
When enabled, tables use Emacs's built-in vtable package instead of
overlay decorations, providing proper cursor navigation through
table cells.

During streaming output, the original overlay-based rendering is
used.  Once output completes, tables are converted to vtable
widgets with full cursor navigation support."
  :lighter " VT"
  :group 'agent-shell
  (if agent-shell-vtable-mode
      (advice-add 'shell-maker-finish-output
                  :after #'agent-shell-vtable--on-finish)
    (advice-remove 'shell-maker-finish-output
                   #'agent-shell-vtable--on-finish)
    (agent-shell-vtable--cleanup)))

;;;###autoload
(define-globalized-minor-mode agent-shell-vtable-global-mode
  agent-shell-vtable-mode
  agent-shell-vtable--maybe-enable
  :group 'agent-shell)

(provide 'agent-shell-vtable)
;;; agent-shell-vtable.el ends here
