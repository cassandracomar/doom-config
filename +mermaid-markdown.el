;;; +mermaid-markdown.el --- Inline mermaid diagram previews in markdown -*- lexical-binding: t; -*-

;;; Commentary:
;; Render fenced ```mermaid code blocks to inline SVG images (via the mermaid
;; CLI, mmdc) shown just below the still-editable source.  `+markdown-mermaid-mode'
;; is auto-enabled in markdown buffers and refreshes on save; unchanged blocks
;; are served from a content-hashed cache so Chromium only spawns when a diagram
;; actually changes.  Binds `,d' (localleader) to a manual re-render.

;;; Code:
(eval-when-compile
  (require 'doom))

(defvar +markdown-mermaid-mmdc (executable-find "mmdc")
  "Path to the mermaid CLI (mmdc) used to render diagrams.")

(defvar +markdown-mermaid-cache-dir (expand-file-name "mermaid/" doom-cache-dir)
  "Directory holding rendered SVGs and the puppeteer config.")

(defvar +markdown-mermaid-puppeteer-config
  (expand-file-name "puppeteer.json" +markdown-mermaid-cache-dir)
  "Puppeteer config passed to mmdc; adds --no-sandbox for Nix's Chromium.")

(defvar +markdown-mermaid-theme nil
  "Mermaid theme for mmdc -t.  When nil, pick dark/default from the frame.")

(defvar +markdown-mermaid-background "transparent"
  "Background passed to mmdc -b.")

(defun +markdown-mermaid--ensure ()
  "Ensure the cache directory and puppeteer config exist."
  (make-directory +markdown-mermaid-cache-dir t)
  (unless (file-exists-p +markdown-mermaid-puppeteer-config)
    (with-temp-file +markdown-mermaid-puppeteer-config
      (insert "{ \"args\": [\"--no-sandbox\"] }"))))

(defun +markdown-mermaid--theme ()
  "Mermaid theme to use, honoring the frame's `background-mode' when unset."
  (or +markdown-mermaid-theme
      (if (eq (frame-parameter nil 'background-mode) 'dark) "dark" "default")))

(defun +markdown-mermaid--blocks ()
  "Return a list of (BEG END CODE) for each fenced ```mermaid block."
  (save-excursion
    (goto-char (point-min))
    (let (blocks)
      (while (re-search-forward "^[ \t]*```[ \t]*mermaid[ \t]*$" nil t)
        (let ((beg (match-beginning 0))
              (cstart (1+ (line-end-position))))
          (when (re-search-forward "^[ \t]*```[ \t]*$" nil t)
            (push (list beg (match-end 0)
                        (buffer-substring-no-properties cstart (line-beginning-position)))
                  blocks))))
      (nreverse blocks))))

(defun +markdown-mermaid-clear ()
  "Remove all mermaid preview overlays in the current buffer."
  (interactive)
  (remove-overlays (point-min) (point-max) '+markdown-mermaid t))

(defun +markdown-mermaid--overlay-at (pos)
  "Return the mermaid preview overlay anchored exactly at POS, or nil."
  (seq-find (lambda (o) (and (overlay-get o '+markdown-mermaid)
                             (= (overlay-start o) pos)))
            (overlays-in (point-min) (point-max))))

(defun +markdown-mermaid--image-string (buf svg)
  "Build the after-string (dim label + image) for SVG, sized to BUF's window."
  (let* ((win (get-buffer-window buf 'visible))
         (maxw (max 200 (- (if win (window-pixel-width win) (frame-text-width)) 30)))
         (img (create-image svg 'svg nil :max-width maxw)))
    (concat "\n"
            (propertize "rendered:" 'face 'shadow)
            "\n"
            (propertize " " 'display img 'rear-nonsticky t)
            "\n")))

(defun +markdown-mermaid--place-or-update (buf pos svg hash)
  "Show SVG just after POS in BUF, reusing the overlay already there.
HASH is stored on the overlay so unchanged blocks can be skipped on a
later render.  Reusing the overlay swaps its image in place -- the old
preview is never cleared first, so re-renders don't flicker."
  (when (and (buffer-live-p buf) (file-exists-p svg))
    (with-current-buffer buf
      (let ((ov (or (+markdown-mermaid--overlay-at pos)
                    (make-overlay pos pos))))
        (overlay-put ov '+markdown-mermaid t)
        (overlay-put ov '+markdown-mermaid-hash hash)
        (overlay-put ov 'after-string (+markdown-mermaid--image-string buf svg))))))

(defun +markdown-mermaid--render-block (buf end code hash)
  "Render CODE (a mermaid block ending at END in BUF, keyed by HASH).
The block's current overlay stays put until mmdc finishes, then is
updated in place."
  (+markdown-mermaid--ensure)
  (let* ((svg (expand-file-name (concat hash ".svg") +markdown-mermaid-cache-dir))
         (mk (with-current-buffer buf (copy-marker end t))))
    (if (file-exists-p svg)
        (+markdown-mermaid--place-or-update buf (marker-position mk) svg hash)
      (let ((mmd (make-temp-file "mermaid" nil ".mmd" code)))
        (make-process
         :name "mmdc" :buffer (generate-new-buffer " *mmdc*") :noquery t
         :command (list +markdown-mermaid-mmdc "-i" mmd "-o" svg
                        "-t" (+markdown-mermaid--theme)
                        "-b" +markdown-mermaid-background
                        "-p" +markdown-mermaid-puppeteer-config)
         :sentinel
         (lambda (proc _event)
           (when (eq (process-status proc) 'exit)
             (ignore-errors (delete-file mmd))
             (if (and (= (process-exit-status proc) 0) (file-exists-p svg))
                 (+markdown-mermaid--place-or-update buf (marker-position mk) svg hash)
               (message "[mermaid] %s"
                        (string-trim (with-current-buffer (process-buffer proc)
                                       (buffer-string)))))
             (when (buffer-live-p (process-buffer proc))
               (kill-buffer (process-buffer proc))))))))))

(defun +markdown-mermaid-render-buffer ()
  "Render fenced ```mermaid blocks as inline images, refreshing in place.
Overlays for deleted blocks are removed, unchanged blocks are left
untouched, and a changed block's overlay is replaced only once mmdc
finishes -- so existing previews never flicker."
  (interactive)
  (unless +markdown-mermaid-mmdc (user-error "mmdc not found on PATH"))
  (let* ((buf (current-buffer))
         (blocks (+markdown-mermaid--blocks))
         (ends (mapcar #'cadr blocks)))
    ;; drop overlays whose block no longer exists
    (dolist (o (overlays-in (point-min) (point-max)))
      (when (and (overlay-get o '+markdown-mermaid)
                 (not (member (overlay-start o) ends)))
        (delete-overlay o)))
    ;; (re)render each block, skipping any already showing the right image
    (dolist (b blocks)
      (let* ((end (cadr b))
             (code (caddr b))
             (hash (secure-hash 'sha1 code))
             (ov (+markdown-mermaid--overlay-at end)))
        (unless (and ov (equal (overlay-get ov '+markdown-mermaid-hash) hash))
          (+markdown-mermaid--render-block buf end code hash))))))

(define-minor-mode +markdown-mermaid-mode
  "Show fenced ```mermaid blocks as inline images, refreshed on save."
  :lighter " mmd"
  (if +markdown-mermaid-mode
      (progn
        (add-hook 'after-save-hook #'+markdown-mermaid-render-buffer nil t)
        (+markdown-mermaid-render-buffer))
    (remove-hook 'after-save-hook #'+markdown-mermaid-render-buffer t)
    (+markdown-mermaid-clear)))

(add-hook 'markdown-mode-hook #'+markdown-mermaid-mode)

(provide '+mermaid-markdown)
;;; +mermaid-markdown.el ends here
