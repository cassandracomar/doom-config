;;; +calfw-khal.el --- calfw source backed by khal CLI -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Loaded lazily by the `+calfw-multi-calendar' bootstrap stub in config.el
;; — that means it activates the first time the user opens the multi-source
;; calendar view (SPC o c).
;;
;; The big win over a plain `calfw-org-create-source' is that calfw queries
;; khal CLI for the rendered date range on each navigation (~0.6s for one
;; month) instead of scanning multi-MB org files (~4s). It also avoids
;; keeping the calendar org files loaded into Emacs.
;;
;; Each event stashes its full payload as a `+calfw-khal-event' text
;; property on the title string so the click handler can render a transient
;; popup without re-querying khal.
;;
;; Depends on the shared `+khalel-calendars' / `+khalel-calendar-colors'
;; defvars from `+khalel-vars.el'.
;;
;;; Code:

(require '+khalel-vars)
(require 'calfw)
(require 'calfw-org)

;; Default is "%s%e%t%l%d" (start, end, title, location, description). The
;; back-to-back times in `- 09:30 - 10:00 Title' are hard to scan at a
;; glance — drop the end time and the empty trailing fields.
(setq calfw-event-format-detail "%s%t")

(defvar +calfw-khal-list-format
  (concat "{start-date}\t{start-time}\t{end-date}\t{end-time}\t"
          "{uid}\t{calendar}\t{title}\t{location}\t{organizer}")
  "Tab-delimited khal list format for `+calfw-khal-source-data'.
Avoids `{description}' to keep each event on a single line so the
naive line-by-line parser works.")

(defun +calfw-khal--parse-iso-date (s)
  "Parse YYYY-MM-DD into calfw date (MONTH DAY YEAR)."
  (when (and (stringp s) (>= (length s) 10))
    (list (string-to-number (substring s 5 7))
          (string-to-number (substring s 8 10))
          (string-to-number (substring s 0 4)))))

(defun +calfw-khal--parse-hhmm (s)
  "Parse HH:MM into (HOUR MIN), nil if empty."
  (when (and (stringp s) (>= (length s) 5)
             (string-match "^\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)" s))
    (list (string-to-number (match-string 1 s))
          (string-to-number (match-string 2 s)))))

(defun +calfw-khal--parse-output-buffer (buf)
  "Parse khal-list output in BUF into a list of calfw-events."
  (let (events)
    (with-current-buffer buf
      (goto-char (point-min))
      (while (not (eobp))
        (let ((parts (split-string
                      (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))
                      "\t" t)))
          (when (>= (length parts) 7)
            (let* ((sd (+calfw-khal--parse-iso-date (nth 0 parts)))
                   (st (+calfw-khal--parse-hhmm (nth 1 parts)))
                   (ed (+calfw-khal--parse-iso-date (nth 2 parts)))
                   (et (+calfw-khal--parse-hhmm (nth 3 parts)))
                   (uid (nth 4 parts))
                   (cal (nth 5 parts))
                   (title (nth 6 parts))
                   (location (or (nth 7 parts) ""))
                   (organizer (or (nth 8 parts) "")))
              (when (and sd ed)
                ;; Location is intentionally NOT set on the calfw-event
                ;; struct — keeping it off keeps *calfw-details* compact.
                ;; The location is still stashed on the title's text property
                ;; so the click popup can show it.
                (push (make-calfw-event
                       :title (propertize title
                                          '+calfw-khal-event
                                          (list :uid uid :calendar cal
                                                :title title
                                                :location location
                                                :organizer organizer
                                                :start-date sd :start-time st
                                                :end-date ed :end-time et))
                       :start-date sd :start-time st
                       :end-date ed :end-time et)
                      events)))))
        (forward-line 1)))
    (nreverse events)))

(defun +calfw-khal--query-all (calendars begin end)
  "Run khal list for each entry in CALENDARS concurrently between BEGIN and END.
Returns an alist of (calendar . events). Spawns one async subprocess
per calendar, then waits on them all — the wall-clock cost collapses
from sum(per-call) to max(per-call)."
  (let ((start-str (format-time-string "%Y-%m-%d" (calfw-calendar-to-emacs begin)))
        (end-str (format-time-string "%Y-%m-%d" (calfw-calendar-to-emacs end)))
        (khal-bin (or (and (boundp 'khalel-khal-command) khalel-khal-command)
                      (executable-find "khal")))
        (procs nil))
    (dolist (cal calendars)
      (let ((buf (generate-new-buffer (format " *khal-out-%s*" cal))))
        (push (cons cal
                    (make-process
                     :name (format "khal-%s" cal)
                     :buffer buf
                     :command (list khal-bin "list" (format "-a%s" cal)
                                    "--format" +calfw-khal-list-format
                                    "--day-format" ""
                                    start-str end-str)
                     :noquery t
                     :connection-type 'pipe))
              procs)))
    ;; wait for all subprocesses
    (dolist (entry procs)
      (let ((proc (cdr entry)))
        (while (process-live-p proc)
          (accept-process-output proc 0.05))))
    ;; parse each buffer and clean up
    (prog1
        (mapcar (lambda (entry)
                  (let* ((cal (car entry))
                         (proc (cdr entry))
                         (buf (process-buffer proc)))
                    (cons cal (+calfw-khal--parse-output-buffer buf))))
                (nreverse procs))
      (dolist (entry procs)
        (kill-buffer (process-buffer (cdr entry)))))))

(defvar +calfw-khal--cache (make-hash-table :test 'equal)
  "Multi-snapshot cache mapping (BEGIN . END) -> alist of (calendar . events).
Lives across renders so month navigation never re-fetches a range you've
already visited within the session. Invalidated wholesale when the vdir
changes (see `+calfw-khal--vdir-watcher').")

(defvar +calfw-khal--vdir-watcher nil
  "file-notify watch descriptor on `+khalel-vdir-root', or nil.")

(defun +calfw-khal--invalidate-cache (&rest _)
  "Clear `+calfw-khal--cache'. Called by the vdir watcher on data change."
  (clrhash +calfw-khal--cache))

(defun +calfw-khal--ensure-vdir-watcher ()
  "Install a recursive file-notify watch on the vdir if one is not already live.
Invalidates `+calfw-khal--cache' whenever a calendar file is added,
changed, or removed (i.e., whenever `vdirsyncer.service' pulls updates)."
  (unless (and +calfw-khal--vdir-watcher
               (file-notify-valid-p +calfw-khal--vdir-watcher))
    (require 'filenotify)
    (let ((root (expand-file-name
                 (or (and (boundp '+khalel-vdir-root) +khalel-vdir-root)
                     "~/.local/share/calendars/"))))
      (when (file-directory-p root)
        (setq +calfw-khal--vdir-watcher
              (file-notify-add-watch
               root '(change attribute-change)
               (lambda (event)
                 ;; ignore self-rewrites / spurious touches
                 (when (memq (cadr event) '(created changed deleted renamed))
                   (+calfw-khal--invalidate-cache)))))))))

(defun +calfw-khal-source-data (calendar begin end)
  "Return calfw-event list for CALENDAR between BEGIN and END (calfw dates).
Reads from a per-(BEGIN, END) cache shared across the three calendar
sources and across renders. The first source to ask for a fresh range
triggers a parallel batch fetch for every calendar in `+khalel-calendars';
all subsequent reads for the same range hit the cache. The cache is
cleared whenever vdirsyncer modifies the vdir."
  (+calfw-khal--ensure-vdir-watcher)
  (let* ((key (cons begin end))
         (cached (gethash key +calfw-khal--cache)))
    (unless cached
      (setq cached (+calfw-khal--query-all
                    (mapcar #'car +khalel-calendars) begin end))
      (puthash key cached +calfw-khal--cache))
    (cdr (assoc calendar cached))))

(defun +calfw-khal-create-source (calendar color)
  "Make a calfw source for khal CALENDAR colored COLOR."
  (make-calfw-source
   :name calendar :color color
   :data (apply-partially #'+calfw-khal-source-data calendar)))

(defun +calfw-khal--event-buffer-name (data)
  "Return a buffer name `*khal: TITLE*' for the event DATA plist."
  (format "*khal: %s*" (or (plist-get data :title) "event")))

(defun +calfw-khal--popup-event (data)
  "Render the khal event DATA plist into a per-event buffer in the current window.
Uses `switch-to-buffer' rather than `pop-to-buffer' so the buffer replaces
whatever was in the calling window (typically *calfw-details*). Pressing
`q' (bound to `View-quit' via `view-mode') buries the event buffer; the
previous buffer surfaces back in the same window."
  (let ((buf (get-buffer-create (+calfw-khal--event-buffer-name data)))
        (sd (plist-get data :start-date))
        (st (plist-get data :start-time))
        (ed (plist-get data :end-date))
        (et (plist-get data :end-time)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (read-only-mode -1)
        (erase-buffer)
        (org-mode)
        (insert (format "* %s :%s:\n" (plist-get data :title)
                        (plist-get data :calendar)))
        (insert ":PROPERTIES:\n"
                (format ":CALENDAR: %s\n" (plist-get data :calendar))
                (format ":ID: %s\n" (plist-get data :uid)))
        (when (> (length (plist-get data :location)) 0)
          (insert (format ":LOCATION: %s\n" (plist-get data :location))))
        (when (> (length (plist-get data :organizer)) 0)
          (insert (format ":ORGANIZER: %s\n" (plist-get data :organizer))))
        (insert ":END:\n"
                (format "<%04d-%02d-%02d%s>--<%04d-%02d-%02d%s>\n"
                        (nth 2 sd) (nth 0 sd) (nth 1 sd)
                        (if st (format " %02d:%02d" (nth 0 st) (nth 1 st)) "")
                        (nth 2 ed) (nth 0 ed) (nth 1 ed)
                        (if et (format " %02d:%02d" (nth 0 et) (nth 1 et)) "")))
        (goto-char (point-min))
        (view-mode 1)))
    (switch-to-buffer buf)))

(defun +calfw-khal--event-on-line ()
  "Return the `+calfw-khal-event' data for the calfw event on the current line,
or nil. Scans the line for the first position carrying the property."
  (save-excursion
    (let ((line-end (line-end-position))
          data)
      (goto-char (line-beginning-position))
      (while (and (not data) (< (point) line-end))
        (let* ((cfw-ev (get-text-property (point) 'cfw:event))
               (title (and (calfw-event-p cfw-ev) (calfw-event-title cfw-ev))))
          (when (stringp title)
            (setq data (get-text-property 0 '+calfw-khal-event title))))
        (forward-char 1))
      data)))

(defun +calfw-khal-event-onclick ()
  "Render the khal event at point into a transient org-mode popup.
Works in both the calfw grid (point on the propertized title) and in
*calfw-details* (point anywhere on an event line — we walk the line to
find the `cfw:event' struct whose title carries our data)."
  (interactive)
  (let ((data (or (get-text-property (point) '+calfw-khal-event)
                  (get-text-property (max (point-min) (1- (point)))
                                     '+calfw-khal-event)
                  (+calfw-khal--event-on-line))))
    (if (null data)
        (message "No khal event at point")
      (+calfw-khal--popup-event data))))

(advice-add 'calfw-org-onclick :override #'+calfw-khal-event-onclick)

;; Iosevka Nerd Font renders U+2026 (`…') at 2 cells wide, but
;; `string-width' assumes 1. That causes calfw's truncated cells to
;; visually overflow by one cell each, drifting the column borders.
;; Override `char-width-table' so `string-width' matches the font, which
;; lets calfw's existing `truncate-string-to-width' calls compute correct
;; widths and the ellipsis ends up aligned. The width is detected from
;; the live font so this stays correct if a future font draws `…' at one
;; cell — we just store whichever value the font actually uses.
(when (and (display-graphic-p)
           ;; with-temp-buffer because string-pixel-width needs a buffer
           (with-temp-buffer (> (string-pixel-width "x") 0)))
  (let* ((default-pw (with-temp-buffer (string-pixel-width "x")))
         (ellipsis-pw (with-temp-buffer (string-pixel-width "…")))
         (width-cells (max 1 (round (/ (float ellipsis-pw) default-pw)))))
    (set-char-table-range char-width-table ?… width-cells)))

;; doom-molokai (and most doom themes) makes calfw's toolbar-button faces
;; inherit from `variable-pitch' AND ships an unreadable gray-on-steelblue
;; default. Force monospace inheritance (so the toolbar lines up with the
;; grid below) and recolor against the doom palette for legible contrast.
(after! calfw
  (dolist (face '(calfw-toolbar-button-off-face
                  calfw-toolbar-button-on-face
                  calfw-toolbar-face))
    (set-face-attribute face nil :inherit nil))
  (when (fboundp 'doom-color)
    (set-face-attribute 'calfw-toolbar-face nil
                        :background (doom-color 'bg-alt)
                        :foreground (doom-color 'fg))
    (set-face-attribute 'calfw-toolbar-button-off-face nil
                        :background (doom-color 'bg-alt)
                        :foreground (doom-color 'fg))
    (set-face-attribute 'calfw-toolbar-button-on-face nil
                        :background (doom-color 'bg-alt)
                        :foreground (doom-color 'orange)
                        :weight 'bold)))

;; calfw-details-mode-map binds RET to `calfw-details-kill-buffer-command'
;; by default, which kills the whole details buffer. We want RET to open the
;; event under point; `q' in the popup then dismisses the popup window and
;; returns focus to *calfw-details* without killing it. evil motion state
;; intercepts RET so we have to override there too.
(with-eval-after-load 'calfw
  (define-key calfw-details-mode-map (kbd "RET") #'+calfw-khal-event-onclick)
  (when (fboundp 'evil-define-key)
    (evil-define-key 'motion calfw-details-mode-map
      (kbd "RET") #'+calfw-khal-event-onclick)))

(defun +calfw-multi-calendar ()
  "Open calfw with one color-coded source per entry in `+khalel-calendars'.
Each source queries khal directly for the rendered date range, avoiding
a full scan of the multi-MB per-calendar org files on every navigation.
Opens in week view by default; press `M' for month, `T' for two-weeks,
`D' for day."
  (interactive)
  (calfw-open-calendar-buffer
   :view 'week
   :contents-sources
   (mapcar (lambda (entry)
             (let* ((name (car entry))
                    (color (or (cdr (assoc name +khalel-calendar-colors))
                               "Seagreen4")))
               (+calfw-khal-create-source name color)))
           +khalel-calendars)))

(provide '+calfw-khal)
;;; +calfw-khal.el ends here
