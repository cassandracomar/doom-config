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
(require 'khalel)
(require 'filenotify)
;; Make `after!' (from doom-lib) and the evil API visible at compile time
;; so the byte-compiler doesn't flag them. `doom-keybinds' (which provides
;; `map!') intentionally is *not* loaded here — it depends on an internal
;; `doom--system-*' var that's only set during full doom startup, and
;; trying to require it standalone errors out. flymake's isolated env
;; will keep flagging `map!' as undefined; that warning is a noisy false
;; positive when the file is actually loaded inside doom.
(eval-when-compile
  (require 'doom-lib)
  (require 'evil))
(declare-function evil-make-overriding-map "evil-core")
(declare-function evil-normalize-keymaps "evil-core")
(declare-function evil-define-key* "evil-core")

;; khalel's `khalel--make-temp-window' splits the frame root window
;; unconditionally, which fails with "Window too small for splitting"
;; when calfw is in a side window or otherwise space-constrained. Route
;; through `display-buffer' so doom's popup rule below places the comint
;; cleanly regardless of frame layout.
(define-advice khalel--make-temp-window
    (:override (buf _height) +calfw-khal/display-buffer)
  (or (get-buffer-window buf 'visible)
      (display-buffer buf)))

(with-no-warnings
  (set-popup-rule! "^\\*khal-edit\\*$"
    :side 'bottom :size 16 :select t :quit t :modeline t
    :ttl nil :autosave nil))

;; Default is "%s%e%t%l%d" (start, end, title, location, description). The
;; back-to-back times in `- 09:30 - 10:00 Title' are hard to scan at a
;; glance — drop the end time and the empty trailing fields.
(setq calfw-event-format-detail "%s%t")

;; Hide weekends across all views (month/week/two-weeks/day). Each renderer
;; sizes columns from `(length calfw-week-days-list)' and the week-builder
;; filters days by membership, so dropping 0/6 just removes those columns.
;; `(car ...)' becoming Monday also makes the week views start on Monday.
(setq calfw-week-days-list '(1 2 3 4 5))

(defvar +calfw-khal-list-format
  (concat "{start-date}\t{start-time}\t{end-date}\t{end-time}\t"
          "{uid}\t{calendar}\t{title}\t{location}\t{organizer}")
  "Tab-delimited khal list format for `+calfw-khal-source-data'.
Avoids `{description}' to keep each event on a single line so the
naive line-by-line parser works.")

(defun +calfw-khal--parse-iso-date (s)
  "Parse YYYY-MM-DD string S into calfw date (MONTH DAY YEAR)."
  (when (and (stringp s) (>= (length s) 10))
    (list (string-to-number (substring s 5 7))
          (string-to-number (substring s 8 10))
          (string-to-number (substring s 0 4)))))

(defun +calfw-khal--parse-hhmm (s)
  "Parse HH:MM string S into (HOUR MIN), or nil if empty."
  (when (and (stringp s) (>= (length s) 5)
             (string-match "^\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)" s))
    (list (string-to-number (match-string 1 s))
          (string-to-number (match-string 2 s)))))

(defun +calfw-khal--parse-output-buffer (buf)
  "Parse khal-list output in BUF into a calfw source result.
Returns a list of single-day `calfw-event' structs, optionally with a
`(periods . LIST)' tail holding multi-day events.

Dedup key is (UID . START-DATE):
  - Multi-day events: khal lists the same row once per day in range with
    the same DTSTART, so they collapse to one entry.
  - Recurring events: each expanded occurrence has a different
    START-DATE, so they survive dedup as distinct entries.

Multi-day events (where START != END) are moved into the `periods'
list so calfw draws them as bars spanning their full range, rather
than stacking on the start day."
  (let (single-day
        periods
        (seen (make-hash-table :test 'equal)))
    (with-current-buffer buf
      (goto-char (point-min))
      (while (not (eobp))
        ;; OMIT-NULLS=nil — empty start-time/end-time fields are real and
        ;; must keep their positional slots; otherwise all-day events
        ;; slide every subsequent field two columns to the left.
        (let ((parts (split-string
                      (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))
                      "\t")))
          (when (>= (length parts) 7)
            (let* ((sd (+calfw-khal--parse-iso-date (nth 0 parts)))
                   (st (+calfw-khal--parse-hhmm (nth 1 parts)))
                   (ed (+calfw-khal--parse-iso-date (nth 2 parts)))
                   (et (+calfw-khal--parse-hhmm (nth 3 parts)))
                   (uid (nth 4 parts))
                   (cal (nth 5 parts))
                   (title (nth 6 parts))
                   (location (or (nth 7 parts) ""))
                   (organizer (or (nth 8 parts) ""))
                   (key (cons uid sd)))
              (when (and sd ed (not (gethash key seen)))
                (puthash key t seen)
                ;; Location stays off the calfw-event struct (keeps
                ;; *calfw-details* compact) but is stashed on the title's
                ;; text property for the click popup.
                (let ((ev (make-calfw-event
                           :title (propertize title
                                              '+calfw-khal-event
                                              (list :uid uid :calendar cal
                                                    :title title
                                                    :location location
                                                    :organizer organizer
                                                    :start-date sd :start-time st
                                                    :end-date ed :end-time et))
                           :start-date sd :start-time st
                           :end-date ed :end-time et)))
                  (if (equal sd ed)
                      (push ev single-day)
                    (push ev periods)))))))
        (forward-line 1)))
    (let ((result (nreverse single-day)))
      (if periods
          (append result (list (cons 'periods (nreverse periods))))
        result))))

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
  "File-notify watch descriptor on `+khalel-vdir-root', or nil.")

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

(defvar-local +calfw-khal--current-event nil
  "DATA plist of the event currently displayed in this popup buffer.
Buffer-local so `+calfw-khal-event-next'/`-prev' know where to step from.")

(defun +calfw-khal--date+time-key (ev)
  "Return a comparable (Y M D H M) integer-tuple for calfw-event EV."
  (let ((sd (calfw-event-start-date ev))
        (st (calfw-event-start-time ev)))
    (list (nth 2 sd) (nth 0 sd) (nth 1 sd)
          (if st (car st) 0)
          (if st (cadr st) 0))))

(defun +calfw-khal--event< (a b)
  "Return non-nil if calfw-event A is earlier than B."
  (let ((ka (+calfw-khal--date+time-key a))
        (kb (+calfw-khal--date+time-key b)))
    (catch 'done
      (cl-loop for x in ka for y in kb
               do (cond ((< x y) (throw 'done t))
                        ((> x y) (throw 'done nil))))
      nil)))

(defun +calfw-khal--all-cached-events ()
  "Flatten every cached event across calendars and ranges, sorted by start."
  (let (acc)
    (maphash
     (lambda (_range cal-alist)
       (dolist (cal-entry cal-alist)
         (dolist (item (cdr cal-entry))
           (cond
            ((calfw-event-p item) (push item acc))
            ((and (consp item) (eq (car item) 'periods))
             (dolist (p (cdr item)) (push p acc)))))))
     +calfw-khal--cache)
    (sort acc #'+calfw-khal--event<)))

(defun +calfw-khal--event-data (ev)
  "Pull the `+calfw-khal-event' text-property data off EV's title."
  (let ((title (calfw-event-title ev)))
    (and (stringp title) (get-text-property 0 '+calfw-khal-event title))))

(defun +calfw-khal--step-event (direction)
  "Show the event neighboring the current one. DIRECTION is +1 or -1.
Kills the current popup before showing the next, so the window history
doesn't accumulate stepped-through popups and `q' returns straight to
*calfw-details* (or whatever opened the popup originally)."
  (let* ((all (+calfw-khal--all-cached-events))
         (current-uid (plist-get +calfw-khal--current-event :uid))
         (idx (cl-position current-uid all
                           :key (lambda (e)
                                  (plist-get (+calfw-khal--event-data e) :uid))
                           :test #'string=))
         (target-idx (and idx (+ idx direction))))
    (cond
     ((null all)
      (message "No events in calfw cache — open the calendar first"))
     ((null idx)
      (message "Current event isn't in the cache anymore"))
     ((or (< target-idx 0) (>= target-idx (length all)))
      (message "No %s event in cache" (if (> direction 0) "next" "previous")))
     (t
      (let ((data (+calfw-khal--event-data (nth target-idx all)))
            (old-buf (current-buffer)))
        (when data
          (+calfw-khal--popup-event data)
          (when (and (buffer-live-p old-buf)
                     (not (eq old-buf (current-buffer))))
            (kill-buffer old-buf))))))))

(defun +calfw-khal-event-next ()
  "Show the next event (chronologically) in the current popup buffer."
  (interactive)
  (+calfw-khal--step-event +1))

(defun +calfw-khal-event-prev ()
  "Show the previous event (chronologically) in the current popup buffer."
  (interactive)
  (+calfw-khal--step-event -1))

(defun +calfw-khal--fetch-description (uid)
  "Return the DESCRIPTION for the event with UID, or nil if not found.
Reads the .ics file directly from the vdir. We tried `khal search'
first, but its free-text matcher silently drops long Exchange UIDs
\(those start with a long base64-ish prefix), so we'd miss most of
the calendar. vdirsyncer writes one .ics per UID with the UID as
the filename, so we can locate it without scanning the file body."
  (when-let* ((path (car (directory-files-recursively
                          (expand-file-name +khalel-vdir-root)
                          (concat "\\`" (regexp-quote uid) "\\.ics\\'")))))
    (with-temp-buffer
      (insert-file-contents-literally path)
      ;; ICS folds long lines by inserting CRLF + single whitespace; undo
      ;; that so the DESCRIPTION value is on one logical line.
      (goto-char (point-min))
      (while (re-search-forward "\r?\n[ \t]" nil t)
        (replace-match ""))
      (goto-char (point-min))
      (when (re-search-forward "^BEGIN:VEVENT" nil t)
        (when (re-search-forward "^DESCRIPTION\\(?:;[^:]*\\)?:\\(.*\\)$" nil t)
          (let ((s (match-string 1)))
            ;; ICS escapes inside text values: \n, \, \; \\
            (setq s (replace-regexp-in-string "\\\\n" "\n" s))
            (setq s (replace-regexp-in-string "\\\\," "," s))
            (setq s (replace-regexp-in-string "\\\\;" ";" s))
            (setq s (replace-regexp-in-string "\\\\\\\\" "\\\\" s))
            (when (> (length s) 0) s)))))))

(defun +calfw-khal-quit ()
  "Kill the current popup buffer and restore the previous window content."
  (interactive)
  (quit-window 'kill))

(defun +calfw-khal--popup-event (data)
  "Render the khal event DATA plist into a per-event buffer in the current window.
Layout (reading order):
  * Title :calendar:
  <timestamp>
  | Field     | Value |
  | ...                |

Properties live in a plain org table rather than a `:PROPERTIES:'
drawer — the popup is a transient view, not source for `org-id-find',
and a table reads more naturally than colon-prefixed lines.

Uses `switch-to-buffer' rather than `pop-to-buffer' so the buffer
replaces whatever was in the calling window (typically *calfw-details*).
We avoid `view-mode' because its `View-quit' only buries the buffer
and toggles itself off; once toggled off, evil-normal's `q' wins and
becomes `evil-record-macro'. Instead we set `read-only-mode' and bind
`q' buffer-locally to `+calfw-khal-quit', which always kills the popup
and restores the previous window content."
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
        ;; Heading
        (insert (format "* %s :%s:\n" (plist-get data :title)
                        (plist-get data :calendar)))
        ;; Property drawer with just the UID — folded by default, not
        ;; useful in the human-readable view but kept available so org-id
        ;; lookups / future tooling can still find the event.
        (insert (format ":PROPERTIES:\n:ID: %s\n:END:\n"
                        (plist-get data :uid)))
        ;; Timestamp under heading (and drawer)
        (insert (format "<%04d-%02d-%02d%s>--<%04d-%02d-%02d%s>\n\n"
                        (nth 2 sd) (nth 0 sd) (nth 1 sd)
                        (if st (format " %02d:%02d" (nth 0 st) (nth 1 st)) "")
                        (nth 2 ed) (nth 0 ed) (nth 1 ed)
                        (if et (format " %02d:%02d" (nth 0 et) (nth 1 et)) "")))
        ;; Property table — skip rows with empty values
        (let ((rows (delq nil
                          (list
                           (cons "Calendar"  (plist-get data :calendar))
                           (and (> (length (plist-get data :location)) 0)
                                (cons "Location"  (plist-get data :location)))
                           (and (> (length (plist-get data :organizer)) 0)
                                (cons "Organizer" (plist-get data :organizer)))))))
          (dolist (row rows)
            (insert (format "| %s | %s |\n" (car row) (cdr row))))
          (when rows
            ;; Align the table we just inserted
            (forward-line -1)
            (org-table-align)
            (goto-char (point-max))))
        ;; Event body — fetched via `khal search' by UID. Keep this last so
        ;; long descriptions don't push the structured info off-screen.
        (when-let* ((desc (+calfw-khal--fetch-description (plist-get data :uid))))
          (insert "\n" desc (if (string-suffix-p "\n" desc) "" "\n")))
        ;; Collapse the :PROPERTIES: drawer so the ID is hidden by default
        (goto-char (point-min))
        (when (re-search-forward "^:PROPERTIES:$" nil t)
          (org-fold-hide-drawer-toggle))
        (goto-char (point-min))
        (read-only-mode 1)
        ;; Buffer-local nav + quit. Remember which event this buffer is
        ;; showing; override n/p to step through events; bind q to a kill-
        ;; and-restore that always works (unlike view-mode's View-quit).
        (setq-local +calfw-khal--current-event data)
        (when (fboundp 'evil-local-set-key)
          (evil-local-set-key 'normal (kbd "n") #'+calfw-khal-event-next)
          (evil-local-set-key 'normal (kbd "p") #'+calfw-khal-event-prev)
          (evil-local-set-key 'normal (kbd "q") #'+calfw-khal-quit)
          ;; khalel-edit-calendar-event spawns `khal edit <uid>' in a
          ;; comint popup; our :PROPERTIES: drawer has the :ID: property
          ;; it needs, so it just works. Modifications hit the .ics, the
          ;; vdir file-notify watcher fires, and calfw's cache invalidates
          ;; automatically.
          (evil-local-set-key 'normal (kbd "e") #'khalel-edit-calendar-event))))
    (switch-to-buffer buf)))

(defun +calfw-khal--event-on-line ()
  "Return the `+calfw-khal-event' data for the calfw event on the current line.
Returns nil if none.  Scans the line for the first position carrying
the property."
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
  "Render the khal event at point into a transient `org-mode' popup.
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

;; Calfw's calendar and details buffers are read-only views with their
;; own dense keymap (n/p/b/f for navigation, W/M/D/T to switch view,
;; RET to open an event, etc.). Without intervention evil's normal-state
;; bindings shadow them. `evil-make-overriding-map' tells evil to defer
;; to the major-mode map for keys it actually binds, while leaving evil
;; bindings (and crucially the SPC leader) intact for everything else.
(after! evil
  (evil-make-overriding-map calfw-calendar-mode-map 'normal)
  (evil-make-overriding-map calfw-details-mode-map 'normal)
  ;; recompute precedence in any already-open buffers
  (dolist (buf '("*cfw-calendar*" "*calfw-details*"))
    (when-let* ((b (get-buffer buf)))
      (with-current-buffer b
        (evil-normalize-keymaps)))))

(defun +calfw-multi-calendar ()
  "Open calfw with one color-coded source per entry in `+khalel-calendars'.
Each source queries khal directly for the rendered date range, avoiding
a full scan of the multi-MB per-calendar org files on every navigation.
Opens in two-week view by default with point on today; press `M' for
month, `W' for week, `D' for day."
  (interactive)
  (calfw-open-calendar-buffer
   :view 'two-weeks
   :contents-sources
   (mapcar (lambda (entry)
             (let* ((name (car entry))
                    (color (or (cdr (assoc name +khalel-calendar-colors))
                               "Seagreen4")))
               (+calfw-khal-create-source name color)))
           +khalel-calendars))
  (when (get-buffer "*cfw-calendar*")
    (with-current-buffer "*cfw-calendar*"
      (calfw-navi-goto-today-command))))

;; calfw renders to the width of the window at creation time and doesn't
;; reflow on resize — until you close and reopen the buffer, the grid
;; stays its original width. We hook `window-size-change-functions' to
;; re-render on resize, but ONLY while a calfw buffer exists — the hook
;; is installed in `calfw-calendar-mode-hook' (per-buffer kill-hook
;; uninstalls when the last calfw buffer goes away). This keeps the
;; global hook list clean when calfw isn't open.
(defvar-local +calfw-khal--last-window-width nil
  "Last `window-total-width' of the window showing this calfw buffer.
Buffer-local so multiple calfw buffers don't clobber each other's
last-width and so a killed-and-reopened buffer starts fresh.")

(defun +calfw-khal--maybe-refresh (&rest _)
  "Refresh `*cfw-calendar*' if its window width changed since last render."
  (when-let* ((buf (get-buffer "*cfw-calendar*"))
              (win (get-buffer-window buf t))
              (w (window-total-width win)))
    (with-current-buffer buf
      (unless (eql w +calfw-khal--last-window-width)
        (setq +calfw-khal--last-window-width w)
        (calfw-refresh-calendar-buffer)
        (calfw-navi-goto-today-command)))))

(defun +calfw-khal--any-other-calfw-buffer-p ()
  "Return non-nil if another `calfw-calendar-mode' buffer is alive besides current."
  (cl-some (lambda (b)
             (and (not (eq b (current-buffer)))
                  (buffer-live-p b)
                  (with-current-buffer b
                    (derived-mode-p 'calfw-calendar-mode))))
           (buffer-list)))

(defun +calfw-khal--uninstall-resize-hook ()
  "Remove the global resize hook if no other calfw buffer is alive."
  (unless (+calfw-khal--any-other-calfw-buffer-p)
    (remove-hook 'window-size-change-functions #'+calfw-khal--maybe-refresh)))

(defun +calfw-khal--install-resize-hook ()
  "Install the global resize hook + buffer-local kill hook."
  (add-hook 'window-size-change-functions #'+calfw-khal--maybe-refresh)
  (add-hook 'kill-buffer-hook #'+calfw-khal--uninstall-resize-hook nil 'local))

(add-hook 'calfw-calendar-mode-hook #'+calfw-khal--install-resize-hook)

(provide '+calfw-khal)
;;; +calfw-khal.el ends here
