;;; +khalel.el --- khalel customizations + per-calendar import -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Loaded lazily by the `:config' of the `khalel' use-package! block in
;; config.el — that means it activates the first time khalel's package is
;; needed (e.g., when `khalel-import-events' is invoked by the user or by
;; the `khalel-import.service' systemd unit).
;;
;; Holds:
;;   - khalel package settings (default calendar, import file, format, etc.)
;;   - `+khalel-earliest-event-date' helper for full-history imports
;;   - `+khalel-import-events-per-calendar' (called by the systemd unit)
;;
;; Shared data (`+khalel-calendars', `+khalel-calendar-colors') lives in
;; config.el because `+calfw-khal.el' needs the same mapping without
;; requiring this file.
;;
;;; Code:

(require 'khalel)

(setq khalel-khal-command (executable-find "khal")
      khalel-vdirsyncer-command (executable-find "vdirsyncer")
      khalel-default-calendar "personal"
      khalel-import-org-file (expand-file-name "todo/calendar.org" (getenv "HOME"))
      khalel-import-org-file-confirm-overwrite nil
      khalel-import-time-delta "30d"
      khalel-import-start-date "today"
      khalel-import-end-date "+30d"
      ;; Slim import format: heading + property drawer + standalone timestamp
      ;; + description. Drops the redundant `- When:`/`- Where:` bullets and
      ;; the inline edit-action links that bloat *calfw-details* and agenda.
      khalel-import-format
      "* {title} {cancelled} :{calendar}:
:PROPERTIES:
:CALENDAR: {calendar}
:LOCATION: {location}
:ID: {uid}
:END:
<{start-date-long} {start-time}>--<{end-date-long} {end-time}>

{description}
")

(defvar +khalel-vdir-root "~/.local/share/calendars/"
  "Root of vdirsyncer's local calendar storage.")

(defun +khalel-earliest-event-date ()
  "Earliest DTSTART inside a VEVENT block across all .ics in `+khalel-vdir-root'.
Skips VTIMEZONE DTSTART entries (Microsoft Exchange uses the Windows
FILETIME epoch 1601-01-01 as a placeholder for timezone effectivity)."
  (let ((earliest nil))
    (dolist (file (directory-files-recursively
                   (expand-file-name +khalel-vdir-root) "\\.ics\\'"))
      (with-temp-buffer
        (insert-file-contents-literally file)
        (goto-char (point-min))
        (when (re-search-forward "^BEGIN:VEVENT" nil t)
          (when (re-search-forward "^DTSTART[^:]*:\\([0-9]\\{8\\}\\)" nil t)
            (let ((dt (match-string 1)))
              (when (or (null earliest) (string< dt earliest))
                (setq earliest dt)))))))
    (when earliest
      (format "%s-%s-%s"
              (substring earliest 0 4)
              (substring earliest 4 6)
              (substring earliest 6 8)))))

(defun +khalel-import-events-per-calendar ()
  "Import each entry in `+khalel-calendars' into its own org file.
Spans from the earliest VEVENT DTSTART in the vdir to one year out,
so the full history is included."
  (interactive)
  (let ((start (or (+khalel-earliest-event-date) "2010-01-01")))
    (message "[khalel] importing from %s to +1y across %d calendar(s)..."
             start (length +khalel-calendars))
    (dolist (entry +khalel-calendars)
      (let ((khalel-default-calendar (car entry))
            (khalel-import-org-file (expand-file-name (cdr entry)))
            (khalel-import-start-date start)
            (khalel-import-end-date "+1y")
            (current-prefix-arg '(4)))
        (khalel-import-events)))))

(provide '+khalel)
;;; +khalel.el ends here
