;;; +khalel-vars.el --- shared khalel/calfw-khal data -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Shared between `+khalel.el', `+calfw-khal.el', and the systemd batch
;; import script in users/ccomar.nix. Holds the per-calendar mapping
;; (account -> org file) and the calfw display colors.
;;
;; Kept in its own tiny file (rather than living in config.el) so the
;; batch import script can `(require '+khalel-vars)' against the doom
;; profile dir without pulling in the rest of the user config.
;;
;;; Code:

(defvar +khalel-calendars
  '(("personal"  . "~/todo/calendar-personal.org")
    ("team"      . "~/todo/calendar-team.org")
    ("pagerduty" . "~/todo/calendar-pagerduty.org"))
  "Mapping of khal calendar name to per-calendar org file path.")

(defvar +khalel-calendar-colors
  '(("personal"  . "deep sky blue")
    ("team"      . "spring green")
    ("pagerduty" . "gold"))
  "Per-calendar colors for calfw display, keyed by khal calendar name.")

(provide '+khalel-vars)
;;; +khalel-vars.el ends here
