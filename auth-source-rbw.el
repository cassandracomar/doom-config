;;; auth-source-rbw.el --- rbw (Bitwarden) backend for auth-source -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides an auth-source backend that retrieves credentials from rbw,
;; the unofficial Bitwarden CLI (https://github.com/doy/rbw).
;;
;; Usage:
;;   (require 'auth-source-rbw)
;;   (auth-source-rbw-enable)
;;
;; Then add entries to your Bitwarden vault. The auth-source :host parameter
;; maps to the rbw entry name, and :user disambiguates when multiple entries
;; share a name.

;;; Code:

(require 'auth-source)
(require 'cl-lib)

(defgroup auth-source-rbw nil
  "rbw (Bitwarden) backend for auth-source."
  :group 'auth-source)

(defcustom auth-source-rbw-executable "rbw"
  "Path to the rbw executable."
  :type 'string
  :group 'auth-source-rbw)

(defun auth-source-rbw--run (args)
  "Run rbw with ARGS, return trimmed stdout or nil on failure."
  (condition-case nil
      (with-temp-buffer
        (when (zerop (apply #'call-process auth-source-rbw-executable nil t nil args))
          (string-trim (buffer-string))))
    (file-error nil)))

(defun auth-source-rbw--get-password (name &optional user)
  "Get password from rbw for entry NAME, optionally disambiguated by USER."
  (auth-source-rbw--run (append (list "get" name) (when user (list user)))))

(defun auth-source-rbw--get-field (name field &optional user)
  "Get FIELD from rbw for entry NAME, optionally disambiguated by USER."
  (auth-source-rbw--run (append (list "get" name)
                                (when user (list user))
                                (list "--field" field))))

(cl-defun auth-source-rbw-search (&rest spec
                                   &key backend type host user port max
                                   &allow-other-keys)
  "Search rbw for credentials matching HOST and USER."
  (let* ((host (if (listp host) (car host) host))
         (user (if (listp user) (car user) user)))
    (when host
      (let ((password (auth-source-rbw--get-password host user)))
        (when password
          (let ((username (or user (auth-source-rbw--get-field host "username"))))
            (list (list :host host
                        :user username
                        :secret (let ((pw password)) (lambda () pw))))))))))

(defun auth-source-rbw-get (host &optional field user)
  "Get a secret from rbw for entry HOST.
If FIELD is non-nil, return that field (e.g. \"username\") instead
of the password.  If FIELD is the symbol `secret', return the
password.  USER disambiguates when multiple entries share a name."
  (if (and field (not (eq field 'secret)))
      (auth-source-rbw--get-field host (if (symbolp field) (symbol-name field) field) user)
    (auth-source-rbw--get-password host user)))

(defun auth-source-rbw-backend-parse (entry)
  "Create an rbw auth-source backend from ENTRY."
  (when (eq entry 'rbw)
    (auth-source-backend
     :source "rbw"
     :type 'rbw
     :search-function #'auth-source-rbw-search)))

;;;###autoload
(defun auth-source-rbw-enable ()
  "Enable rbw as an auth-source backend."
  (add-hook 'auth-source-backend-parser-functions #'auth-source-rbw-backend-parse)
  (add-to-list 'auth-sources 'rbw)
  (auth-source-forget-all-cached))

(provide 'auth-source-rbw)
;;; auth-source-rbw.el ends here