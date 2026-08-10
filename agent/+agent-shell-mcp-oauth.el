;;; +agent-shell-mcp-oauth.el --- Authenticate Codex MCP servers from Emacs -*- lexical-binding: t; -*-

;; `codex-acp' uses Codex App Server internally, but ACP does not expose App
;; Server's `mcpServer/oauth/login' method.  This helper starts a short-lived
;; App Server using the same Codex bundled with codex-acp, asks it to perform
;; the supported OAuth flow, and lets Codex write the credentials to its usual
;; store.  A restarted codex-acp process then sees those credentials normally.

;;; Commentary:
;;; Code:

(require 'ansi-color)
(require 'browse-url)
(require 'cl-lib)
(require 'json)
(require 'map)
(require 'subr-x)

(defgroup +agent-shell-mcp-oauth nil
  "Authenticate Codex MCP servers from agent-shell."
  :group 'agent-shell)

(defcustom +agent-shell-mcp-oauth-timeout 300
  "Seconds Codex App Server should wait for MCP OAuth to complete."
  :type 'integer
  :group '+agent-shell-mcp-oauth)

(defvar agent-shell--state)
(defvar agent-shell-mcp-servers)
(defvar agent-shell-openai-codex-acp-command)
(defvar agent-shell-openai-codex-environment)

(declare-function agent-shell-restart "agent-shell" (&rest args))

(defvar +agent-shell-mcp-oauth--process nil
  "Transient Codex App Server process handling the current OAuth login.")

(defun +agent-shell-mcp-oauth--dynamic-value (value)
  "Evaluate VALUE when it is a literal lambda, as agent-shell does."
  (if (and (consp value) (eq (car value) 'lambda))
      (funcall value)
    value))

(defun +agent-shell-mcp-oauth--http-servers ()
  "Return configured agent-shell HTTP MCP servers as (NAME . URL) pairs.

URL lambdas stay unevaluated until their server is selected."
  (cl-loop for server in agent-shell-mcp-servers
           for name = (+agent-shell-mcp-oauth--dynamic-value
                       (alist-get 'name server))
           for type = (+agent-shell-mcp-oauth--dynamic-value
                       (alist-get 'type server))
           when (and (stringp name) (equal type "http"))
           collect (cons name (alist-get 'url server))))

(defun +agent-shell-mcp-oauth--read-server ()
  "Read an HTTP MCP server name, preferring a Slack server by default."
  (let* ((servers (+agent-shell-mcp-oauth--http-servers))
         (names (mapcar #'car servers))
         (default (or (cl-find-if (lambda (name)
                                   (string-match-p "slack" name))
                                 names)
                      (car names))))
    (completing-read "MCP server to authenticate: " names nil nil
                     nil nil default)))

(defun +agent-shell-mcp-oauth--server-url (name)
  "Return the evaluated agent-shell URL for MCP server NAME, or nil."
  (when-let ((entry (assoc-string name
                                  (+agent-shell-mcp-oauth--http-servers))))
    (+agent-shell-mcp-oauth--dynamic-value (cdr entry))))

(defun +agent-shell-mcp-oauth--codex-command ()
  "Return the Codex command prefix used by the installed codex-acp."
  (let ((codex-path (getenv "CODEX_PATH")))
    (cond
     ((and codex-path (not (string-empty-p codex-path)))
      (list codex-path))
     ((and (boundp 'agent-shell-openai-codex-acp-command)
           (stringp (car agent-shell-openai-codex-acp-command)))
      (let* ((acp-command (car agent-shell-openai-codex-acp-command))
             (acp-path (if (file-name-absolute-p acp-command)
                           acp-command
                         (executable-find acp-command)))
             (acp-source (and acp-path
                              (file-exists-p acp-path)
                              (file-truename acp-path)))
             (package-root (and acp-source
                                (expand-file-name
                                 ".." (file-name-directory acp-source))))
             (bundled-codex
              (and package-root
                   (expand-file-name
                    "node_modules/@openai/codex/bin/codex.js"
                    package-root)))
             (node (executable-find "node")))
        (cond
         ((and node bundled-codex (file-exists-p bundled-codex))
          (list node bundled-codex))
         ((executable-find "codex")
          (list (executable-find "codex")))
         (t
          (user-error "Cannot locate the Codex bundled with codex-acp")))))
     ((executable-find "codex")
      (list (executable-find "codex")))
     (t
      (user-error "Cannot locate Codex or codex-acp")))))

(defun +agent-shell-mcp-oauth--append-log (process text)
  "Append TEXT to PROCESS's diagnostic buffer."
  (when-let ((buffer (process-get process 'log-buffer)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert text)))))

(defun +agent-shell-mcp-oauth--log-tail (process)
  "Return a short diagnostic tail for PROCESS."
  (if-let ((buffer (process-get process 'log-buffer)))
      (if (buffer-live-p buffer)
          (with-current-buffer buffer
            (string-trim
             (ansi-color-filter-apply
              (buffer-substring-no-properties
               (max (point-min) (- (point-max) 2000))
               (point-max)))))
        "")
    ""))

(defun +agent-shell-mcp-oauth--send-request
    (process method params callback)
  "Send METHOD with PARAMS to PROCESS, invoking CALLBACK on its response."
  (let* ((id (1+ (or (process-get process 'next-id) 0)))
         (callbacks (process-get process 'callbacks))
         (request `((method . ,method) (id . ,id) (params . ,params))))
    (process-put process 'next-id id)
    (puthash id callback callbacks)
    (process-send-string process (concat (json-serialize request) "\n"))))

(defun +agent-shell-mcp-oauth--offer-restart (buffer server)
  "Offer to resume BUFFER after SERVER authentication succeeds."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (derived-mode-p 'agent-shell-mode)
        (if (y-or-n-p
             (format "OAuth complete for %s.  Restart agent-shell now? " server))
            (let ((session-id
                   (map-nested-elt agent-shell--state '(:session :id))))
              (if session-id
                  (agent-shell-restart :session-id session-id)
                (agent-shell-restart)))
          (message "Restart agent-shell when you want it to reload %s"
                   server))))))

(defun +agent-shell-mcp-oauth--finish (process success &optional error)
  "Finish PROCESS's OAuth flow with SUCCESS or ERROR."
  (unless (process-get process 'finished)
    (process-put process 'finished t)
    (when (eq process +agent-shell-mcp-oauth--process)
      (setq +agent-shell-mcp-oauth--process nil))
    (when (process-live-p process)
      (delete-process process))
    (let ((server (process-get process 'server)))
      (if success
          (progn
            (message "MCP OAuth complete for %s" server)
            (when-let ((origin (process-get process 'origin-buffer)))
              (run-at-time 0 nil
                           #'+agent-shell-mcp-oauth--offer-restart
                           origin server)))
        (let* ((tail (+agent-shell-mcp-oauth--log-tail process))
               (show-diagnostics
                (and (not (string-empty-p tail))
                     (or (null error)
                         (string-prefix-p "Codex App Server" error)))))
          (display-warning
           '+agent-shell-mcp-oauth
           (string-join
            (delq nil
                  (list (format "MCP OAuth failed for %s: %s"
                                server (or error "unknown error"))
                        (and show-diagnostics tail)))
            "\n\n")
           :error))))))

(defun +agent-shell-mcp-oauth--oauth-response (process result error)
  "Handle PROCESS's OAuth login response RESULT or ERROR."
  (if error
      (+agent-shell-mcp-oauth--finish
       process nil (or (alist-get 'message error) (format "%S" error)))
    (let ((url (alist-get 'authorizationUrl result)))
      (if (not (stringp url))
          (+agent-shell-mcp-oauth--finish
           process nil "Codex returned no authorization URL")
        (process-put process 'authorization-url url)
        (message "Opening MCP authorization for %s"
                 (process-get process 'server))
        (condition-case browse-error
            (browse-url url)
          (error
           (kill-new url)
           (message "Could not open the browser; authorization URL copied: %s"
                    (error-message-string browse-error))))))))

(defun +agent-shell-mcp-oauth--initialize-response (process _result error)
  "Start PROCESS's OAuth request after initialization, unless ERROR occurred."
  (if error
      (+agent-shell-mcp-oauth--finish
       process nil (or (alist-get 'message error) (format "%S" error)))
    (+agent-shell-mcp-oauth--send-request
     process "mcpServer/oauth/login"
     `((name . ,(process-get process 'server))
       (timeoutSecs . ,+agent-shell-mcp-oauth-timeout))
     #'+agent-shell-mcp-oauth--oauth-response)))

(defun +agent-shell-mcp-oauth--handle-message (process message)
  "Handle one parsed App Server MESSAGE from PROCESS."
  (let ((id (alist-get 'id message))
        (method (alist-get 'method message)))
    (cond
     (id
      (let* ((callbacks (process-get process 'callbacks))
             (callback (gethash id callbacks)))
        (when callback
          (remhash id callbacks)
          (funcall callback process
                   (alist-get 'result message)
                   (alist-get 'error message)))))
     ((equal method "mcpServer/oauthLogin/completed")
      (let* ((params (alist-get 'params message))
             (name (alist-get 'name params)))
        (when (equal name (process-get process 'server))
          (+agent-shell-mcp-oauth--finish
           process (alist-get 'success params) (alist-get 'error params))))))))

(defun +agent-shell-mcp-oauth--process-filter (process chunk)
  "Parse newline-delimited App Server JSON from PROCESS CHUNK."
  (let ((pending (concat (or (process-get process 'pending-output) "")
                         chunk)))
    (while (string-match "\n" pending)
      (let ((line (string-trim-right
                   (substring pending 0 (match-beginning 0)) "\r")))
        (setq pending (substring pending (match-end 0)))
        (unless (string-empty-p line)
          (condition-case parse-error
              (+agent-shell-mcp-oauth--handle-message
               process
               (json-parse-string line
                                  :object-type 'alist
                                  :array-type 'list
                                  :null-object nil
                                  :false-object nil))
            (error
             (+agent-shell-mcp-oauth--append-log
              process
              (format "Unparsed stdout: %s\n%s\n"
                      line (error-message-string parse-error))))))))
    (process-put process 'pending-output pending)))

(defun +agent-shell-mcp-oauth--process-sentinel (process event)
  "Report an unexpected PROCESS termination described by EVENT."
  (when (and (memq (process-status process) '(exit signal))
             (not (process-get process 'finished)))
    (+agent-shell-mcp-oauth--finish
     process nil (format "Codex App Server %s" (string-trim event)))))

;;;###autoload
(defun +agent-shell-mcp-oauth-login (server)
  "Authenticate MCP SERVER through Codex App Server.

HTTP servers from `agent-shell-mcp-servers' are offered as completion
candidates.  An arbitrary server name may also be entered when it already
exists in Codex's config.  On success, offer to restart and resume the current
agent-shell session so codex-acp reloads the credentials."
  (interactive (list (+agent-shell-mcp-oauth--read-server)))
  (when (process-live-p +agent-shell-mcp-oauth--process)
    (user-error "An MCP OAuth login is already in progress"))
  (let* ((configured-server server)
         (server (replace-regexp-in-string "[[:space:]]" "_"
                                           configured-server))
         (url (+agent-shell-mcp-oauth--server-url configured-server))
         (origin (and (derived-mode-p 'agent-shell-mode) (current-buffer)))
         (log-buffer (get-buffer-create " *agent-shell-mcp-oauth-log*"))
         (override (and url
                        (format "mcp_servers.%s.url=%s"
                                server
                                (json-serialize url))))
         (command (append (+agent-shell-mcp-oauth--codex-command)
                          '("app-server")
                          (and override (list "-c" override))
                          '("--stdio")))
         (process-environment
          (append (and (boundp 'agent-shell-openai-codex-environment)
                       agent-shell-openai-codex-environment)
                  process-environment))
         process)
    (unless (and (stringp server) (not (string-empty-p server)))
      (user-error "MCP server name cannot be empty"))
    (unless (string-match-p "\\`[A-Za-z0-9_-]+\\'" server)
      (user-error "MCP server name contains unsupported characters: %s"
                  configured-server))
    (when (and url (not (stringp url)))
      (user-error "MCP server %s has a non-string URL" server))
    (with-current-buffer log-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (setq process
          (make-process
           :name "agent-shell-mcp-oauth"
           :command command
           :connection-type 'pipe
           :coding 'utf-8-unix
           :noquery t
           :buffer nil
           :stderr log-buffer
           :filter #'+agent-shell-mcp-oauth--process-filter
           :sentinel #'+agent-shell-mcp-oauth--process-sentinel))
    (setq +agent-shell-mcp-oauth--process process)
    (process-put process 'callbacks (make-hash-table :test #'eql))
    (process-put process 'server server)
    (process-put process 'origin-buffer origin)
    (process-put process 'log-buffer log-buffer)
    (+agent-shell-mcp-oauth--send-request
     process "initialize"
     '((clientInfo . ((name . "doom-emacs-agent-shell")
                      (title . "Doom Emacs agent-shell")
                      (version . "1.0.0"))))
     #'+agent-shell-mcp-oauth--initialize-response)
    (message "Starting MCP OAuth for %s" server)))

(provide '+agent-shell-mcp-oauth)
;;; +agent-shell-mcp-oauth.el ends here
