;;; +agent-shell-mcp-oauth.el --- Authenticate agent-shell MCP servers -*- lexical-binding: t; -*-

;; `codex-acp' uses Codex App Server internally, but ACP does not expose App
;; Server's `mcpServer/oauth/login' method.  This helper starts a short-lived
;; App Server using the same Codex bundled with codex-acp, asks it to perform
;; the supported OAuth flow, and lets Codex write the credentials to its usual
;; store.  A restarted codex-acp process then sees those credentials normally.
;; Claude uses its SDK control protocol for the same purpose, polling MCP
;; status until the browser callback has completed and credentials are saved.

;;; Commentary:
;;; Code:

(require 'ansi-color)
(require 'browse-url)
(require 'cl-lib)
(require 'json)
(require 'map)
(require 'subr-x)

(defgroup +agent-shell-mcp-oauth nil
  "Authenticate Codex and Claude MCP servers from agent-shell."
  :group 'agent-shell)

(defcustom +agent-shell-mcp-oauth-timeout 300
  "Seconds to wait for MCP OAuth to complete."
  :type 'integer
  :group '+agent-shell-mcp-oauth)

(defcustom +agent-shell-mcp-oauth-claude-command nil
  "Claude Code command prefix to use for MCP OAuth.
When nil, use CLAUDE_CODE_EXECUTABLE from Claude's environment, or the
`claude' executable on PATH.  This should use the same credential store
as claude-agent-acp.  Requires Claude's SDK `mcp_authenticate' support."
  :type '(choice (const :tag "Automatic" nil) (repeat string))
  :group '+agent-shell-mcp-oauth)

(defvar agent-shell--state)
(defvar agent-shell-mcp-servers)
(defvar agent-shell-openai-codex-acp-command)
(defvar agent-shell-openai-codex-environment)
(defvar agent-shell-anthropic-claude-environment)

(declare-function agent-shell-restart "agent-shell" (&rest args))

(defvar +agent-shell-mcp-oauth--process nil
  "Transient process handling the current MCP OAuth login.")

(defun +agent-shell-mcp-oauth--backend (&optional prompt)
  "Return the current agent's OAuth backend.
Prompt when PROMPT is non-nil or outside an agent-shell buffer."
  (let ((identifier (and (derived-mode-p 'agent-shell-mode)
                         (map-nested-elt agent-shell--state
                                         '(:agent-config :identifier)))))
    (cond
     ((or prompt (null identifier))
      (intern (completing-read "MCP OAuth backend: " '("codex" "claude")
                               nil t nil nil
                               (if (eq identifier 'claude-code)
                                   "claude"
                                 "codex"))))
     ((eq identifier 'claude-code) 'claude)
     ((eq identifier 'codex) 'codex)
     (t (user-error "MCP OAuth is not supported for %s" identifier)))))

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

(defun +agent-shell-mcp-oauth--claude-server-config (name)
  "Return Claude's HTTP configuration for NAME.
Evaluate only the selected server's URL and headers."
  (when-let ((server
              (cl-find-if
               (lambda (server)
                 (and (equal name (+agent-shell-mcp-oauth--dynamic-value
                                    (alist-get 'name server)))
                      (equal "http" (+agent-shell-mcp-oauth--dynamic-value
                                      (alist-get 'type server)))))
               agent-shell-mcp-servers)))
    (let ((url (+agent-shell-mcp-oauth--dynamic-value (alist-get 'url server)))
          (headers (make-hash-table :test #'equal)))
      (unless (and (stringp url) (not (string-empty-p url)))
        (user-error "MCP server %s has an invalid URL" name))
      (dolist (header (+agent-shell-mcp-oauth--dynamic-value
                      (alist-get 'headers server)))
        (let ((key (+agent-shell-mcp-oauth--dynamic-value (alist-get 'name header)))
              (value (+agent-shell-mcp-oauth--dynamic-value (alist-get 'value header))))
          (unless (and (stringp key) (stringp value))
            (user-error "MCP server %s has an invalid HTTP header" name))
          (puthash key value headers)))
      `((type . "http") (url . ,url) (headers . ,headers)))))

(defun +agent-shell-mcp-oauth--claude-command ()
  "Return the Claude Code command prefix in the current process environment."
  (or +agent-shell-mcp-oauth-claude-command
      (when-let ((path (getenv "CLAUDE_CODE_EXECUTABLE")))
        (unless (string-empty-p path) (list path)))
      (when-let ((path (executable-find "claude")))
        (list path))
      (user-error "Cannot locate Claude Code; set +agent-shell-mcp-oauth-claude-command")))

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
  (let* ((next-id (1+ (or (process-get process 'next-id) 0)))
         (claude (eq (process-get process 'backend) 'claude))
         (id (if claude (number-to-string next-id) next-id))
         (callbacks (process-get process 'callbacks))
         (request (if claude
                      `((type . "control_request")
                        (request_id . ,id)
                        (request . ((subtype . ,method) ,@params)))
                    `((method . ,method) (id . ,id) (params . ,params)))))
    (process-put process 'next-id next-id)
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
    (when-let ((callbacks (process-get process 'callbacks)))
      (clrhash callbacks))
    (dolist (property '(timeout-timer poll-timer))
      (when-let ((timer (process-get process property)))
        (cancel-timer timer)
        (process-put process property nil)))
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
                         (string-prefix-p "Codex App Server" error)
                         (string-prefix-p "Claude Code" error)))))
          (display-warning
           '+agent-shell-mcp-oauth
           (string-join
            (delq nil
                  (list (format "MCP OAuth failed for %s: %s"
                                server (or error "unknown error"))
                        (and show-diagnostics tail)))
            "\n\n")
           :error))))))

(defun +agent-shell-mcp-oauth--open-url (process url)
  "Open PROCESS's authorization URL in Emacs's configured browser."
  (process-put process 'authorization-url url)
  (message "Opening MCP authorization for %s" (process-get process 'server))
  (condition-case browse-error
      (browse-url url)
    (error
     (kill-new url)
     (message "Could not open the browser; authorization URL copied: %s"
              (error-message-string browse-error)))))

(defun +agent-shell-mcp-oauth--oauth-response (process result error)
  "Handle PROCESS's OAuth login response RESULT or ERROR."
  (if error
      (+agent-shell-mcp-oauth--finish
       process nil (or (alist-get 'message error) (format "%S" error)))
    (let ((url (alist-get 'authorizationUrl result)))
      (if (not (stringp url))
          (+agent-shell-mcp-oauth--finish
           process nil "Codex returned no authorization URL")
        (+agent-shell-mcp-oauth--open-url process url)))))

(defun +agent-shell-mcp-oauth--claude-poll (process)
  "Request PROCESS's MCP status while its OAuth flow is active."
  (when (and (process-live-p process) (not (process-get process 'finished)))
    (+agent-shell-mcp-oauth--send-request
     process "mcp_status" nil #'+agent-shell-mcp-oauth--claude-status-response)))

(defun +agent-shell-mcp-oauth--claude-status-response (process result error)
  "Handle Claude MCP status RESULT or ERROR from PROCESS."
  (let* ((server (cl-find (process-get process 'server)
                          (alist-get 'mcpServers result)
                          :key (lambda (entry) (alist-get 'name entry))
                          :test #'equal))
         (status (alist-get 'status server)))
    (cond
     (error (+agent-shell-mcp-oauth--finish process nil (alist-get 'message error)))
     ((null server)
      (+agent-shell-mcp-oauth--finish process nil "Claude did not report the MCP server"))
     ((equal status "connected") (+agent-shell-mcp-oauth--finish process t))
     ((member status '("failed" "disabled"))
      (+agent-shell-mcp-oauth--finish
       process nil (or (alist-get 'error server) (format "MCP server is %s" status))))
     ((and (equal status "needs-auth") (not (process-get process 'auth-started)))
      (process-put process 'auth-started t)
      (+agent-shell-mcp-oauth--send-request
       process "mcp_authenticate" `((serverName . ,(process-get process 'server)))
       #'+agent-shell-mcp-oauth--claude-oauth-response))
     (t
      (process-put process 'poll-timer
                   (run-at-time 1 nil #'+agent-shell-mcp-oauth--claude-poll process))))))

(defun +agent-shell-mcp-oauth--claude-oauth-response (process result error)
  "Handle Claude's OAuth RESULT or ERROR from PROCESS."
  (cond
   (error (+agent-shell-mcp-oauth--finish process nil (alist-get 'message error)))
   ((and (alist-get 'requiresUserAction result)
         (not (stringp (alist-get 'authUrl result))))
    (+agent-shell-mcp-oauth--finish process nil "Claude returned no authorization URL"))
   (t
    (when (alist-get 'requiresUserAction result)
      (+agent-shell-mcp-oauth--open-url process (alist-get 'authUrl result)))
    (+agent-shell-mcp-oauth--claude-poll process))))

(defun +agent-shell-mcp-oauth--claude-initialize-response (process _result error)
  "Check PROCESS's MCP status after Claude initialization, unless ERROR occurred."
  (if error
      (+agent-shell-mcp-oauth--finish process nil (alist-get 'message error))
    (+agent-shell-mcp-oauth--claude-poll process)))

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
  "Handle one parsed Codex or Claude MESSAGE from PROCESS."
  (let ((id (alist-get 'id message))
        (method (alist-get 'method message)))
    (cond
     ((equal (alist-get 'type message) "control_response")
      (let* ((response (alist-get 'response message))
             (id (alist-get 'request_id response))
             (callbacks (process-get process 'callbacks))
             (callback (gethash id callbacks)))
        (when callback
          (remhash id callbacks)
          (funcall callback process (alist-get 'response response)
                   (unless (equal (alist-get 'subtype response) "success")
                     `((message . ,(or (alist-get 'error response)
                                       "Claude control request failed"))))))))
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
  "Parse newline-delimited Codex or Claude JSON from PROCESS CHUNK."
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
     process nil (format "%s %s"
                         (if (eq (process-get process 'backend) 'claude)
                             "Claude Code" "Codex App Server")
                         (string-trim event)))))

(defun +agent-shell-mcp-oauth--codex-login (server)
  "Authenticate MCP SERVER through a transient Codex App Server."
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

(defun +agent-shell-mcp-oauth--claude-login (server)
  "Authenticate MCP SERVER through Claude's SDK control protocol."
  (let* ((config (+agent-shell-mcp-oauth--claude-server-config server))
         (servers (make-hash-table :test #'equal))
         (log-buffer (get-buffer-create " *agent-shell-mcp-oauth-log*"))
         (origin (and (derived-mode-p 'agent-shell-mode) (current-buffer)))
         process)
    (when config (puthash server config servers))
    (with-current-buffer log-buffer
      (let ((inhibit-read-only t)) (erase-buffer)))
    (setq process
          (make-process
           :name "agent-shell-mcp-oauth"
           :command (append
                     (+agent-shell-mcp-oauth--claude-command)
                     '("--print" "--input-format" "stream-json"
                       "--output-format" "stream-json" "--verbose"
                       "--no-session-persistence" "--tools" ""
                       "--settings" "{\"disableAllHooks\":true}")
                     (when config
                       (list "--strict-mcp-config" "--mcp-config"
                             (json-serialize `((mcpServers . ,servers))))))
           :connection-type 'pipe
           :coding 'utf-8-unix
           :noquery t
           :buffer nil
           :stderr log-buffer
           :filter #'+agent-shell-mcp-oauth--process-filter
           :sentinel #'+agent-shell-mcp-oauth--process-sentinel))
    (setq +agent-shell-mcp-oauth--process process)
    (process-put process 'backend 'claude)
    (process-put process 'callbacks (make-hash-table :test #'equal))
    (process-put process 'server server)
    (process-put process 'origin-buffer origin)
    (process-put process 'log-buffer log-buffer)
    (process-put process 'timeout-timer
                 (run-at-time +agent-shell-mcp-oauth-timeout nil
                              #'+agent-shell-mcp-oauth--finish
                              process nil "Claude Code OAuth timed out"))
    (+agent-shell-mcp-oauth--send-request
     process "initialize" nil #'+agent-shell-mcp-oauth--claude-initialize-response)
    (message "Starting Claude MCP OAuth for %s" server)))

;;;###autoload
(defun +agent-shell-mcp-oauth-login (server &optional backend)
  "Authenticate MCP SERVER with Codex or Claude.

Use the current agent-shell's backend, or prompt outside agent-shell.
With a prefix argument, select the backend explicitly.  Lisp callers may
pass BACKEND as `codex' or `claude'.

HTTP servers from `agent-shell-mcp-servers' are offered as completion
candidates.  An arbitrary server name may also be entered when it already
exists in the chosen backend's config.  On success, offer to restart and
resume the current agent-shell session to reload the credentials."
  (interactive
   (let ((backend (+agent-shell-mcp-oauth--backend current-prefix-arg)))
     (list (+agent-shell-mcp-oauth--read-server) backend)))
  (when (process-live-p +agent-shell-mcp-oauth--process)
    (user-error "An MCP OAuth login is already in progress"))
  (unless (and (stringp server) (not (string-empty-p (string-trim server))))
    (user-error "MCP server name cannot be empty"))
  (let* ((backend (or backend (+agent-shell-mcp-oauth--backend)))
         (process-environment
          (append (pcase backend
                    ('codex (and (boundp 'agent-shell-openai-codex-environment)
                                 agent-shell-openai-codex-environment))
                    ('claude (and (boundp 'agent-shell-anthropic-claude-environment)
                                  agent-shell-anthropic-claude-environment)))
                  process-environment)))
    (pcase backend
      ('codex (+agent-shell-mcp-oauth--codex-login server))
      ('claude (+agent-shell-mcp-oauth--claude-login server))
      (_ (user-error "Unsupported MCP OAuth backend: %s" backend)))))

(provide '+agent-shell-mcp-oauth)
;;; +agent-shell-mcp-oauth.el ends here
