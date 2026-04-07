;;; +agent-dispatcher.el -*- lexical-binding: t; -*-

;; Permission forwarding, progress polling, and agent spawning for
;; meta-agent-shell dispatch workflows.

(require 'cl-lib)
(require 'color)
(require 'map)
(require 'svg)

(declare-function agent-shell--make-permission-button "agent-shell")
(declare-function agent-shell--start "agent-shell")
(declare-function agent-shell-anthropic-make-claude-code-config "agent-shell")
(declare-function agent-shell-ui-delete-fragment "agent-shell")
(declare-function agent-shell-ui-make-fragment-model "agent-shell")
(declare-function agent-shell-ui-update-fragment "agent-shell")
(defvar agent-shell--state)
(defvar shell-maker--busy)

;; -- Permission forwarding from background agents to dispatcher buffer --

(defvar +meta-agent-shell--primary-buffer nil
  "Buffer name of the primary (dispatcher) shell for permission rendering.")

(defvar +meta-agent-shell--pending-permission-agents nil
  "List of agent buffer names with unresolved permission dialogs.")

(defvar +meta-agent-shell--dispatch-state nil
  "State for the dispatch progress polling timer.")

(defun +meta-agent-shell--cleanup-permission (perm-id target-buf agent-buf option-id)
  "Remove the permission dialog identified by PERM-ID from TARGET-BUF."
  (when-let* ((buf (get-buffer target-buf)))
    (with-current-buffer buf
      (save-excursion
        (let ((inhibit-read-only t))
          (goto-char (point-min))
          (when-let* ((match (text-property-search-forward
                              '+meta-agent-shell-perm-id perm-id t)))
            (let ((start (prop-match-beginning match))
                  (end (prop-match-end match)))
              (delete-region start end)
              (goto-char start)
              (insert (propertize
                       (format "    ✓ %s — %s\n" agent-buf option-id)
                       'font-lock-face 'font-lock-comment-face
                       'read-only t)))))))))

(defun +meta-agent-shell--find-dispatch-fragment ()
  "Find the start position of the dispatch-progress fragment in current buffer."
  (save-excursion
    (goto-char (point-max))
    (cl-block nil
      (while (let ((match (text-property-search-backward
                           'agent-shell-ui-section nil nil)))
               (when match
                 (let ((help (get-text-property (prop-match-beginning match) 'help-echo)))
                   (when (and help (stringp help) (string-match-p "dispatch-progress" help))
                     (cl-return (prop-match-beginning match))))
                 t))))))

(defun +meta-agent-shell--insert-before-prompt (buf text)
  "Insert TEXT into BUF above the dispatch fragment, or before the prompt."
  (with-current-buffer buf
    (save-excursion
      (let ((inhibit-read-only t))
        (cond
         ;; If dispatch fragment exists, insert just above it
         ((when-let* ((frag-pos (+meta-agent-shell--find-dispatch-fragment)))
            (goto-char frag-pos)
            (forward-line 0)
            (insert text "\n")
            t))
         ;; If busy, append at point-max
         (shell-maker--busy
          (goto-char (point-max))
          (insert text))
         ;; Idle: insert before prompt
         ((when-let* ((proc (get-buffer-process (current-buffer))))
            (goto-char (process-mark proc))
            (forward-line 0)
            (insert text "\n")
            t))
         ;; Fallback
         (t (goto-char (point-max))
            (insert text)))))))

(defun +meta-agent-shell-forward-permission (permission)
  "Render PERMISSION from a background agent in the dispatcher buffer.
Uses agent-shell's native button maker for full navigation compatibility."
  (when-let* ((tool-call (map-elt permission :tool-call))
              (options (map-elt permission :options))
              (respond (map-elt permission :respond))
              (title (or (map-elt tool-call :title) "unknown"))
              (kind (or (map-elt tool-call :kind) "unknown"))
              (agent-buf (buffer-name))
              (target (and +meta-agent-shell--primary-buffer
                           (get-buffer +meta-agent-shell--primary-buffer))))
    (cl-pushnew agent-buf +meta-agent-shell--pending-permission-agents :test #'equal)
    (let* ((perm-id (format "perm-%s-%s" agent-buf (random)))
           (keymap (make-sparse-keymap))
           (buttons
            (mapconcat
             (lambda (opt)
               (let* ((option-id (map-elt opt :option-id))
                      (action (lambda ()
                                (interactive)
                                (funcall respond option-id)
                                (setq +meta-agent-shell--pending-permission-agents
                                      (delete agent-buf +meta-agent-shell--pending-permission-agents))
                                (+meta-agent-shell--cleanup-permission
                                 perm-id
                                 (buffer-name target)
                                 agent-buf
                                 option-id))))
                 (when-let* ((char-str (map-elt opt :char)))
                   (define-key keymap (kbd char-str) action))
                 (agent-shell--make-permission-button
                  :text (map-elt opt :label)
                  :help (map-elt opt :label)
                  :action action
                  :keymap keymap
                  :char (map-elt opt :char)
                  :option (or (map-elt opt :option)
                              (map-elt opt :label))
                  :navigatable t)))
             options
             " "))
           (text (format "\n╭─\n\n    %s %s %s\n\n\n    %s\n\n\n    %s\n\n\n╰─\n"
                         (propertize "⚠" 'font-lock-face 'warning)
                         (propertize (format "Permission: %s" agent-buf)
                                     'font-lock-face 'bold)
                         (propertize "⚠" 'font-lock-face 'warning)
                         (propertize (format "%s (%s)" title kind)
                                     'font-lock-face 'comint-highlight-input)
                         buttons)))
      (put-text-property 0 (length text) 'keymap keymap text)
      (put-text-property 0 (length text) '+meta-agent-shell-perm-id perm-id text)
      (+meta-agent-shell--insert-before-prompt target text))
    t))

;; -- Dispatch task graph and progress rendering --

(defvar +dispatch--spinner-frames
  '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏")
  "Braille spinner animation frames.")

(defvar +dispatch--spinner-index 0
  "Current spinner frame index.")

(defun +dispatch--format-elapsed (start-time)
  "Format elapsed time since START-TIME as a human-readable string."
  (let ((elapsed (floor (float-time (time-subtract nil start-time)))))
    (cond
     ((< elapsed 60) (format "%ds" elapsed))
     ((< elapsed 3600) (format "%dm%02ds" (/ elapsed 60) (% elapsed 60)))
     (t (format "%dh%02dm" (/ elapsed 3600) (% (/ elapsed 60) 60))))))

(defun +dispatch-report (task-id status &optional detail)
  "Report STATUS for TASK-ID. Called by agents via MCP.
STATUS is a string: \"working\", \"done\", \"error\".
DETAIL is an optional description of current activity."
  (when-let* ((state +meta-agent-shell--dispatch-state)
              (statuses (plist-get state :statuses)))
    (let ((existing (gethash task-id statuses)))
      (puthash task-id
               (list :status (intern status)
                     :detail detail
                     :updated (current-time)
                     :started (or (plist-get existing :started) (current-time)))
               statuses))))

(defun +dispatch--blend (color1 color2 alpha)
  "Blend COLOR1 with COLOR2 by ALPHA (0-1, where 1 = all COLOR1).
Returns a 6-digit hex color string."
  (let ((c1 (color-name-to-rgb color1))
        (c2 (color-name-to-rgb color2)))
    (apply #'format "#%02x%02x%02x"
           (cl-mapcar (lambda (a b)
                        (round (* 255 (+ (* alpha a) (* (- 1 alpha) b)))))
                      c1 c2))))

(defvar +dispatch--theme-cache nil
  "Cached theme color scheme for SVG rendering.")

(defun +dispatch--theme-colors ()
  "Derive SVG color scheme from current Emacs theme.
Caches the result; call `+dispatch--refresh-theme' to update."
  (or +dispatch--theme-cache
      (setq +dispatch--theme-cache (+dispatch--compute-theme-colors))))

(defun +dispatch--refresh-theme ()
  "Recompute cached theme colors."
  (interactive)
  (setq +dispatch--theme-cache (+dispatch--compute-theme-colors)))

(defun +dispatch--compute-theme-colors ()
  "Compute SVG color scheme from current Emacs faces."
  (let* ((bg  (face-background 'default))
         (fg  (face-foreground 'default))
         (ok  (or (face-foreground 'success nil t) "#b6e63e"))
         (wrn (or (face-foreground 'warning nil t) "#e2c770"))
         (err (or (face-foreground 'error nil t) "#e74c3c"))
         (dim (or (face-foreground 'font-lock-comment-face nil t) "#75715e"))
         (fnc (or (face-foreground 'font-lock-function-name-face nil t) fg))
         (tint 0.2))
    (list :bg bg :fg fg :name-fg fnc :dim dim
          :font (replace-regexp-in-string
                 "\\\\" ""
                 (or (ignore-errors
                       (symbol-name (font-get (face-attribute 'default :font) :family)))
                     "monospace"))
          :arrow (+dispatch--blend dim bg 0.6)
          :pill-bg (+dispatch--blend dim bg 0.25)
          :status
          `((done       :bg ,(+dispatch--blend ok  bg tint) :fg ,ok  :icon "✓")
            (working    :bg ,(+dispatch--blend wrn bg tint) :fg ,wrn :icon "⠹")
            (permission :bg ,(+dispatch--blend err bg tint) :fg ,err :icon "🔒")
            (waiting    :bg ,(+dispatch--blend dim bg 0.1)  :fg ,dim :icon "◦")
            (error      :bg ,(+dispatch--blend err bg tint) :fg ,err :icon "✗")
            (dead       :bg ,(+dispatch--blend dim bg 0.05) :fg ,dim :icon "?")))))

(defun +dispatch--compute-levels (tasks-info)
  "Compute topological levels for TASKS-INFO based on :depends-on.
Returns tasks-info with :level added to each entry."
  (let ((id-to-task (make-hash-table :test 'equal))
        (id-to-level (make-hash-table :test 'equal)))
    (dolist (task tasks-info)
      (puthash (plist-get task :id) task id-to-task))
    (cl-labels ((compute (id)
                  (or (gethash id id-to-level)
                      (let* ((task (gethash id id-to-task))
                             (deps (plist-get task :depends-on))
                             (level (if deps
                                        (1+ (cl-loop for d in deps maximize (compute d)))
                                      0)))
                        (puthash id level id-to-level)
                        level))))
      (dolist (task tasks-info) (compute (plist-get task :id))))
    (mapcar (lambda (task)
              (append task (list :level (gethash (plist-get task :id) id-to-level))))
            tasks-info)))

(defun +dispatch--transitive-reduce (tasks-info)
  "Compute edges for TASKS-INFO with transitive reduction.
Returns list of (from-id . to-id) pairs, plus edges from \"start\" and to \"end\"."
  (let* ((id-to-task (make-hash-table :test 'equal))
         (id-to-deps (make-hash-table :test 'equal))
         (depended-on (make-hash-table :test 'equal))
         (edges nil))
    (dolist (task tasks-info)
      (let ((id (plist-get task :id))
            (deps (plist-get task :depends-on)))
        (puthash id task id-to-task)
        (puthash id deps id-to-deps)
        (dolist (d deps) (puthash d t depended-on))))
    ;; Direct dependency edges (already minimal from user spec)
    (dolist (task tasks-info)
      (let ((id (plist-get task :id))
            (deps (plist-get task :depends-on)))
        (if deps
            (dolist (d deps) (push (cons d id) edges))
          ;; No deps → depends on start
          (push (cons "start" id) edges))))
    ;; Terminal tasks → end
    (dolist (task tasks-info)
      (unless (gethash (plist-get task :id) depended-on)
        (push (cons (plist-get task :id) "end") edges)))
    edges))

(defun +dispatch--draw-arrow (svg x1 y1 x2 y2 color)
  "Draw a curved arrow from (X1,Y1) to (X2,Y2) on SVG with arrowhead.
Uses a cubic bezier curve that bows based on vertical distance."
  (let* ((head-len 5)
         (dx (- x2 x1))
         (cp-offset (* 0.4 (abs dx)))
         ;; Control points: horizontal pull toward target
         (cx1 (+ x1 cp-offset)) (cy1 (float y1))
         (cx2 (- x2 cp-offset)) (cy2 (float y2))
         ;; Shorten endpoint for arrowhead
         (ex (- (float x2) head-len)) (ey (float y2))
         ;; Arrowhead triangle pointing right
         (ax1 (- ex head-len)) (ay1 (- ey 4.0))
         (ax2 (- ex head-len)) (ay2 (+ ey 4.0)))
    (dom-append-child svg
      (dom-node 'path
        `((d . ,(format "M%f,%f C%f,%f %f,%f %f,%f"
                        (float x1) (float y1) cx1 cy1 cx2 cy2 ex ey))
          (stroke . ,color) (stroke-width . "1.5") (fill . "none"))))
    (dom-append-child svg
      (dom-node 'polygon
        `((points . ,(format "%f,%f %f,%f %f,%f"
                             (float x2) (float y2) ax1 ay1 ax2 ay2))
          (fill . ,color))))))

(defun +dispatch--build-svg (tasks-info)
  "Build a horizontal dependency graph SVG from TASKS-INFO.
Each entry: (:id ID :name NAME :status STATUS :elapsed STR :detail STR-OR-NIL
             :depends-on (ID...) :level N).
Colors are derived from the current Emacs theme."
  (let* ((theme (+dispatch--theme-colors))
         (status-colors (plist-get theme :status))
         (font (plist-get theme :font))
         (node-h 34) (node-pad 8) (col-gap 50) (margin 10)
         (pill-w 40) (pill-h 28)
         (leveled (if (plist-get (car tasks-info) :level) tasks-info
                    (+dispatch--compute-levels tasks-info)))
         (max-level (cl-loop for t_ in leveled maximize (plist-get t_ :level)))
         ;; Group by level
         (columns (make-hash-table))
         (_ (dolist (task leveled)
              (push task (gethash (plist-get task :level) columns))))
         (_ (cl-loop for lv from 0 to max-level
                     do (puthash lv (nreverse (gethash lv columns)) columns)))
         (max-col-size (cl-loop for lv from 0 to max-level
                                maximize (length (gethash lv columns))))
         (node-w (+ 40 (* 7 (cl-loop for t_ in leveled
                                      maximize (length (plist-get t_ :name))))))
         (node-w (min node-w 220))
         (w (+ (* 2 margin) pill-w col-gap
               (* (1+ max-level) (+ node-w col-gap))
               pill-w))
         (h (+ (* 2 margin) (* max-col-size (+ node-h node-pad)) (- node-pad)))
         (h (max h 60))
         (svg (svg-create w h))
         (node-edges (make-hash-table :test 'equal))
         (edges (+dispatch--transitive-reduce leveled)))

    ;; Background
    (svg-rectangle svg 0 0 w h :fill (plist-get theme :bg) :rx 8)

    ;; Start pill
    (let* ((sx margin) (sy (/ h 2)))
      (svg-rectangle svg sx (- sy (/ pill-h 2))
                     pill-w pill-h :fill (plist-get theme :pill-bg) :rx 14)
      (svg-text svg "▶" :x (+ sx (/ pill-w 2)) :y (+ sy 5)
                :fill (plist-get theme :dim) :font-size 14 :font-family font
                :text-anchor "middle")
      (puthash "start" (list :left-x sx :right-x (+ sx pill-w) :cy sy) node-edges))

    ;; Task nodes by level
    (cl-loop for lv from 0 to max-level
             for col-tasks = (gethash lv columns)
             for n = (length col-tasks)
             for col-x = (+ margin pill-w col-gap (* lv (+ node-w col-gap)))
             for col-h = (+ (* n node-h) (* (1- (max n 1)) node-pad))
             for start-y = (/ (- h col-h) 2)
             do (cl-loop for task in col-tasks
                         for i from 0
                         for y = (+ start-y (* i (+ node-h node-pad)))
                         for id = (plist-get task :id)
                         for name = (plist-get task :name)
                         for status = (plist-get task :status)
                         for elapsed = (or (plist-get task :elapsed) "")
                         for sc = (cdr (assq status status-colors))
                         for bg = (plist-get sc :bg)
                         for fg = (plist-get sc :fg)
                         for icon = (plist-get sc :icon)
                         do
                         (svg-rectangle svg col-x y node-w node-h :fill bg :rx 5)
                         (svg-text svg (format "%s %s" icon
                                              (if (> (length name) 24)
                                                  (concat (substring name 0 22) "…")
                                                name))
                                   :x (+ col-x 8) :y (+ y 22)
                                   :fill fg :font-size 12 :font-family font)
                         (when (> (length elapsed) 0)
                           (svg-text svg elapsed
                                     :x (+ col-x node-w -8) :y (+ y 22)
                                     :fill (plist-get theme :dim)
                                     :font-size 10 :font-family font
                                     :text-anchor "end"))
                         (puthash id (list :left-x col-x
                                           :right-x (+ col-x node-w)
                                           :cy (+ y (/ node-h 2)))
                                  node-edges)))

    ;; End pill
    (let* ((ex (+ margin pill-w col-gap (* (1+ max-level) (+ node-w col-gap))))
           (ey (/ h 2)))
      (svg-rectangle svg ex (- ey (/ pill-h 2))
                     pill-w pill-h :fill (plist-get theme :pill-bg) :rx 14)
      (svg-text svg "■" :x (+ ex (/ pill-w 2)) :y (+ ey 5)
                :fill (plist-get theme :dim) :font-size 14 :font-family font
                :text-anchor "middle")
      (puthash "end" (list :left-x ex :right-x (+ ex pill-w) :cy ey) node-edges))

    ;; Draw curved arrows
    (dolist (edge edges)
      (let* ((from (gethash (car edge) node-edges))
             (to (gethash (cdr edge) node-edges)))
        (when (and from to)
          (+dispatch--draw-arrow svg
                                 (plist-get from :right-x) (plist-get from :cy)
                                 (plist-get to :left-x) (plist-get to :cy)
                                 (plist-get theme :arrow)))))

    svg))

(defun +dispatch--resolve-status (task statuses)
  "Determine effective status for TASK given STATUSES hash.
Returns (STATUS ELAPSED DETAIL STARTED) list."
  (let* ((id (plist-get task :id))
         (agent-buf (plist-get task :agent))
         (buf (get-buffer agent-buf))
         (alive (and buf (get-buffer-process buf)))
         (busy (and buf (buffer-local-value 'shell-maker--busy buf)))
         (perm (member agent-buf +meta-agent-shell--pending-permission-agents))
         (reported (gethash id statuses))
         (rep-status (plist-get reported :status))
         (rep-detail (plist-get reported :detail))
         (started (plist-get reported :started))
         (effective (cond
                     ((eq rep-status 'done) 'done)
                     ((eq rep-status 'error) 'error)
                     (perm 'permission)
                     ((and alive busy) 'working)
                     ((eq rep-status 'working) 'working)
                     ((and alive (not busy) started) 'done)
                     (alive 'waiting)
                     (t 'dead))))
    ;; Record start time on first working state
    (when (and (eq effective 'working) (not started))
      (puthash id (list :status 'working :detail rep-detail
                        :updated (current-time) :started (current-time))
               statuses)
      (setq started (current-time)))
    (list effective
          (if started (+dispatch--format-elapsed started) nil)
          (and rep-detail (memq effective '(working permission)) rep-detail)
          started)))

(defun +dispatch--render ()
  "Render the dispatch task graph as an SVG in a progress fragment.
Combines agent-reported status with shell-maker--busy fallback."
  (when-let* ((state +meta-agent-shell--dispatch-state)
              (dispatcher (plist-get state :dispatcher-buffer))
              (tasks (plist-get state :tasks))
              (statuses (plist-get state :statuses))
              ((get-buffer dispatcher)))
    ;; Cycle spinner
    (cl-incf +dispatch--spinner-index)
    (let ((spinner (nth (% +dispatch--spinner-index (length +dispatch--spinner-frames))
                        +dispatch--spinner-frames)))
      ;; Update spinner icon in theme color scheme
      (plist-put (cdr (assq 'working (plist-get (+dispatch--theme-colors) :status)))
                 :icon spinner))
    ;; Resolve statuses and build render data
    (let* ((total (length tasks))
           (ready-count 0) (busy-count 0)
           (render-data
            (cl-loop for task in tasks
                     for (effective elapsed detail _started)
                       = (+dispatch--resolve-status task statuses)
                     when (eq effective 'done) do (cl-incf ready-count)
                     when (memq effective '(working permission)) do (cl-incf busy-count)
                     collect (list :id (plist-get task :id)
                                   :name (plist-get task :name)
                                   :status effective
                                   :elapsed elapsed
                                   :detail detail
                                   :depends-on (plist-get task :depends-on))))
           (svg (+dispatch--build-svg render-data))
           (img (svg-image svg :scale 1.0))
           (body (propertize "dispatch-graph" 'display img)))
      (let ((buf (get-buffer-create "*dispatch-progress*")))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (propertize " " 'display img))
            (setq buffer-read-only t
                  cursor-type nil
                  mode-line-format nil
                  header-line-format nil)))
        ;; Ensure the window exists, attached below the dispatcher
        (unless (get-buffer-window buf)
          (when-let* ((agent-win (get-buffer-window dispatcher)))
            (let ((win (split-window agent-win -6 'below)))
              (set-window-buffer win buf)
              (set-window-dedicated-p win t)
              (set-window-parameter win 'no-other-window t)
              (set-window-parameter win 'no-delete-other-windows t))))
        (when-let* ((win (get-buffer-window buf)))
          (fit-window-to-buffer win))))))

(defun +dispatch-start (dispatcher-buffer tasks &optional interval)
  "Start the dispatch task graph in a window below the dispatcher.
DISPATCHER-BUFFER is the dispatcher's agent-shell buffer name.
TASKS is a list of plists: ((:id ID :name NAME :agent AGENT-BUF) ...).
INTERVAL is seconds between render updates (default 2)."
  (+dispatch-stop)
  (setq +meta-agent-shell--pending-permission-agents nil)
  (let ((interval (or interval 2)))
    (setq +meta-agent-shell--dispatch-state
          (list :dispatcher-buffer dispatcher-buffer
                :tasks tasks
                :statuses (make-hash-table :test 'equal)
                :timer (run-with-timer 1 interval #'+dispatch--render)))))

(defun +dispatch-stop ()
  "Stop the dispatch render timer and close the progress window."
  (when-let* ((state +meta-agent-shell--dispatch-state)
              (timer (plist-get state :timer)))
    (cancel-timer timer))
  (when-let* ((win (get-buffer-window "*dispatch-progress*")))
    (delete-window win))
  (when (get-buffer "*dispatch-progress*")
    (kill-buffer "*dispatch-progress*"))
  (setq +meta-agent-shell--dispatch-state nil))

;; Backward-compat aliases for old skill API
(defun +meta-agent-shell-start-progress-polling (dispatcher-buffer agents &optional interval)
  "Backward-compat wrapper. Converts alist to task plist format."
  (+dispatch-start dispatcher-buffer
                   (cl-loop for agent in agents
                            for i from 1
                            collect (list :id (format "task-%d" i)
                                         :name (cdr agent)
                                         :agent (car agent)))
                   interval))
(defalias '+meta-agent-shell-stop-progress-polling #'+dispatch-stop)

(defun +meta-agent-shell-kill-agents ()
  "Kill all agent-shell sessions except the current buffer.
Returns the number of agents killed."
  (let ((self (buffer-name))
        (killed 0))
    (dolist (session (meta-agent-shell-list-sessions))
      (let ((buf-name (plist-get session :buffer)))
        (unless (equal buf-name self)
          (when-let* ((buf (get-buffer buf-name))
                      (proc (get-buffer-process buf)))
            (set-process-query-on-exit-flag proc nil)
            (delete-process proc))
          (when (get-buffer buf-name)
            (kill-buffer buf-name))
          (cl-incf killed))))
    killed))

;; -- Start function for spawned agents --

(defun +meta-agent-shell-start (_arg &optional _buffer-name)
  "Start a new Claude agent-shell for meta-agent-shell.
No window popup, no session prompt. Copies the session mode from the
primary (dispatcher) buffer. Permissions are rendered in the dispatcher buffer."
  (let* ((config (copy-alist (agent-shell-anthropic-make-claude-code-config)))
         (mode-id (or (when-let* ((primary +meta-agent-shell--primary-buffer)
                                  (pbuf (get-buffer primary)))
                        (with-current-buffer pbuf
                          (map-nested-elt agent-shell--state '(:session :mode-id))))
                      "default"))
         (buf nil))
    (setf (map-elt config :default-session-mode-id)
          (lambda () mode-id))
    (setq buf (agent-shell--start :config config
                                  :no-focus t
                                  :new-session t
                                  :session-strategy 'new))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq-local agent-shell-permission-responder-function
                    #'+meta-agent-shell-forward-permission)))
    buf))
