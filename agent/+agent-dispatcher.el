;;; +agent-dispatcher.el -*- lexical-binding: t; -*-

;; Permission forwarding, progress polling, and agent spawning for
;; meta-agent-shell dispatch workflows.

(require 'cl-lib)
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

(defun +meta-agent-shell--insert-before-prompt (buf text)
  "Insert TEXT into BUF before the prompt, or at point-max if busy."
  (with-current-buffer buf
    (save-excursion
      (let ((inhibit-read-only t))
        (if shell-maker--busy
            (progn (goto-char (point-max)) (insert text))
          (if-let* ((proc (get-buffer-process (current-buffer))))
              (progn
                (goto-char (process-mark proc))
                (forward-line 0)
                (insert text "\n"))
            (goto-char (point-max))
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

(defvar +dispatch--svg-colors
  '((done       :bg "#2a4a2a" :fg "#b6e63e" :icon "✓")
    (working    :bg "#4a3a1a" :fg "#fd971f" :icon "⠹")
    (permission :bg "#4a1a2a" :fg "#f92672" :icon "🔒")
    (waiting    :bg "#2a2a2a" :fg "#75715e" :icon "◦")
    (error      :bg "#4a1a1a" :fg "#f92672" :icon "✗")
    (dead       :bg "#1a1a1a" :fg "#555555" :icon "?"))
  "Color scheme for dispatch SVG task graph.")

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
  "Draw an arrow from (X1,Y1) to (X2,Y2) on SVG with arrowhead."
  (let* ((head-len 6)
         (dx (- x2 x1)) (dy (- y2 y1))
         (len (sqrt (+ (* dx dx) (* dy dy))))
         (ux (/ (float dx) len)) (uy (/ (float dy) len))
         ;; Shorten line so arrowhead meets the target
         (ex (- x2 (* ux head-len))) (ey (- y2 (* uy head-len)))
         ;; Arrowhead points
         (px (- uy)) (py ux)
         (ax1 (- ex (* ux head-len) (- (* px 4))))
         (ay1 (- ey (* uy head-len) (- (* py 4))))
         (ax2 (+ (- ex (* ux head-len)) (- (* px 4))))
         (ay2 (+ (- ey (* uy head-len)) (- (* py 4)))))
    (dom-append-child svg
      (dom-node 'line `((x1 . ,(format "%f" x1)) (y1 . ,(format "%f" y1))
                        (x2 . ,(format "%f" ex)) (y2 . ,(format "%f" ey))
                        (stroke . ,color) (stroke-width . "1.5"))))
    (dom-append-child svg
      (dom-node 'polygon `((points . ,(format "%f,%f %f,%f %f,%f"
                                              (float x2) (float y2)
                                              ax1 ay1 ax2 ay2))
                           (fill . ,color))))))

(defun +dispatch--build-svg (tasks-info)
  "Build a horizontal dependency graph SVG from TASKS-INFO.
Each entry: (:id ID :name NAME :status STATUS :elapsed STR :detail STR-OR-NIL
             :depends-on (ID...) :level N)."
  (let* ((font "Iosevka Nerd Font")
         (node-h 34) (node-pad 8) (col-gap 50) (margin 10)
         (pill-w 40) (pill-h 28)
         (leveled (if (plist-get (car tasks-info) :level) tasks-info
                    (+dispatch--compute-levels tasks-info)))
         (max-level (cl-loop for t_ in leveled maximize (plist-get t_ :level)))
         ;; Group by level
         (columns (make-hash-table))
         (_ (dolist (task leveled)
              (push task (gethash (plist-get task :level) columns))))
         ;; Reverse to preserve order within columns
         (_ (cl-loop for lv from 0 to max-level
                     do (puthash lv (nreverse (gethash lv columns)) columns)))
         ;; Compute column max tasks for height
         (max-col-size (cl-loop for lv from 0 to max-level
                                maximize (length (gethash lv columns))))
         ;; Node width based on longest name
         (node-w (+ 40 (* 7 (cl-loop for t_ in leveled
                                      maximize (length (plist-get t_ :name))))))
         (node-w (min node-w 220))
         ;; Total dimensions
         (total-cols (+ 2 max-level 1))  ;; start + levels + end
         (w (+ (* 2 margin) pill-w col-gap
               (* (1+ max-level) (+ node-w col-gap))
               pill-w))
         (h (+ (* 2 margin) (* max-col-size (+ node-h node-pad)) (- node-pad)))
         (h (max h 60))
         (svg (svg-create w h))
         ;; Node positions: hash of id → (cx . cy)
         (positions (make-hash-table :test 'equal))
         (edges (+dispatch--transitive-reduce leveled))
         (arrow-color "#555566"))

    ;; Background
    (svg-rectangle svg 0 0 w h :fill "#1a1a2e" :rx 8)

    ;; Start pill
    (let* ((sx (+ margin (/ pill-w 2)))
           (sy (/ h 2)))
      (svg-rectangle svg (- sx (/ pill-w 2)) (- sy (/ pill-h 2))
                     pill-w pill-h :fill "#333355" :rx 14)
      (svg-text svg "▶" :x sx :y (+ sy 5)
                :fill "#75715e" :font-size 14 :font-family font :text-anchor "middle")
      (puthash "start" (cons (+ sx (/ pill-w 2)) sy) positions))

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
                         for colors = (cdr (assq status +dispatch--svg-colors))
                         for bg = (plist-get colors :bg)
                         for fg = (plist-get colors :fg)
                         for icon = (plist-get colors :icon)
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
                                     :fill "#75715e" :font-size 10 :font-family font
                                     :text-anchor "end"))
                         (puthash id (cons (+ col-x (/ node-w 2)) (+ y (/ node-h 2)))
                                  positions)))

    ;; End pill
    (let* ((ex (+ margin pill-w col-gap (* (1+ max-level) (+ node-w col-gap))
                  (/ pill-w 2)))
           (ey (/ h 2)))
      (svg-rectangle svg (- ex (/ pill-w 2)) (- ey (/ pill-h 2))
                     pill-w pill-h :fill "#333355" :rx 14)
      (svg-text svg "■" :x ex :y (+ ey 5)
                :fill "#75715e" :font-size 14 :font-family font :text-anchor "middle")
      (puthash "end" (cons (+ ex (/ pill-w 2)) ey) positions))

    ;; Draw arrows
    (dolist (edge edges)
      (let* ((from-pos (gethash (car edge) positions))
             (to-pos (gethash (cdr edge) positions))
             (from-id (car edge))
             (to-id (cdr edge)))
        (when (and from-pos to-pos)
          ;; Arrow from right edge of source to left edge of target
          (let* ((from-x (+ (car from-pos)
                            (if (equal from-id "start") (/ pill-w 2)
                              (/ node-w 2))))
                 (from-y (cdr from-pos))
                 (to-x (- (car to-pos)
                           (if (equal to-id "end") (/ pill-w 2)
                             (/ node-w 2))))
                 (to-y (cdr to-pos)))
            (+dispatch--draw-arrow svg from-x from-y to-x to-y arrow-color)))))

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
      ;; Update spinner icon in color scheme
      (plist-put (cdr (assq 'working +dispatch--svg-colors)) :icon spinner))
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
      (with-current-buffer dispatcher
        (agent-shell-ui-update-fragment
         (agent-shell-ui-make-fragment-model
          :namespace-id "dispatch-progress"
          :block-id "status"
          :label-left (propertize " Dispatch" 'font-lock-face 'font-lock-keyword-face)
          :label-right (propertize
                        (cond
                         ((= ready-count total) (format "[%d/%d ✓ complete]" ready-count total))
                         ((> busy-count 0) (format "[%d/%d done, %d active]" ready-count total busy-count))
                         (t (format "[%d/%d done]" ready-count total)))
                        'font-lock-face
                        (if (= ready-count total) 'success 'font-lock-comment-face))
          :body body)
         :expanded t)))))

(defun +dispatch-start (dispatcher-buffer tasks &optional interval)
  "Start the dispatch task graph.
DISPATCHER-BUFFER is the dispatcher's agent-shell buffer name.
TASKS is a list of plists: ((:id ID :name NAME :agent AGENT-BUF) ...).
INTERVAL is seconds between render updates (default 2)."
  (+dispatch-stop)
  (setq +meta-agent-shell--pending-permission-agents nil)
  (with-current-buffer dispatcher-buffer
    (ignore-errors
      (agent-shell-ui-delete-fragment
       :namespace-id "dispatch-progress"
       :block-id "status")))
  (let ((interval (or interval 2)))
    (setq +meta-agent-shell--dispatch-state
          (list :dispatcher-buffer dispatcher-buffer
                :tasks tasks
                :statuses (make-hash-table :test 'equal)
                :timer (run-with-timer 1 interval #'+dispatch--render)))))

(defun +dispatch-stop ()
  "Stop the dispatch render timer."
  (when-let* ((state +meta-agent-shell--dispatch-state)
              (timer (plist-get state :timer)))
    (cancel-timer timer))
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
