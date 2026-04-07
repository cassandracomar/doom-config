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
        (cond
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

(defun +meta-agent-shell--make-respond-action (respond option-id agent-buf perm-id target-name)
  "Create a permission button action that responds and cleans up."
  (lambda ()
    (interactive)
    (funcall respond option-id)
    (setq +meta-agent-shell--pending-permission-agents
          (delete agent-buf +meta-agent-shell--pending-permission-agents))
    (+meta-agent-shell--cleanup-permission perm-id target-name agent-buf option-id)))

(defun +meta-agent-shell--make-permission-buttons (options keymap respond agent-buf perm-id target-name)
  "Build permission button string from OPTIONS, binding keys in KEYMAP."
  (mapconcat
   (lambda (opt)
     (let ((action (+meta-agent-shell--make-respond-action
                    respond (map-elt opt :option-id) agent-buf perm-id target-name)))
       (when-let* ((char-str (map-elt opt :char)))
         (define-key keymap (kbd char-str) action))
       (agent-shell--make-permission-button
        :text (map-elt opt :label)
        :help (map-elt opt :label)
        :action action
        :keymap keymap
        :char (map-elt opt :char)
        :option (or (map-elt opt :option) (map-elt opt :label))
        :navigatable t)))
   options " "))

(defun +meta-agent-shell--format-permission (agent-buf title kind buttons)
  "Format the permission dialog text."
  (format "\n╭─\n\n    %s %s %s\n\n\n    %s\n\n\n    %s\n\n\n╰─\n"
          (propertize "⚠" 'font-lock-face 'warning)
          (propertize (format "Permission: %s" agent-buf) 'font-lock-face 'bold)
          (propertize "⚠" 'font-lock-face 'warning)
          (propertize (format "%s (%s)" title kind) 'font-lock-face 'comint-highlight-input)
          buttons))

(defun +meta-agent-shell-forward-permission (permission)
  "Render PERMISSION from a background agent in the dispatcher buffer."
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
           (buttons (+meta-agent-shell--make-permission-buttons
                     options keymap respond agent-buf perm-id (buffer-name target)))
           (text (+meta-agent-shell--format-permission agent-buf title kind buttons)))
      (put-text-property 0 (length text) 'keymap keymap text)
      (put-text-property 0 (length text) '+meta-agent-shell-perm-id perm-id text)
      (+meta-agent-shell--insert-before-prompt target text))
    t))

;; -- Dispatch task graph and progress rendering --

(defvar +dispatch--layout
  '(:node-h 34 :node-pad 8 :col-gap 50 :margin 10
    :pill-w 40 :pill-h 28 :pill-rx 14
    :node-rx 5 :node-max-w 220 :node-base-w 40 :node-char-w 7
    :node-text-x 8 :node-text-y 22 :node-elapsed-pad 8
    :node-font-size 12 :pill-font-size 14 :elapsed-font-size 10
    :pill-text-y-offset 5 :name-max-len 24
    :arrow-head-len 5 :arrow-head-hw 4.0 :arrow-cp-factor 0.4
    :arrow-stroke-w "1.5" :bg-rx 8)
  "Layout constants for the SVG task graph.")

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

(defun +dispatch--resolve-face (face attr)
  "Resolve FACE's ATTR respecting face-remapping-alist in the dispatcher buffer."
  (let* ((buf (or (and +meta-agent-shell--primary-buffer
                       (get-buffer +meta-agent-shell--primary-buffer))
                  (current-buffer)))
         (remap (with-current-buffer buf
                  (alist-get face face-remapping-alist)))
         (resolved (if (and remap (facep (car remap))) (car remap) face)))
    (funcall (if (eq attr :background) #'face-background #'face-foreground)
             resolved nil t)))

(defun +dispatch--compute-theme-colors ()
  "Compute SVG color scheme from current Emacs faces.
Respects face remapping (e.g. solaire-mode) in the dispatcher buffer."
  (let* ((bg  (or (+dispatch--resolve-face 'default :background)
                  (face-background 'default)))
         (fg  (or (+dispatch--resolve-face 'default :foreground)
                  (face-foreground 'default)))
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
          :arrow (doom-blend dim bg 0.6)
          :pill-bg (doom-blend dim bg 0.25)
          :status
          `((done       :bg ,(doom-blend ok  bg tint) :fg ,ok  :icon "✓")
            (working    :bg ,(doom-blend wrn bg tint) :fg ,wrn :icon "⠹")
            (permission :bg ,(doom-blend err bg tint) :fg ,err :icon "🔒")
            (waiting    :bg ,(doom-blend dim bg 0.1)  :fg ,dim :icon "◦")
            (error      :bg ,(doom-blend err bg tint) :fg ,err :icon "✗")
            (dead       :bg ,(doom-blend dim bg 0.05) :fg ,dim :icon "?")))))

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

(defun +dispatch--draw-arrow (svg x1 y1 x2 y2 color &optional bypass-y)
  "Draw a curved arrow from (X1,Y1) to (X2,Y2) on SVG with arrowhead.
If BYPASS-Y is non-nil, route the arrow through that Y coordinate
to avoid crossing intermediate boxes."
  (let* ((L +dispatch--layout)
         (head-len (plist-get L :arrow-head-len))
         (hw (plist-get L :arrow-head-hw))
         (ex (- (float x2) head-len)) (ey (float y2))
         (sw (plist-get L :arrow-stroke-w)))
    ;; Path
    (dom-append-child svg
      (dom-node 'path
        `((d . ,(if bypass-y
                    ;; Both control points at bypass-y: arc stays high/low
                    (let ((by (float bypass-y)))
                      (format "M%f,%f C%f,%f %f,%f %f,%f"
                              (float x1) (float y1)
                              (+ x1 20.0) by
                              (- ex 20.0) by
                              ex ey))
                  ;; Direct bezier
                  (let ((cp (* (plist-get L :arrow-cp-factor) (abs (- x2 x1)))))
                    (format "M%f,%f C%f,%f %f,%f %f,%f"
                            (float x1) (float y1)
                            (+ x1 cp) (float y1)
                            (- x2 cp) (float y2)
                            ex ey))))
          (stroke . ,color) (stroke-width . ,sw) (fill . "none"))))
    ;; Arrowhead
    (dom-append-child svg
      (dom-node 'polygon
        `((points . ,(format "%f,%f %f,%f %f,%f"
                             (float x2) (float y2)
                             (- ex head-len) (- ey hw)
                             (- ex head-len) (+ ey hw)))
          (fill . ,color))))))

(defun +dispatch--group-by-level (tasks)
  "Group TASKS by :level into a hash of level → task list (preserving order)."
  (let ((columns (make-hash-table))
        (max-level (cl-loop for t_ in tasks maximize (plist-get t_ :level))))
    (dolist (task tasks)
      (push task (gethash (plist-get task :level) columns)))
    (cl-loop for lv from 0 to max-level
             do (puthash lv (nreverse (gethash lv columns)) columns))
    columns))

(defun +dispatch--draw-pill (svg x cy w h icon theme)
  "Draw a pill-shaped node on SVG at X, centered at CY. Returns edge positions."
  (let ((L +dispatch--layout))
    (svg-rectangle svg x (- cy (/ h 2)) w h
                   :fill (plist-get theme :pill-bg)
                   :rx (plist-get L :pill-rx))
    (svg-text svg icon :x (+ x (/ w 2)) :y (+ cy (plist-get L :pill-text-y-offset))
              :fill (plist-get theme :dim)
              :font-size (plist-get L :pill-font-size)
              :font-family (plist-get theme :font) :text-anchor "middle")
    (list :left-x x :right-x (+ x w) :cy cy)))

(defun +dispatch--wrap-text (text max-chars)
  "Wrap TEXT into lines of at most MAX-CHARS, breaking at word boundaries."
  (if (<= (length text) max-chars)
      (list text)
    (let ((words (split-string text " ")) lines current-line)
      (dolist (word words)
        (if (and current-line
                 (> (+ (length current-line) 1 (length word)) max-chars))
            (progn (push current-line lines)
                   (setq current-line word))
          (setq current-line (if current-line
                                 (concat current-line " " word)
                               word))))
      (when current-line (push current-line lines))
      (nreverse lines))))

(defun +dispatch--task-node-height (task node-w)
  "Compute the height needed for TASK given NODE-W."
  (let* ((L +dispatch--layout)
         (char-w (plist-get L :node-char-w))
         (text-pad (+ (plist-get L :node-text-x) (plist-get L :node-elapsed-pad) 20))
         (avail-chars (max 10 (/ (- node-w text-pad) (max 1 char-w))))
         (name (plist-get task :name))
         (lines (+dispatch--wrap-text name avail-chars))
         (line-h (plist-get L :node-font-size))
         (base-h (plist-get L :node-h)))
    (if (> (length lines) 1)
        (+ base-h (* (1- (length lines)) (+ line-h 2)))
      base-h)))

(defun +dispatch--draw-task-node (svg x y w h task theme)
  "Draw a task node on SVG. H is the pre-computed height. Returns edge positions."
  (let* ((L +dispatch--layout)
         (status (plist-get task :status))
         (name (plist-get task :name))
         (elapsed (or (plist-get task :elapsed) ""))
         (sc (cdr (assq status (plist-get theme :status))))
         (font (plist-get theme :font))
         (char-w (plist-get L :node-char-w))
         (text-pad (+ (plist-get L :node-text-x) (plist-get L :node-elapsed-pad) 20))
         (avail-chars (max 10 (/ (- w text-pad) (max 1 char-w))))
         (lines (+dispatch--wrap-text name avail-chars))
         (icon (plist-get sc :icon))
         (font-size (plist-get L :node-font-size))
         (line-h (+ font-size 2))
         (text-x (+ x (plist-get L :node-text-x)))
         (text-y (+ y (plist-get L :node-text-y))))
    (svg-rectangle svg x y w h :fill (plist-get sc :bg)
                   :rx (plist-get L :node-rx))
    ;; First line with icon
    (svg-text svg (format "%s %s" icon (car lines))
              :x text-x :y text-y :fill (plist-get sc :fg)
              :font-size font-size :font-family font)
    ;; Continuation lines
    (cl-loop for line in (cdr lines)
             for i from 1
             do (svg-text svg (format "  %s" line)
                          :x text-x :y (+ text-y (* i line-h))
                          :fill (plist-get sc :fg)
                          :font-size font-size :font-family font))
    ;; Elapsed time on first line
    (when (> (length elapsed) 0)
      (svg-text svg elapsed
                :x (- (+ x w) (plist-get L :node-elapsed-pad))
                :y text-y
                :fill (plist-get theme :dim)
                :font-size (plist-get L :elapsed-font-size)
                :font-family font :text-anchor "end"))
    (list :left-x x :right-x (+ x w) :cy (+ y (/ h 2)))))

(defun +dispatch--build-svg (tasks-info)
  "Build a horizontal dependency graph SVG from TASKS-INFO.
Colors derived from Emacs theme, dimensions from `+dispatch--layout'."
  (let* ((L +dispatch--layout)
         (theme (+dispatch--theme-colors))
         (node-h (plist-get L :node-h))
         (node-pad (plist-get L :node-pad))
         (col-gap (plist-get L :col-gap))
         (margin (plist-get L :margin))
         (pill-w (plist-get L :pill-w))
         (pill-h (plist-get L :pill-h))
         (leveled (if (plist-get (car tasks-info) :level) tasks-info
                    (+dispatch--compute-levels tasks-info)))
         (max-level (cl-loop for t_ in leveled maximize (plist-get t_ :level)))
         (columns (+dispatch--group-by-level leveled))
         (max-col-size (cl-loop for lv from 0 to max-level
                                maximize (length (gethash lv columns))))
         (node-w (min (+ (plist-get L :node-base-w)
                         (* (plist-get L :node-char-w)
                            (cl-loop for t_ in leveled
                                     maximize (length (plist-get t_ :name)))))
                      (plist-get L :node-max-w)))
         ;; Pre-compute per-task heights for wrapping
         (task-heights (make-hash-table :test 'equal))
         (_ (dolist (task leveled)
              (puthash (plist-get task :id)
                       (+dispatch--task-node-height task node-w)
                       task-heights)))
         ;; Column height = sum of task heights + padding
         (max-col-h (cl-loop for lv from 0 to max-level
                             maximize (let ((col (gethash lv columns)))
                                        (+ (cl-loop for t_ in col
                                                    sum (gethash (plist-get t_ :id) task-heights))
                                           (* (1- (max (length col) 1)) node-pad)))))
         (w (+ (* 2 margin) pill-w col-gap
               (* (1+ max-level) (+ node-w col-gap)) pill-w))
         (h (max (+ (* 2 margin) max-col-h) 60))
         (svg (svg-create w h))
         (node-edges (make-hash-table :test 'equal))
         (edges (+dispatch--transitive-reduce leveled)))

    (svg-rectangle svg 0 0 w h :fill (plist-get theme :bg)
                   :rx (plist-get L :bg-rx))

    (puthash "start"
             (+dispatch--draw-pill svg margin (/ h 2) pill-w pill-h "▶" theme)
             node-edges)

    (cl-loop for lv from 0 to max-level
             for col-tasks = (gethash lv columns)
             for col-x = (+ margin pill-w col-gap (* lv (+ node-w col-gap)))
             for col-h = (+ (cl-loop for t_ in col-tasks
                                     sum (gethash (plist-get t_ :id) task-heights))
                            (* (1- (max (length col-tasks) 1)) node-pad))
             for cur-y = (/ (- h col-h) 2)
             do (cl-loop for task in col-tasks
                         for th = (gethash (plist-get task :id) task-heights)
                         do (puthash (plist-get task :id)
                                     (+dispatch--draw-task-node
                                      svg col-x cur-y node-w th task theme)
                                     node-edges)
                            (cl-incf cur-y (+ th node-pad))))

    (puthash "end"
             (+dispatch--draw-pill svg
                                   (+ margin pill-w col-gap
                                      (* (1+ max-level) (+ node-w col-gap)))
                                   (/ h 2) pill-w pill-h "■" theme)
             node-edges)

    ;; Build column bounds for bypass routing
    (let ((col-bounds (make-hash-table)))
      ;; Track top/bottom of each level's nodes
      (maphash (lambda (id edges)
                 (unless (member id '("start" "end"))
                   (let* ((task (cl-find-if (lambda (t_) (equal (plist-get t_ :id) id)) leveled))
                          (lv (and task (plist-get task :level)))
                          (cy (plist-get edges :cy))
                          (th (and task (gethash id task-heights)))
                          (top (- cy (/ (or th node-h) 2)))
                          (bot (+ cy (/ (or th node-h) 2))))
                     (when lv
                       (let ((cur (gethash lv col-bounds)))
                         (puthash lv (list :top (if cur (min (plist-get cur :top) top) top)
                                           :bot (if cur (max (plist-get cur :bot) bot) bot))
                                  col-bounds))))))
               node-edges)
      ;; Store levels for start/end
      (let ((id-to-level (make-hash-table :test 'equal)))
        (puthash "start" -1 id-to-level)
        (puthash "end" (1+ max-level) id-to-level)
        (dolist (task leveled)
          (puthash (plist-get task :id) (plist-get task :level) id-to-level))
        ;; Draw arrows with bypass for multi-level edges
        (dolist (edge edges)
          (when-let* ((from (gethash (car edge) node-edges))
                      (to (gethash (cdr edge) node-edges)))
            (let* ((from-lv (gethash (car edge) id-to-level))
                   (to-lv (gethash (cdr edge) id-to-level))
                   (span (and from-lv to-lv (- to-lv from-lv)))
                   ;; Need bypass if spanning >1 level and intermediate levels have boxes
                   (from-cy (plist-get from :cy))
                   (bypass-y
                    (when (and span (> span 1))
                      (let ((min-top h) (max-bot 0) (has-intermediate nil))
                        (cl-loop for lv from (1+ from-lv) below to-lv
                                 for bounds = (gethash lv col-bounds)
                                 when bounds
                                   do (setq has-intermediate t
                                            min-top (min min-top (plist-get bounds :top))
                                            max-bot (max max-bot (plist-get bounds :bot))))
                        (when has-intermediate
                          ;; Route above if source is above center, below if below
                          (let ((pad (+ (plist-get L :node-pad) 6)))
                            (if (< from-cy (/ h 2))
                                (- min-top pad)
                              (+ max-bot pad))))))))
              (+dispatch--draw-arrow svg
                                     (plist-get from :right-x) (plist-get from :cy)
                                     (plist-get to :left-x) (plist-get to :cy)
                                     (plist-get theme :arrow)
                                     bypass-y))))))
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

(defun +dispatch--scroll-right ()
  "Scroll the dispatch progress window right."
  (interactive)
  (scroll-left 8))

(defun +dispatch--scroll-left ()
  "Scroll the dispatch progress window left."
  (interactive)
  (scroll-right 8))

(defun +dispatch--auto-scroll (win render-data)
  "Scroll WIN so the first non-done task is one node from the left edge."
  (let* ((L +dispatch--layout)
         (margin (plist-get L :margin))
         (pill-w (plist-get L :pill-w))
         (col-gap (plist-get L :col-gap))
         (node-w (min (+ (plist-get L :node-base-w)
                         (* (plist-get L :node-char-w)
                            (cl-loop for t_ in render-data
                                     maximize (length (plist-get t_ :name)))))
                      (plist-get L :node-max-w)))
         (leveled (if (plist-get (car render-data) :level) render-data
                    (+dispatch--compute-levels render-data)))
         ;; Find level of first non-done task
         (target-level (or (cl-loop for task in leveled
                                    unless (eq (plist-get task :status) 'done)
                                    minimize (plist-get task :level))
                           0))
         ;; Show one completed level to the left of target
         (show-level (max 0 (1- target-level)))
         ;; Compute pixel offset for that level
         (scroll-px (max 0 (- (+ margin pill-w col-gap
                                 (* show-level (+ node-w col-gap)))
                              margin)))
         ;; Convert pixels to columns (approximate)
         (char-w (frame-char-width))
         (scroll-cols (/ scroll-px char-w)))
    (set-window-hscroll win scroll-cols)))

(defun +dispatch--render ()
  "Render the dispatch task graph as an SVG in a progress window.
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
           (img (svg-image svg :scale 1.0)))
      (let ((buf (get-buffer-create "*dispatch-progress*"))
            (bg (plist-get (+dispatch--theme-colors) :bg)))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (propertize " " 'display img))
            (setq buffer-read-only t
                  cursor-type nil
                  mode-line-format nil
                  header-line-format nil
                  truncate-lines t)
            (face-remap-add-relative 'default :background bg)
            ;; Mouse horizontal scrolling
            (unless (local-variable-p '+dispatch--scroll-keys-set)
              (local-set-key [wheel-right] #'+dispatch--scroll-right)
              (local-set-key [wheel-left] #'+dispatch--scroll-left)
              (local-set-key [S-wheel-down] #'+dispatch--scroll-right)
              (local-set-key [S-wheel-up] #'+dispatch--scroll-left)
              (setq-local +dispatch--scroll-keys-set t))))
        ;; Ensure the window exists
        (unless (get-buffer-window buf)
          (let* ((svg-w (dom-attr svg 'width))
                 (agent-win (get-buffer-window dispatcher))
                 (agent-pw (and agent-win (window-body-width agent-win t))))
            (if (and agent-pw svg-w (> svg-w agent-pw))
                ;; Wide graph: use full-frame side window
                (display-buffer-in-side-window buf
                  '((side . bottom)
                    (window-height . fit-window-to-buffer)
                    (dedicated . t)))
              ;; Fits: split below agent-shell
              (when (and agent-win (> (window-height agent-win) 10))
                (let ((win (split-window agent-win -6 'below)))
                  (set-window-buffer win buf)
                  (set-window-dedicated-p win t)
                  (set-window-parameter win 'no-other-window t)
                  (set-window-parameter win 'no-delete-other-windows t))))))
        (when-let* ((win (get-buffer-window buf)))
          (set-window-parameter win 'no-other-window t)
          (set-window-parameter win 'no-delete-other-windows t)
          (ignore-errors (fit-window-to-buffer win))
          ;; Only auto-scroll when graph is wider than window
          (let* ((svg-w (dom-attr svg 'width))
                 (win-pw (window-body-width win t)))
            (if (and svg-w win-pw (> svg-w win-pw))
                (+dispatch--auto-scroll win render-data)
              (set-window-hscroll win 0))))))))

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

(defun +dispatch-start-linear (buffer-name task-names &optional interval)
  "Start a linear task graph for single-agent sequential work.
BUFFER-NAME is the agent-shell buffer.
TASK-NAMES is a list of strings, chained as a sequential dependency graph.
The agent should call (+dispatch-report ID STATUS DETAIL) as it works."
  (let ((prev nil))
    (+dispatch-start buffer-name
                     (cl-loop for name in task-names
                              for i from 1
                              for id = (format "step-%d" i)
                              for deps = (when prev (list prev))
                              do (setq prev id)
                              collect (list :id id :name name
                                           :agent buffer-name
                                           :depends-on deps))
                     interval)))

(defun +meta-agent-shell-kill-agents ()
  "Kill all agent-shell sessions except the current buffer.
Also stops dispatch polling and closes the progress window.
Returns the number of agents killed."
  (+dispatch-stop)
  (cl-loop with self = (buffer-name)
           for session in (meta-agent-shell-list-sessions)
           for buf-name = (plist-get session :buffer)
           unless (equal buf-name self)
             do (when-let* ((buf (get-buffer buf-name))
                            (proc (get-buffer-process buf)))
                  (set-process-query-on-exit-flag proc nil)
                  (delete-process proc))
                (when-let* ((buf (get-buffer buf-name)))
                  (kill-buffer buf))
             and count t))

;; -- Start function for spawned agents --

(defun +meta-agent-shell-start (config _arg &optional buffer-name)
  "Start a new Claude agent-shell for meta-agent-shell.
No window popup, no session prompt. Copies the session mode from the
primary (dispatcher) buffer. Permissions are rendered in the dispatcher buffer.
BUFFER-NAME, if provided, is incorporated into the buffer label."
  (let* ((cfg (copy-alist config))
         (mode-id (or (when-let* ((primary +meta-agent-shell--primary-buffer)
                                  (pbuf (get-buffer primary)))
                        (with-current-buffer pbuf
                          (map-nested-elt agent-shell--state '(:session :mode-id))))
                      "default"))
         (buf nil))
    (setf (map-elt cfg :default-session-mode-id)
          (lambda () mode-id))
    (setf (map-elt cfg :buffer-name)
          (if buffer-name
              (format "[agent:%s]" buffer-name)
            "[agent]"))
    (setq buf (agent-shell--start :config cfg
                                  :no-focus t
                                  :new-session t
                                  :session-strategy 'new))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq-local agent-shell-permission-responder-function
                    #'+meta-agent-shell-forward-permission)))
    buf))
