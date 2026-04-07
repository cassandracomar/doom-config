;;; +agent-dispatcher.el -*- lexical-binding: t; -*-

;; Permission forwarding, progress polling, and agent spawning for
;; meta-agent-shell dispatch workflows.

(require 'cl-lib)
(require 'map)
(require 'svg)

(declare-function agent-shell--make-permission-button "agent-shell")
(declare-function agent-shell--start "agent-shell")
(declare-function agent-shell-anthropic-make-claude-code-config "agent-shell")
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

(defconst +dispatch--layout
  '(:node-h 34 :node-pad 8 :col-gap 50 :margin 10
    :pill-w 40 :pill-h 28 :pill-rx 14
    :node-rx 5 :node-max-w 220
    :node-pad-x 4 :node-icon-w 14 :node-text-y 22
    :node-font-size 12 :pill-font-size 14
    :pill-text-y-offset 5 :name-max-len 24
    :arrow-head-len 5 :arrow-head-hw 4.0 :arrow-cp-factor 0.4
    :arrow-stroke-w "1.5" :bg-rx 8
    :stack-vgap 30 :stack-threshold 5 :stack-arrow-overshoot 40)
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
  (let* ((bg  (or (+dispatch--resolve-face 'header-line :background)
                  (face-background 'header-line nil t)
                  (face-background 'default)))
         (fg  (or (+dispatch--resolve-face 'default :foreground)
                  (face-foreground 'default)))
         (ok  (or (face-foreground 'success nil t) "#b6e63e"))
         (wrn (or (face-foreground 'warning nil t) "#e2c770"))
         (err (or (face-foreground 'error nil t) "#e74c3c"))
         (dim (or (face-foreground 'font-lock-comment-face nil t) "#75715e"))
         (fnc (or (face-foreground 'font-lock-function-name-face nil t) fg))
         (tint 0.2))
    ;; Compute SVG char width by measuring actual font rendering.
    ;; string-pixel-width at SVG font-size gives the Emacs-side measurement;
    ;; librsvg may differ slightly, so we apply a 0.85 correction factor.
    (let* ((svg-font-size (plist-get +dispatch--layout :node-font-size))
           (font-family (or (ignore-errors
                              (symbol-name (font-get (face-attribute 'default :font) :family)))
                            "monospace"))
           (sample (propertize "MMMMMMMMMM" 'face
                               `(:family ,(replace-regexp-in-string "\\\\" "" font-family)
                                 :height ,(* svg-font-size 10))))
           (char-w (* 0.85 (/ (float (string-pixel-width sample)) 10.0))))
    (list :bg bg :fg fg :name-fg fnc :dim dim
          :char-w char-w
          :font (replace-regexp-in-string "\\\\" "" font-family)
          :arrow (doom-blend dim bg 0.6)
          :pill-bg (doom-blend dim bg 0.25)
          :status
          `((done       :bg ,(doom-blend ok  bg tint) :fg ,ok  :icon "✓")
            (working    :bg ,(doom-blend wrn bg tint) :fg ,wrn :icon "⠹")
            (permission :bg ,(doom-blend err bg tint) :fg ,err :icon "🔒")
            (waiting    :bg ,(doom-blend dim bg 0.1)  :fg ,dim :icon "◦")
            (error      :bg ,(doom-blend err bg tint) :fg ,err :icon "✗")
            (dead       :bg ,(doom-blend dim bg 0.05) :fg ,dim :icon "?"))))))

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
  "Compute edges for TASKS-INFO.
Returns list of (from-id . to-id) pairs, including start/end connections."
  (let ((depended-on (make-hash-table :test 'equal))
        (edges nil))
    (dolist (task tasks-info)
      (dolist (d (plist-get task :depends-on))
        (puthash d t depended-on)))
    (dolist (task tasks-info)
      (let ((id (plist-get task :id))
            (deps (plist-get task :depends-on)))
        (if deps
            (dolist (d deps) (push (cons d id) edges))
          (push (cons "start" id) edges))
        (unless (gethash id depended-on)
          (push (cons id "end") edges))))
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

(defun +dispatch--draw-stack-arrow (svg top-edges bot-x bot-y bot-w color)
  "Draw internal pair arrow from top node's right edge to bottom node's top center.
TOP-EDGES is the plist returned by draw-task-node for the top node.
BOT-X, BOT-Y are the top-left corner of the bottom node. BOT-W is its width."
  (let* ((L +dispatch--layout)
         (x1 (plist-get top-edges :right-x))
         (y1 (plist-get top-edges :cy))
         (x2 (+ bot-x (/ bot-w 2)))
         (y2 (float bot-y))
         (ey (+ y2 5))
         (overshoot (plist-get L :stack-arrow-overshoot))
         (cp1-x (+ x1 overshoot))
         (cp1-y (float bot-y))
         (cp2-x (float x2))
         (cp2-y (- y2 20.0))
         (hw (plist-get L :arrow-head-hw))
         (sw (plist-get L :arrow-stroke-w)))
    ;; Path
    (dom-append-child svg
      (dom-node 'path
        `((d . ,(format "M%f,%f C%f,%f %f,%f %f,%f"
                        (float x1) (float y1)
                        cp1-x cp1-y
                        cp2-x cp2-y
                        (float x2) ey))
          (stroke . ,color) (stroke-width . ,sw) (fill . "none"))))
    ;; Downward arrowhead
    (dom-append-child svg
      (dom-node 'polygon
        `((points . ,(format "%f,%f %f,%f %f,%f"
                             (float x2) y2
                             (- x2 hw) (- y2 (plist-get L :arrow-head-len))
                             (+ x2 hw) (- y2 (plist-get L :arrow-head-len))))
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

(defun +dispatch--compute-stacks (columns max-level)
  "Identify levels to stack vertically as pairs.
Only activates when there are more than :stack-threshold task columns.
Returns a hash of level -> (:peer-level N :position top|bottom),
or nil if stacking is not needed."
  (when (>= (1+ max-level) (plist-get +dispatch--layout :stack-threshold))
    (let ((stack-map (make-hash-table))
          (lv 0))
      ;; Scan for runs of consecutive single-node levels
      (while (<= lv max-level)
        (if (and (<= (1+ lv) max-level)
                 (= 1 (length (gethash lv columns)))
                 (= 1 (length (gethash (1+ lv) columns))))
            ;; Pair lv (top) with lv+1 (bottom)
            (progn
              (puthash lv (list :peer-level (1+ lv) :position 'top) stack-map)
              (puthash (1+ lv) (list :peer-level lv :position 'bottom) stack-map)
              (cl-incf lv 2))
          (cl-incf lv)))
      (when (> (hash-table-count stack-map) 0)
        stack-map))))

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
    (let ((rx (plist-get L :pill-rx)))
      (list :left-x (+ x rx) :right-x (- (+ x w) rx) :cy cy))))

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

(defun +dispatch--node-wrap-lines (name node-w)
  "Wrap NAME to fit within NODE-W pixels. Returns list of lines."
  (let* ((L +dispatch--layout)
         (char-w (plist-get (+dispatch--theme-colors) :char-w))
         (text-start (+ (plist-get L :node-pad-x) (plist-get L :node-icon-w)))
         (avail-chars (max 10 (/ (- node-w text-start (plist-get L :node-pad-x))
                                 (max 1 char-w)))))
    (+dispatch--wrap-text name avail-chars)))

(defun +dispatch--task-node-height (task node-w)
  "Compute the height needed for TASK given NODE-W."
  (let* ((L +dispatch--layout)
         (lines (+dispatch--node-wrap-lines (plist-get task :name) node-w))
         (base-h (plist-get L :node-h)))
    (if (> (length lines) 1)
        (+ base-h (* (1- (length lines)) (+ (plist-get L :node-font-size) 2)))
      base-h)))

(defun +dispatch--draw-task-node (svg x y w h task theme)
  "Draw a task node on SVG. H is the pre-computed height. Returns edge positions."
  (let* ((L +dispatch--layout)
         (sc (cdr (assq (plist-get task :status) (plist-get theme :status))))
         (font (plist-get theme :font))
         (pad (plist-get L :node-pad-x))
         (lines (+dispatch--node-wrap-lines (plist-get task :name) w))
         (font-size (plist-get L :node-font-size))
         (line-h (+ font-size 2))
         (text-x (+ x pad (plist-get L :node-icon-w)))
         (text-y (+ y (plist-get L :node-text-y)))
         (cy (+ y (/ h 2))))
    (svg-rectangle svg x y w h :fill (plist-get sc :bg)
                   :rx (plist-get L :node-rx))
    ;; Icon — vertically centered
    (svg-text svg (plist-get sc :icon) :x (+ x pad) :y (+ cy 4)
              :fill (plist-get sc :fg)
              :font-size font-size :font-family font)
    ;; Text lines
    (cl-loop for line in lines
             for i from 0
             do (svg-text svg line
                          :x text-x :y (+ text-y (* i line-h))
                          :fill (plist-get sc :fg)
                          :font-size font-size :font-family font))
    (let ((rx (plist-get L :node-rx)))
      (list :left-x (+ x rx) :right-x (- (+ x w) rx) :cy cy))))

(defun +dispatch--compute-col-widths (columns max-level &optional stack-map)
  "Compute per-column widths from COLUMNS hash (level → tasks), up to MAX-LEVEL.
When STACK-MAP is non-nil, bottom-of-pair levels share width with their top level."
  (let ((L +dispatch--layout)
        (max-name-len (plist-get +dispatch--layout :name-max-len))
        (char-w (plist-get (+dispatch--theme-colors) :char-w))
        (widths (make-hash-table)))
    (cl-loop for lv from 0 to max-level
             for stack-info = (and stack-map (gethash lv stack-map))
             ;; Skip bottom-of-pair levels — their width is merged into the top
             unless (and stack-info (eq (plist-get stack-info :position) 'bottom))
             do (let* ((levels-to-measure
                        (if (and stack-info (eq (plist-get stack-info :position) 'top))
                            (list lv (plist-get stack-info :peer-level))
                          (list lv)))
                       (max-line-len
                        (cl-loop for mlv in levels-to-measure
                                 for col = (gethash mlv columns)
                                 maximize (cl-loop for t_ in col
                                                   for lines = (+dispatch--wrap-text
                                                                (plist-get t_ :name) max-name-len)
                                                   maximize (cl-loop for line in lines
                                                                     maximize (length line)))))
                       (w (min (+ (* 2 (plist-get L :node-pad-x))
                                  (plist-get L :node-icon-w)
                                  (* char-w max-line-len))
                               (plist-get L :node-max-w))))
                  (puthash lv w widths)
                  ;; Also store for bottom level so task-heights can look it up
                  (when (and stack-info (eq (plist-get stack-info :position) 'top))
                    (puthash (plist-get stack-info :peer-level) w widths))))
    widths))

(defun +dispatch--compute-col-x-positions (max-level col-widths &optional stack-map)
  "Compute cumulative x-positions for each column level.
When STACK-MAP is non-nil, bottom-of-pair levels share their top level's x-position."
  (let ((positions (make-hash-table))
        (x (+ (plist-get +dispatch--layout :margin)
              (plist-get +dispatch--layout :pill-w)
              (plist-get +dispatch--layout :col-gap))))
    (cl-loop for lv from 0 to max-level
             for stack-info = (and stack-map (gethash lv stack-map))
             do (if (and stack-info (eq (plist-get stack-info :position) 'bottom))
                    ;; Bottom shares top's x-position
                    (puthash lv (gethash (plist-get stack-info :peer-level) positions) positions)
                  (puthash lv x positions)
                  (cl-incf x (+ (gethash lv col-widths)
                                (plist-get +dispatch--layout :col-gap)))))
    positions))

(defun +dispatch--compute-task-heights (leveled col-widths)
  "Compute per-task heights based on text wrapping within column widths."
  (let ((heights (make-hash-table :test 'equal)))
    (dolist (task leveled)
      (puthash (plist-get task :id)
               (+dispatch--task-node-height
                task (gethash (plist-get task :level) col-widths))
               heights))
    heights))

(defun +dispatch--col-height (col task-heights node-pad)
  "Compute total height of column COL given TASK-HEIGHTS and NODE-PAD."
  (+ (cl-loop for t_ in col sum (gethash (plist-get t_ :id) task-heights))
     (* (1- (max (length col) 1)) node-pad)))

(defun +dispatch--compute-bypass-y (from-lv to-lv from-cy col-bounds h)
  "Compute bypass Y for an arrow spanning FROM-LV to TO-LV, or nil if not needed."
  (let ((span (- to-lv from-lv)))
    (when (> span 1)
      (let ((min-top h) (max-bot 0) (has-intermediate nil))
        (cl-loop for lv from (1+ from-lv) below to-lv
                 for bounds = (gethash lv col-bounds)
                 when bounds
                   do (setq has-intermediate t
                            min-top (min min-top (plist-get bounds :top))
                            max-bot (max max-bot (plist-get bounds :bot))))
        (when has-intermediate
          (let ((pad (+ (plist-get +dispatch--layout :node-pad) 6)))
            (if (< from-cy (/ h 2))
                (- min-top pad)
              (+ max-bot pad))))))))

(defun +dispatch--compute-col-bounds (node-edges leveled task-heights node-h)
  "Compute top/bottom bounds per level from NODE-EDGES."
  (let ((col-bounds (make-hash-table)))
    (maphash (lambda (id edges)
               (unless (member id '("start" "end"))
                 (when-let* ((task (cl-find-if (lambda (t_) (equal (plist-get t_ :id) id)) leveled))
                             (lv (plist-get task :level))
                             (cy (plist-get edges :cy))
                             (th (or (gethash id task-heights) node-h)))
                   (let* ((top (- cy (/ th 2)))
                          (bot (+ cy (/ th 2)))
                          (cur (gethash lv col-bounds)))
                     (puthash lv (list :top (if cur (min (plist-get cur :top) top) top)
                                       :bot (if cur (max (plist-get cur :bot) bot) bot))
                              col-bounds)))))
             node-edges)
    col-bounds))

(defun +dispatch--draw-edges (svg edges node-edges leveled col-bounds h theme)
  "Draw arrows for EDGES with bypass routing around intermediate columns."
  (let ((id-to-level (make-hash-table :test 'equal)))
    (puthash "start" -1 id-to-level)
    (puthash "end" (1+ (cl-loop for t_ in leveled maximize (plist-get t_ :level))) id-to-level)
    (dolist (task leveled)
      (puthash (plist-get task :id) (plist-get task :level) id-to-level))
    (dolist (edge edges)
      (when-let* ((from (gethash (car edge) node-edges))
                  (to (gethash (cdr edge) node-edges))
                  (from-lv (gethash (car edge) id-to-level))
                  (to-lv (gethash (cdr edge) id-to-level)))
        (+dispatch--draw-arrow svg
                               (plist-get from :right-x) (plist-get from :cy)
                               (plist-get to :left-x) (plist-get to :cy)
                               (plist-get theme :arrow)
                               (+dispatch--compute-bypass-y
                                from-lv to-lv (plist-get from :cy) col-bounds h))))))

(defun +dispatch--build-svg (tasks-info)
  "Build a horizontal dependency graph SVG from TASKS-INFO."
  (let* ((L +dispatch--layout)
         (theme (+dispatch--theme-colors))
         (node-pad (plist-get L :node-pad))
         (col-gap (plist-get L :col-gap))
         (margin (plist-get L :margin))
         (pill-w (plist-get L :pill-w))
         (pill-h (plist-get L :pill-h))
         (leveled (if (plist-get (car tasks-info) :level) tasks-info
                    (+dispatch--compute-levels tasks-info)))
         (max-level (cl-loop for t_ in leveled maximize (plist-get t_ :level)))
         (columns (+dispatch--group-by-level leveled))
         (col-widths (+dispatch--compute-col-widths columns max-level))
         (col-xs (+dispatch--compute-col-x-positions max-level col-widths))
         (task-heights (+dispatch--compute-task-heights leveled col-widths))
         (max-col-h (cl-loop for lv from 0 to max-level
                             maximize (+dispatch--col-height
                                       (gethash lv columns) task-heights node-pad)))
         (w (+ (gethash max-level col-xs) (gethash max-level col-widths)
               col-gap pill-w margin))
         (h (max (+ (* 2 margin) max-col-h) 60))
         (svg (svg-create w h))
         (node-edges (make-hash-table :test 'equal)))

    ;; Background
    (svg-rectangle svg 0 0 w h :fill (plist-get theme :bg) :rx (plist-get L :bg-rx))

    ;; Start pill
    (puthash "start"
             (+dispatch--draw-pill svg margin (/ h 2) pill-w pill-h "▶" theme)
             node-edges)

    ;; Task nodes by column
    (cl-loop for lv from 0 to max-level
             for col-tasks = (gethash lv columns)
             for col-x = (gethash lv col-xs)
             for cw = (gethash lv col-widths)
             for col-h = (+dispatch--col-height col-tasks task-heights node-pad)
             for cur-y = (/ (- h col-h) 2)
             do (cl-loop for task in col-tasks
                         for th = (gethash (plist-get task :id) task-heights)
                         do (puthash (plist-get task :id)
                                     (+dispatch--draw-task-node svg col-x cur-y cw th task theme)
                                     node-edges)
                            (cl-incf cur-y (+ th node-pad))))

    ;; End pill
    (puthash "end"
             (+dispatch--draw-pill svg
                                   (+ (gethash max-level col-xs)
                                      (gethash max-level col-widths) col-gap)
                                   (/ h 2) pill-w pill-h "■" theme)
             node-edges)

    ;; Arrows with bypass routing
    (+dispatch--draw-edges svg
                           (+dispatch--transitive-reduce leveled)
                           node-edges leveled
                           (+dispatch--compute-col-bounds
                            node-edges leveled task-heights (plist-get L :node-h))
                           h theme)
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


(defun +dispatch--apply-viewport (svg-str render-data dispatcher-buf)
  "Apply viewBox panning to SVG-STR if the graph is wider than the window.
Centers on the first non-done task, showing one completed level to its left."
  (when-let* ((dims (+dispatch--svg-dimensions svg-str))
              (svg-w (car dims))
              (svg-h (cdr dims))
              (win (get-buffer-window dispatcher-buf))
              (win-pw (window-body-width win t))
              ((> svg-w win-pw)))
    (let* ((leveled (if (plist-get (car render-data) :level) render-data
                      (+dispatch--compute-levels render-data)))
           (max-level (cl-loop for t_ in leveled maximize (plist-get t_ :level)))
           (columns (+dispatch--group-by-level leveled))
           (col-widths (+dispatch--compute-col-widths columns max-level))
           (col-xs (+dispatch--compute-col-x-positions max-level col-widths))
           (target-level (or (cl-loop for task in leveled
                                      unless (eq (plist-get task :status) 'done)
                                      minimize (plist-get task :level))
                             0))
           (show-level (max 0 (1- target-level)))
           (view-x (max 0 (- (gethash show-level col-xs)
                              (plist-get +dispatch--layout :margin)))))
      (setq svg-str (replace-regexp-in-string
                     (format "width=\"%d\" height=\"%d\"" svg-w svg-h)
                     (format "width=\"%d\" height=\"%d\" viewBox=\"%d 0 %d %d\""
                             win-pw svg-h view-x win-pw svg-h)
                     svg-str t t))))
  svg-str)

(defun +dispatch--svg-dimensions (svg-str)
  "Extract (WIDTH . HEIGHT) from SVG-STR, or nil."
  (when (string-match "width=\"\\([0-9]+\\)\"" svg-str)
    (let ((w (string-to-number (match-string 1 svg-str))))
      (when (string-match "height=\"\\([0-9]+\\)\"" svg-str)
        (cons w (string-to-number (match-string 1 svg-str)))))))

(defun +dispatch--strip-svg-wrapper (svg-str)
  "Remove the outer <svg>...</svg> tags from SVG-STR."
  (replace-regexp-in-string
   "\\`<svg[^>]*>" ""
   (replace-regexp-in-string "</svg>\\'" "" svg-str)))

(defun +dispatch--combine-svgs (top-svg bottom-svg gap-above gap-below)
  "Stack TOP-SVG and BOTTOM-SVG vertically with GAP-ABOVE and GAP-BELOW."
  (when-let* ((top-dims (+dispatch--svg-dimensions top-svg))
              (bot-dims (+dispatch--svg-dimensions bottom-svg)))
    (let* ((w (max (car top-dims) (car bot-dims)))
           (h (+ (cdr top-dims) gap-above (cdr bot-dims) gap-below)))
      (format "<svg width=\"%d\" height=\"%d\" version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\">
<svg y=\"0\">%s</svg>
<svg y=\"%d\">%s</svg>
</svg>"
              w h
              (+dispatch--strip-svg-wrapper top-svg)
              (+ (cdr top-dims) gap-above)
              (+dispatch--strip-svg-wrapper bottom-svg)))))

(defun +dispatch--cycle-spinner ()
  "Advance the spinner frame and update the working icon."
  (cl-incf +dispatch--spinner-index)
  (plist-put (cdr (assq 'working (plist-get (+dispatch--theme-colors) :status)))
             :icon (nth (% +dispatch--spinner-index (length +dispatch--spinner-frames))
                        +dispatch--spinner-frames)))

(defun +dispatch--build-render-data (tasks statuses)
  "Resolve task statuses into render-ready plists."
  (cl-loop for task in tasks
           for (effective elapsed detail _started)
           = (+dispatch--resolve-status task statuses)
           collect (list :id (plist-get task :id)
                         :name (plist-get task :name)
                         :status effective
                         :elapsed elapsed
                         :detail detail
                         :depends-on (plist-get task :depends-on))))

(defun +dispatch--extend-header (&rest _)
  "Build task graph SVG and append it below agent-shell's header SVG.
Called as :after advice on `agent-shell--update-header-and-mode-line'."
  (when-let* ((state +meta-agent-shell--dispatch-state)
              (tasks (plist-get state :tasks))
              (statuses (plist-get state :statuses))
              ((stringp header-line-format))
              (disp (get-text-property 1 'display header-line-format))
              (orig-svg (plist-get (cdr disp) :data)))
    (+dispatch--cycle-spinner)
    (let* ((render-data (+dispatch--build-render-data tasks statuses))
           (svg (+dispatch--build-svg render-data))
           (graph-svg (with-temp-buffer (svg-print svg) (buffer-string)))
           (graph-svg (+dispatch--apply-viewport graph-svg render-data (buffer-name)))
           (combined (+dispatch--combine-svgs orig-svg graph-svg -12 10)))
      (when combined
        (setq header-line-format
              (format " %s" (propertize " " 'display
                                        (list 'image :type 'svg
                                              :data combined :scale 'default))))))))

(defun +dispatch-start (dispatcher-buffer tasks &optional _interval)
  "Start the dispatch task graph in the agent-shell header.
DISPATCHER-BUFFER is the dispatcher's agent-shell buffer name.
TASKS is a list of plists: ((:id ID :name NAME :agent AGENT-BUF) ...)."
  (+dispatch-stop)
  (setq +meta-agent-shell--pending-permission-agents nil)
  ;; Normalize :agent — default to dispatcher buffer if missing or not a string
  (let ((normalized (mapcar (lambda (task)
                              (let ((agent (plist-get task :agent)))
                                (if (stringp agent) task
                                  (plist-put (copy-sequence task) :agent dispatcher-buffer))))
                            tasks)))
    (setq +meta-agent-shell--dispatch-state
          (list :dispatcher-buffer dispatcher-buffer
                :tasks normalized
                :statuses (make-hash-table :test 'equal)))
    ;; Install header advice — graph rebuilds on every header update
    (advice-add 'agent-shell--update-header-and-mode-line
                :after #'+dispatch--extend-header)))

(defun +dispatch-stop ()
  "Remove the dispatch task graph from the header."
  (let ((dispatcher (and +meta-agent-shell--dispatch-state
                         (plist-get +meta-agent-shell--dispatch-state :dispatcher-buffer))))
    (setq +meta-agent-shell--dispatch-state nil)
    ;; Remove header advice and force header rebuild
    (advice-remove 'agent-shell--update-header-and-mode-line #'+dispatch--extend-header)
    (when-let* ((buf (and dispatcher (get-buffer dispatcher))))
      (with-current-buffer buf
        (when (boundp 'agent-shell--header-cache)
          (setq agent-shell--header-cache nil))
        (ignore-errors (agent-shell--update-header-and-mode-line))))))

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
