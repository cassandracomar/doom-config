# Stacked Layout Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use hex:executing-plans to implement this plan task-by-task.

**Goal:** Compress long horizontal task chains by stacking consecutive single-node levels into vertical pairs.

**Architecture:** Post-processing pass after level computation identifies stackable pairs. Column layout, node drawing, and edge drawing all consult a stack-map to handle stacked columns. Single file change.

**Tech Stack:** Emacs Lisp, svg.el, dom.el

---

### Task 1: Add `:stack-vgap` layout constant and `+dispatch--compute-stacks`

**Files:**
- Modify: `agent/+agent-dispatcher.el:124-133` (layout constant)
- Modify: `agent/+agent-dispatcher.el:304-312` (after `+dispatch--group-by-level`)

**Step 1: Add `:stack-vgap 30` to `+dispatch--layout`**

```elisp
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
```

**Step 2: Write `+dispatch--compute-stacks`**

Insert after `+dispatch--group-by-level`. This scans for runs of consecutive single-node levels and pairs them left-first.

```elisp
(defun +dispatch--compute-stacks (columns max-level)
  "Identify levels to stack vertically as pairs.
Only activates when there are more than :stack-threshold task columns.
Returns a hash of level -> (:peer-level N :position top|bottom),
or nil if stacking is not needed."
  (when (> (1+ max-level) (plist-get +dispatch--layout :stack-threshold))
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
```

**Step 3: Verify in Emacs via MCP**

Eval the function definition, then test with sample data:

```elisp
(let* ((tasks '((:id "t1" :name "A" :level 0)
                (:id "t2" :name "B" :level 1)
                (:id "t3" :name "C" :level 2)
                (:id "t4" :name "D" :level 3)
                (:id "t5" :name "E" :level 4)))
       (columns (+dispatch--group-by-level tasks))
       (stacks (+dispatch--compute-stacks columns 4)))
  (list :stacks-count (hash-table-count stacks)
        :lv0 (gethash 0 stacks)
        :lv1 (gethash 1 stacks)
        :lv4 (gethash 4 stacks)))
```

Expected: levels 0,1 paired (top/bottom), levels 2,3 paired, level 4 standalone (nil).

**Step 4: Commit**

```bash
git add agent/+agent-dispatcher.el
git commit -m "add +dispatch--compute-stacks and :stack-vgap layout constant"
```

---

### Task 2: Write `+dispatch--draw-stack-arrow`

**Files:**
- Modify: `agent/+agent-dispatcher.el:267-302` (after `+dispatch--draw-arrow`)

**Step 1: Write the stack arrow function**

Insert after `+dispatch--draw-arrow`:

```elisp
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
```

**Step 2: Verify visually via MCP**

Create a minimal SVG with one stack arrow and render to a temp file to confirm the curve shape.

**Step 3: Commit**

```bash
git add agent/+agent-dispatcher.el
git commit -m "add +dispatch--draw-stack-arrow for internal pair arrows"
```

---

### Task 3: Update column layout functions for stacking

**Files:**
- Modify: `agent/+agent-dispatcher.el:389-434` (`+dispatch--compute-col-widths`, `+dispatch--compute-col-x-positions`, `+dispatch--compute-task-heights`, `+dispatch--col-height`)

**Step 1: Update `+dispatch--compute-col-widths` to merge stacked pair widths**

The stacked column width should be `max(top-node-width, bottom-node-width)`. When a level is the bottom of a pair, it shares the top level's column — compute its width contribution into the top level's entry.

```elisp
(defun +dispatch--compute-col-widths (columns max-level &optional stack-map)
  "Compute per-column widths from COLUMNS hash (level -> tasks), up to MAX-LEVEL.
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
```

**Step 2: Update `+dispatch--compute-col-x-positions` to skip consumed levels**

```elisp
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
```

**Step 3: Verify column positions via MCP**

Test that a 6-level linear chain with stacking produces 3 x-positions instead of 6.

**Step 4: Commit**

```bash
git add agent/+agent-dispatcher.el
git commit -m "update column layout functions to support stacked pairs"
```

---

### Task 4: Update `+dispatch--build-svg` to draw stacked columns

**Files:**
- Modify: `agent/+agent-dispatcher.el:490-552` (`+dispatch--build-svg`)

**Step 1: Compute stack-map and thread it through layout**

Add stack-map computation after `columns` is built, pass it to col-widths and col-xs.

**Step 2: Update the task node drawing loop**

The main column loop currently iterates `for lv from 0 to max-level`. Update it to:
- Skip bottom-of-pair levels (they're drawn by their top level)
- When a level is top-of-pair, draw both nodes stacked and the internal arrow
- Register edge positions for both top and bottom nodes in `node-edges`

For stacked pairs, the top node gets normal edge positions (`:left-x`, `:right-x`, `:cy`). The bottom node also gets edge positions, but its `:left-x` is set to its top-center x (since arrows enter from above via the stack arrow, not from the left).

Actually — external horizontal arrows still use `:left-x` and `:right-x`. For stacked pairs:
- **Top node**: ingress uses `:left-x`, `:cy` (normal)
- **Bottom node**: egress uses `:right-x`, `:cy` (normal)
- The internal stack arrow uses `+dispatch--draw-stack-arrow`

The bottom node's `:left-x` won't be used by external arrows (since the top node receives ingress), so it's fine to leave it as-is.

```elisp
;; In +dispatch--build-svg, replace the task node drawing loop:

;; Task nodes by column (with stacking support)
(cl-loop for lv from 0 to max-level
         for stack-info = (and stack-map (gethash lv stack-map))
         ;; Skip bottom-of-pair levels — drawn by their top
         unless (and stack-info (eq (plist-get stack-info :position) 'bottom))
         do (let* ((col-tasks (gethash lv columns))
                   (col-x (gethash lv col-xs))
                   (cw (gethash lv col-widths)))
              (if (and stack-info (eq (plist-get stack-info :position) 'top))
                  ;; Stacked pair: draw top + bottom + internal arrow
                  (let* ((bot-lv (plist-get stack-info :peer-level))
                         (top-task (car col-tasks))
                         (bot-task (car (gethash bot-lv columns)))
                         (top-h (gethash (plist-get top-task :id) task-heights))
                         (bot-h (gethash (plist-get bot-task :id) task-heights))
                         (stack-vgap (plist-get L :stack-vgap))
                         (pair-h (+ top-h stack-vgap bot-h))
                         (top-y (/ (- h pair-h) 2))
                         (bot-y (+ top-y top-h stack-vgap))
                         (top-edges (+dispatch--draw-task-node
                                     svg col-x top-y cw top-h top-task theme))
                         (bot-edges (+dispatch--draw-task-node
                                     svg col-x bot-y cw bot-h bot-task theme)))
                    (puthash (plist-get top-task :id) top-edges node-edges)
                    (puthash (plist-get bot-task :id) bot-edges node-edges)
                    ;; Internal stack arrow
                    (+dispatch--draw-stack-arrow
                     svg top-edges col-x bot-y cw (plist-get theme :arrow)))
                ;; Normal column (unstacked)
                (let* ((col-h (+dispatch--col-height col-tasks task-heights node-pad))
                       (cur-y (/ (- h col-h) 2)))
                  (cl-loop for task in col-tasks
                           for th = (gethash (plist-get task :id) task-heights)
                           do (puthash (plist-get task :id)
                                       (+dispatch--draw-task-node
                                        svg col-x cur-y cw th task theme)
                                       node-edges)
                              (cl-incf cur-y (+ th node-pad)))))))
```

**Step 3: Update `max-col-h` computation to account for stacked pair heights**

```elisp
(max-col-h (cl-loop for lv from 0 to max-level
                    for stack-info = (and stack-map (gethash lv stack-map))
                    unless (and stack-info (eq (plist-get stack-info :position) 'bottom))
                    maximize (if (and stack-info (eq (plist-get stack-info :position) 'top))
                                 (let* ((bot-lv (plist-get stack-info :peer-level))
                                        (top-task (car (gethash lv columns)))
                                        (bot-task (car (gethash bot-lv columns)))
                                        (top-h (gethash (plist-get top-task :id) task-heights))
                                        (bot-h (gethash (plist-get bot-task :id) task-heights)))
                                   (+ top-h (plist-get L :stack-vgap) bot-h))
                               (+dispatch--col-height
                                (gethash lv columns) task-heights node-pad))))
```

**Step 4: Update end pill x-position**

The end pill x-position uses `(gethash max-level col-xs)`, but if max-level is a consumed (bottom) level, its x-position is already set to the top level's. However the width computation `(gethash max-level col-widths)` might fail if the bottom level wasn't stored. Since we now store widths for both top and bottom levels, this should work. But we need to find the last *visual* column's right edge. Simplest: find the max x + width across all levels.

```elisp
;; Replace the end pill x calculation with:
(last-col-right (cl-loop for lv from 0 to max-level
                         for si = (and stack-map (gethash lv stack-map))
                         unless (and si (eq (plist-get si :position) 'bottom))
                         maximize (+ (gethash lv col-xs) (gethash lv col-widths))))
```

Then end pill x = `(+ last-col-right col-gap)` and total width = `(+ last-col-right col-gap pill-w margin)`.

**Step 5: Update `+dispatch--draw-edges` to skip internal pair edges**

In `+dispatch--draw-edges`, edges between stacked pairs are already drawn by `+dispatch--draw-stack-arrow`. Skip them:

```elisp
(defun +dispatch--draw-edges (svg edges node-edges leveled col-bounds h theme &optional stack-map)
  "Draw arrows for EDGES with bypass routing around intermediate columns.
When STACK-MAP is non-nil, skip edges between stacked pair members."
  (let ((id-to-level (make-hash-table :test 'equal))
        (id-to-stack (when stack-map
                       (let ((m (make-hash-table :test 'equal)))
                         (dolist (task leveled)
                           (when-let* ((si (gethash (plist-get task :level) stack-map)))
                             (puthash (plist-get task :id) si m)))
                         m))))
    (puthash "start" -1 id-to-level)
    (puthash "end" (1+ (cl-loop for t_ in leveled maximize (plist-get t_ :level))) id-to-level)
    (dolist (task leveled)
      (puthash (plist-get task :id) (plist-get task :level) id-to-level))
    (dolist (edge edges)
      ;; Skip edges between stacked pair members
      (unless (and id-to-stack
                   (when-let* ((from-si (gethash (car edge) id-to-stack))
                               (to-si (gethash (cdr edge) id-to-stack)))
                     (and (eq (plist-get from-si :position) 'top)
                          (eq (plist-get to-si :position) 'bottom)
                          (= (plist-get from-si :peer-level)
                             (gethash (cdr edge) id-to-level)))))
        (when-let* ((from (gethash (car edge) node-edges))
                    (to (gethash (cdr edge) node-edges))
                    (from-lv (gethash (car edge) id-to-level))
                    (to-lv (gethash (cdr edge) id-to-level)))
          (+dispatch--draw-arrow svg
                                 (plist-get from :right-x) (plist-get from :cy)
                                 (plist-get to :left-x) (plist-get to :cy)
                                 (plist-get theme :arrow)
                                 (+dispatch--compute-bypass-y
                                  from-lv to-lv (plist-get from :cy) col-bounds h)))))))
```

**Step 6: Verify visually via MCP**

Start a 6-task linear chain dispatch and confirm the graph renders with 3 stacked columns.

**Step 7: Commit**

```bash
git add agent/+agent-dispatcher.el
git commit -m "integrate stacking into build-svg and edge drawing"
```

---

### Task 5: Test edge cases and mixed layouts

**Files:**
- No file changes — testing only

**Step 1: Test 4-task chain (below threshold)**

```elisp
(+dispatch-start (buffer-name)
 '((:id "t1" :name "A")
   (:id "t2" :name "B" :depends-on ("t1"))
   (:id "t3" :name "C" :depends-on ("t2"))
   (:id "t4" :name "D" :depends-on ("t3"))))
```

Expected: 4 columns, NO stacking (below threshold of 5).

**Step 2: Test 5-task chain (at threshold)**

```elisp
(+dispatch-start (buffer-name)
 '((:id "t1" :name "A")
   (:id "t2" :name "B" :depends-on ("t1"))
   (:id "t3" :name "C" :depends-on ("t2"))
   (:id "t4" :name "D" :depends-on ("t3"))
   (:id "t5" :name "E" :depends-on ("t4"))))
```

Expected: Stacked: [A,B] [C,D] E standalone. 3 visual columns.

**Step 3: Test mixed layout (parallel + chain)**

```elisp
(+dispatch-start (buffer-name)
 '((:id "t1" :name "Read code")
   (:id "t2" :name "Write tests" :depends-on ("t1"))
   (:id "t3" :name "Write impl" :depends-on ("t1"))
   (:id "t4" :name "Run tests" :depends-on ("t2" "t3"))
   (:id "t5" :name "Fix failures" :depends-on ("t4"))
   (:id "t6" :name "Commit" :depends-on ("t5"))))
```

Expected: Level 0 has 1 node, level 1 has 2 nodes (breaks run), levels 2,3,4 are single-node. Stacking depends on which consecutive runs of single-node levels exist. Level 0 is alone (next level has 2 nodes). Levels 2,3 can pair, level 4 is standalone.

**Step 4: Test status updates render correctly in stacked nodes**

Start a 6-task chain, report some as done, some working — confirm status colors and spinner appear correctly in both top and bottom positions.

**Step 5: Clean up test dispatches**

```elisp
(+dispatch-stop)
```

**Step 6: Commit design doc update**

```bash
git add docs/plans/2026-04-07-stacked-layout-design.md
git commit -m "update design doc with implementation plan"
```
