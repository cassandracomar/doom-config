---
name: tasks
description: 'ALWAYS use when planning multi-step work. Replaces TodoWrite for visual progress tracking. Use whenever you would create a task list, break work into steps, plan an approach, or outline sequential/parallel steps. Renders a live SVG dependency graph in the agent-shell header showing task status, elapsed time, and dependencies.'
---

# Task Graph for Structured Work

Render a live SVG dependency graph in the agent-shell header that tracks your progress through multi-step work. Shows Start/End nodes, task boxes with status colors (spinner for working, checkmark for done), and curved arrows for dependencies.

**This replaces TodoWrite for visual progress tracking.** Use this instead of TodoWrite whenever you're planning and executing steps.

## When to Use

- You are about to break work into steps or create a task list
- You have 3+ steps to complete
- The user asks you to plan and execute a multi-step task
- You would otherwise use TodoWrite to track progress
- You want to show clear, visual progress through a workflow

## Step 1: Plan Your Tasks

Break the work into discrete steps. Identify dependencies between them — which steps must complete before others can start. Assign each an ID like `step-1`, `step-2`, etc.

## Step 2: Start the Task Graph

Call via Emacs MCP. Each task is a plist with `:id`, `:name`, `:agent` (your buffer name), and optionally `:depends-on` (list of task IDs that must complete first):

```
mcp__emacs__claude-code-ide-extras-emacs_eval_elisp:
(+dispatch-start
 (buffer-name)
 '((:id "step-1" :name "Read codebase")
   (:id "step-2" :name "Write tests" :depends-on ("step-1"))
   (:id "step-3" :name "Implement feature" :depends-on ("step-1"))
   (:id "step-4" :name "Run tests" :depends-on ("step-2" "step-3"))
   (:id "step-5" :name "Commit" :depends-on ("step-4"))))
```

The `:agent` field is optional for single-agent work — it defaults to the dispatcher buffer. Tasks with no `:depends-on` depend on Start. Tasks nothing depends on connect to End. The graph renders horizontally: Start → tasks by dependency level → End.

## Step 3: Report Progress

As you begin each step:
```
mcp__emacs__claude-code-ide-extras-emacs_eval_elisp:
(+dispatch-report "step-1" "working" "brief description of current activity")
```

When you finish a step:
```
mcp__emacs__claude-code-ide-extras-emacs_eval_elisp:
(+dispatch-report "step-1" "done")
```

If a step fails:
```
mcp__emacs__claude-code-ide-extras-emacs_eval_elisp:
(+dispatch-report "step-2" "error" "what went wrong")
```

## Step 4: Clean Up

When all work is complete:
```
mcp__emacs__claude-code-ide-extras-emacs_eval_elisp:
(+dispatch-stop)
```

## Rules

- Create the graph BEFORE starting work
- Report "working" when you START a step, "done" when you FINISH it
- Keep step names short (they render in SVG boxes, long names wrap)
- Express real dependencies — if step 3 needs step 1's output, say so
- Independent steps should NOT depend on each other (they render in parallel columns)
- Always call `+dispatch-stop` when finished
