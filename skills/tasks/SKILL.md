---
name: tasks
description: 'Render a visual SVG task graph for your work. Break your plan into steps with dependencies, track progress with a live dependency graph.'
---

# Task Graph for Structured Work

When you have a multi-step task, render a live SVG dependency graph that tracks your progress. The graph appears in a dedicated window below the agent-shell buffer, showing Start/End nodes, task boxes with status colors, and curved arrows for dependencies.

## When to Use

- You have 3+ steps to complete
- The user asks you to plan and execute a multi-step task
- You want to show clear progress through a workflow

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
