---
name: tasks
description: 'Render a visual SVG task graph for sequential work. Break your plan into steps, track progress with a live dependency graph.'
---

# Task Graph for Sequential Work

When you have a multi-step task, render a live SVG dependency graph that tracks your progress. The graph appears in a dedicated window below the agent-shell buffer.

## When to Use

- You have 3+ sequential steps to complete
- The user asks you to plan and execute a multi-step task
- You want to show clear progress through a workflow

## Step 1: Plan Your Tasks

Break the work into discrete steps. Each step should be a clear unit of work.

## Step 2: Start the Task Graph

Call via Emacs MCP:

```
mcp__emacs__claude-code-ide-extras-emacs_eval_elisp:
(+dispatch-start-linear
 (buffer-name)
 '("Step 1 description" "Step 2 description" "Step 3 description"))
```

Task IDs are auto-generated as `step-1`, `step-2`, etc.

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

## Example

For a task like "add a new feature with tests":

```
mcp__emacs__claude-code-ide-extras-emacs_eval_elisp:
(+dispatch-start-linear
 (buffer-name)
 '("Read existing code" "Write tests" "Implement feature" "Run tests" "Commit"))
```

Then as you work:
```
(+dispatch-report "step-1" "working" "reading src/main.el")
(+dispatch-report "step-1" "done")
(+dispatch-report "step-2" "working" "writing test cases")
...
(+dispatch-report "step-5" "done")
(+dispatch-stop)
```

## Rules

- Create the graph BEFORE starting work
- Report "working" when you START a step, "done" when you FINISH it
- Keep step names short (they render in SVG boxes)
- Always call `+dispatch-stop` when finished
- The graph renders Start → step1 → step2 → ... → End as a horizontal chain
