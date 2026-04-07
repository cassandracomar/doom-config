---
name: dispatch
description: 'Execute a plan with parallel agents. You become the dispatcher: spawn impl/review agents, coordinate work, and track progress with a live UI fragment.'
---

# Dispatch Plan Execution

You (the primary agent) become the dispatcher. Use the Emacs MCP to call meta-agent-shell elisp functions for spawning agents, sending messages, and checking status.

## When to Use

- User has a plan (from hex:writing-plans or discussed in conversation)
- Work can be split across 2-3 independent implementation agents
- User wants parallel execution with review

## Step 1: Gather Context

1. **Plan**: Read the plan file, or summarize from conversation
2. **Task list**: Extract independent tasks with clear boundaries
3. **Project state**: Current branch, recent commits, key files
4. **User parameters** (ask if not specified):
   - Number of impl agents (default: 2, max: 3)
   - Review policy: `per-task`, `batch`, or `none`

## Step 2: Register as Dispatcher and Spawn Agents

First, register your buffer as the dispatcher so permission requests render here:

```
mcp__emacs__claude-code-ide-extras-emacs_eval_elisp:
(setq +meta-agent-shell--primary-buffer (buffer-name))
```

Then spawn agents. They run in the background (no popup, no prompts, acceptEdits mode). Non-edit permissions (bash, etc.) render as button dialogs in YOUR buffer — the user handles them directly. You do NOT handle permissions.

```
mcp__emacs__claude-code-ide-extras-emacs_eval_elisp:
(meta-agent-shell-start-named-agent
 default-directory
 "Impl-1"
 "You are implementation agent 1. Wait for your task assignment."
 t)
```

Repeat for each agent. Then verify:

```
mcp__emacs__claude-code-ide-extras-emacs_eval_elisp:
(meta-agent-shell-list-sessions)
```

Note exact buffer names.

## Step 3: Assign Tasks and Start Progress Polling

Send each agent its task. Include full description — do NOT include reporting instructions (the progress system handles monitoring automatically).

```
mcp__emacs__claude-code-ide-extras-emacs_eval_elisp:
(meta-agent-shell-send-to-session
 "EXACT-BUFFER-NAME"
 "## Your Task: TASK_NAME

TASK_DESCRIPTION

## Instructions
- Work in the project directory
- Implement the task
- Commit with a descriptive message

## Acceptance Criteria
CRITERIA"
 "dispatcher")
```

After sending ALL tasks, create the initial progress fragment (IMPORTANT: must be created during your output, before the prompt appears):

```
mcp__emacs__claude-code-ide-extras-emacs_eval_elisp:
(agent-shell-ui-update-fragment
 (agent-shell-ui-make-fragment-model
  :namespace-id "dispatch-progress"
  :block-id "status"
  :label-left (propertize " Dispatch" 'font-lock-face 'font-lock-keyword-face)
  :label-right (propertize "[starting]" 'font-lock-face 'font-lock-comment-face)
  :body "  ⏳ Starting agents...")
 :expanded t)
```

Then start the progress polling timer. Pass an alist of `(buffer-name . task-description)`:

```
mcp__emacs__claude-code-ide-extras-emacs_eval_elisp:
(+meta-agent-shell-start-progress-polling
 (buffer-name)
 '(("Claude Agent @ doom-config<N>" . "Task 1 description")
   ("Claude Agent @ doom-config<M>" . "Task 2 description")))
```

The timer updates the fragment in-place every 3 seconds. You do NOT need to poll or check statuses.

## Step 4: Wait for User

Tell the user:

> Dispatch running. The progress fragment updates automatically. Tell me when:
> - All tasks show complete (✓) and you want me to gather results
> - An agent needs guidance and you want me to intervene
> - You want to stop (`stop`)

**Do NOT poll statuses yourself.** The elisp timer handles progress updates. Wait for the user to tell you what to do next.

### Permissions:
Non-edit permissions from background agents render as button dialogs in your buffer. The user sees and clicks Accept/Reject directly. You do NOT need to handle, accept, or reject permissions.

### If user says "stop":
```
mcp__emacs__claude-code-ide-extras-emacs_eval_elisp:
(progn
  (+meta-agent-shell-stop-progress-polling)
  (meta-agent-shell-big-red-button))
```

### If user says an agent needs help:
View that agent's output and send it guidance:
```
mcp__emacs__claude-code-ide-extras-emacs_eval_elisp:
(meta-agent-shell-view-session "BUFFER-NAME" 200)
```

## Step 5: Completion

When the user tells you all tasks are complete:

1. **Stop progress polling**:
```
mcp__emacs__claude-code-ide-extras-emacs_eval_elisp:
(+meta-agent-shell-stop-progress-polling)
```

2. **Gather results**:
```
mcp__emacs__claude-code-ide-extras-emacs_eval_elisp:
(meta-agent-shell-view-project-agents default-directory 3000)
```

3. **Report**: completed tasks, commits, issues, next steps.

4. **Clean up agents**:
```
mcp__emacs__claude-code-ide-extras-emacs_eval_elisp:
(meta-agent-shell-big-red-button)
```

## Agent Communication Reference

| Action | Function |
|--------|----------|
| Spawn agent | `(meta-agent-shell-start-named-agent DIR NAME MSG t)` |
| Send message | `(meta-agent-shell-send-to-session BUF MSG FROM)` |
| Ask question | `(meta-agent-shell-ask-session BUF QUESTION FROM)` |
| List agents | `(meta-agent-shell-list-sessions)` |
| View output | `(meta-agent-shell-view-session BUF LINES)` |
| Start polling | `(+meta-agent-shell-start-progress-polling BUF AGENTS)` |
| Stop polling | `(+meta-agent-shell-stop-progress-polling)` |
| Interrupt one | `(meta-agent-shell-interrupt-session BUF)` |
| Stop ALL | `(meta-agent-shell-big-red-button)` |

## Rules

- You ARE the dispatcher. Don't start a separate session.
- ALL agent management via MCP elisp calls.
- Use `meta-agent-shell-list-sessions` to discover buffer names.
- Assign ONE task per agent at a time.
- Permissions are shown directly to the user — do NOT accept or reject on their behalf.
- Do NOT poll or check statuses in a loop — the elisp timer handles progress.
- Wait for the user to tell you when tasks are done or need intervention.
- Never implement tasks yourself — coordinate.
- Always clean up agents and stop polling when dispatch is complete.
