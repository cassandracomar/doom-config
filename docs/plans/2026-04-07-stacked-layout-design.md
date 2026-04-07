# Stacked Layout for SVG Task Graph

## Problem

Long horizontal chains of single-node levels waste horizontal space. A 6-task linear chain produces 6 columns plus Start/End pills, often overflowing the window.

## Solution

When two adjacent levels each contain exactly one node, stack them vertically in a single column. This halves the horizontal footprint of linear chains.

## Rules

- **Threshold**: Only activate stacking when there are 5+ task columns (not counting Start/End pills). Below that, horizontal layout is fine.
- **Pairing**: Scan for runs of consecutive single-node levels. Group into pairs, left-first. Odd remainder stays as a standalone column.
- **Multi-node columns are never stacked**: If a level has 2+ nodes, it and its neighbors break any run.

## Layout Constants

Add to `+dispatch--layout`:
- `:stack-vgap 30` — vertical gap between stacked pair nodes (separate from `:node-pad`)

## New Function: `+dispatch--compute-stacks`

**Input**: leveled tasks, column grouping (level -> task list)
**Output**: stack-map (hash: level -> `(:peer-level N :position top|bottom)`), set of consumed levels

Example — 6 single-node levels (0-5):
- Levels 0,1 -> stacked pair (0=top, 1=bottom)
- Levels 2,3 -> stacked pair
- Levels 4,5 -> stacked pair

Example — 5 single-node levels (0-4):
- Levels 0,1 -> stacked pair
- Levels 2,3 -> stacked pair
- Level 4 -> standalone

## Column Layout Changes

In `+dispatch--build-svg` and column helpers:

- **Skip consumed levels**: Bottom-of-pair levels don't get their own column x-position.
- **Stacked column width**: `max(top-node-width, bottom-node-width)`.
- **Stacked column height**: `top-h + stack-vgap + bottom-h`.
- **Node placement**: Top node at `cur-y`, bottom at `cur-y + top-h + stack-vgap`.

## Arrow Types

| Arrow | From | To | Function |
|-------|------|----|----------|
| Horizontal ingress | Previous node's right edge | Top node's left edge | `+dispatch--draw-arrow` (existing) |
| Internal pair | Top node's right edge | Bottom node's top-center | `+dispatch--draw-stack-arrow` (new) |
| Horizontal egress | Bottom node's right edge | Next node's left edge | `+dispatch--draw-arrow` (existing) |

### Internal Pair Arrow (`+dispatch--draw-stack-arrow`)

Cubic bezier from top-node right-center to bottom-node top-center:
- Start: `(right-x, cy-top)`
- CP1: `(right-x + 40, bot-top-y)` — exits horizontally right, curves down
- CP2: `(mid-x, bot-top-y - 20)` — approaches from above
- End: `(mid-x, bot-top-y)`
- Arrowhead: downward-pointing triangle at top-center of bottom node

### Edge Detection

`+dispatch--transitive-reduce` already produces `(top-id . bottom-id)` edges. The draw step checks the stack-map: if source is top-of-pair and target is bottom-of-pair in the same column, route through `+dispatch--draw-stack-arrow`.

## Bypass Routing

No changes needed — `+dispatch--compute-bypass-y` uses actual node positions from `node-edges`, which will reflect stacked positions automatically.

## Viewport Panning

No changes — left as-is. Stacking alone should reduce width enough.

## Not In Scope

- Stacking 3+ nodes (limited to pairs)
- Stacking multi-node columns
- Viewport panning updates
