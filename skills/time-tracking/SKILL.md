---
name: time-tracking
description: Reconstruct time spent on a tracked project from work evidence (agent chats, calendar, tickets, PRs), draft time entries, and log them to Harvest after approval. Use whenever the user says "update my <project> hours", "log my time on X", "what have I logged on X", "set up time tracking for a project", or when a scheduled job asks for a time-tracking draft. Also use when correcting or re-describing existing time entries.
---

# Time tracking

Turns scattered evidence of work into honest, readable time entries. Per-project settings live outside this skill, in `~/.config/time-tracking/projects/<project>.md` (private: they hold IDs, hosts and scope rules). If the project has no settings file, run **setup** first (`references/setup.md`).

## Principles

- **The user's time, not the agent's.** Agents may write the code; entries record the user's directing, reviewing, deciding and researching. Use verbs that say so ("Reviewed…", "Decided…", "Explored…").
- **Draft, never log, without a yes.** Every run ends in a draft. Only an explicit approval writes to Harvest. A scheduled run sends the draft and stops.
- **Scope is the project's, not yours.** Follow the settings' include/exclude rules exactly; when work is ambiguous, list it under "Left out — tell me if it counts" instead of guessing.
- **Notes outlive memory.** Each note says in plain words what the work was, with ticket/PR codes in parentheses: "Reviewed a PR check that fails when generated code is out of date (PER-73, PR #222)" — never "Reviewed PER-73". Look up titles before writing.
- **Unknown durations are asked, not invented.** Work with no timing evidence goes in the draft as "How long did this take?".

## Counting rule

Timing comes only from **timing sources** (chats, calendar, user reports). **Labeling sources** (tickets, PRs, commits) say *what* the time was for and never add hours on their own.

For message-based sources:
- A **stretch** runs from the first to the last in-scope message by the user.
- Gaps ≤ `gap_minutes` (default 45) stay inside the stretch — that is usually the user reading a review page or diff.
- A lone message counts `lone_minutes` (default 5).
- No percentage cuts and no subtracting overlap with other work: switching context is effort, not a discount.

`scripts/stretches.py` does this arithmetic; feed it messages, don't recompute by hand.

## Run

1. **Range.** From the day after the latest Harvest entry on the project through yesterday. Include today only when asked; drop future dates. Work in the settings' timezone (convert UTC times from tools like `gh`). If the range overlaps days already logged, recompute those days and compare: show only entries that change by ≥ 0.1 h, as `before → after`.
2. **Gather.** For each source in the settings, read only its recipe in `references/sources/<type>.md`. Pull compact results (timestamps + one-line text), not transcripts. Read a full message only when a one-liner can't tell you whether it's in scope.
3. **Compute** stretches per day; attach labels from tickets/PRs; flag orphan labels (a PR with no stretch near it).
4. **Draft** (format below). Also show existing entries the draft would change, as before → after.
5. **Approve.** Interactive: wait for the user's yes or edits. Scheduled: deliver the draft via the settings' `deliver` channel and stop.
6. **Log** approved entries; then report the new project total against the budget (sum of the project's entries since the budget start). Pick each entry's task by the settings' task guide; when unsure, follow how similar existing entries were filed.

## Draft format

```
<Project> — draft for <range>   (logged so far: X of Y h)

| Date | Task | Hours | Note | Evidence |
|------|------|-------|------|----------|
| Oct 28 | Coding | 1.75 | Reviewed … (PER-72) | chat 4:53–6:39pm |
| Oct 30 | Coding | 1.05 → 1.4 | Reviewed … (PER-74, PR #223) | chat 11:21am–12:46pm |

How long did this take?
- Oct 29: Opened PR #221 — one command to rebuild generated files (PER-72)

Left out: <one line per excluded item and why>
```

Keep it to the table and those two lists. No narration. If nothing would change, say so in one line instead.
