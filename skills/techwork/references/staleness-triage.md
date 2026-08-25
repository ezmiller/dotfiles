# Staleness Triage

Identify tasks that have been carried over repeatedly without progress. Useful both as part
of the morning briefing (light mention) and as a standalone cleanup exercise.

**The three tiers:**

| Tier | Label | Criteria |
|------|-------|----------|
| 🔴 Active | Work on today | STARTED state, OR TODO with activity in last 3 days |
| 🟡 Queued | Carried, lower urgency | TODO, carried over, no recent updates, < 7 days old |
| ⚪ Stale | Needs decision | TODO/STARTED appearing 5+ journal days with no progress |

**Progress signals (task is NOT stale if any are true):**
- LOGBOOK entry with timestamp in last 7 days
- Body text changed between journal files (new bullets, prose, links)
- State change (e.g., TODO → STARTED) in last 7 days
- Mentioned in a standup "Today" or "Yesterday" section in last 3 days
- Has a sub-task that was marked DONE recently

**Efficient staleness detection algorithm:**

Don't read every journal file. Instead:

1. **Start from today's file** — extract all TODO/STARTED task headlines (the `* TODO ...` lines)
2. **Get the last 10 journal filenames** — sorted by date descending
3. **For each task headline, grep across those 10 files:**
   - Count how many files contain the headline (carryover count)
   - Check if any file has a LOGBOOK timestamp within 7 days
   - Check if the body differs between the oldest and newest occurrence
4. **Classify:**
   - Carryover count ≥ 5 AND no recent progress signals → ⚪ Stale
   - Carryover count < 5 OR has progress signals → 🟡 Queued or 🔴 Active

**Grep pattern for a task headline:**
```
^\* (TODO|STARTED|REVIEW|BLOCKED) Task title here
```
Escape special characters in the title. Match is case-sensitive for org keywords.

**Output format for standalone triage:**

```
## ⚪ Stale (7+ days, no progress)
These tasks have been carried over repeatedly. Consider: close, delegate, or re-scope.

- TODO Enhance discount code error messaging (since Jan 5 — 72 days)
- TODO Add INP metric to useReportPageLoadMetrics (since Feb 10 — 36 days)

## 🟡 Queued (carried but recent)
- TODO EPD-2466: Convert collectionQuery to GraphQL
- TODO Full-width design for tablet/mobile in Nav

## 🔴 Active (work today)
- STARTED EPD-2481 Support a pre-header logo in 1-column layout
- STARTED Helpdesk Week of 3/16
```

**Suggested actions for stale tasks:**
- **Close it** — mark DONE with a note like "Deprioritized, closing"
- **Defer it** — move to a `pages/backlog.org` or similar, remove from daily carryover
- **Re-scope it** — break into smaller pieces, create a fresh TODO for the next step
- **Actually do it** — if it's small, maybe just knock it out

When Ethan asks to act on a stale task, confirm the action before editing the file.

**Relationship to morning briefing vs EOD:**

| Time | Focus | Staleness handling |
|------|-------|-------------------|
| Morning | Orient, plan | Light mention: "3 stale tasks" (details on request) |
| EOD | Close out, clean up | Full triage: list stale tasks, prompt for decisions |

The morning briefing should stay lightweight — just flag the stale count so you're aware.
EOD (`references/end-of-day.md`) is when you actually deal with them.
