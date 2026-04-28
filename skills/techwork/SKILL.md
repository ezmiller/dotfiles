---
name: techwork
description: >
  Knowledge base and assistant for Ethan's ~/org/techwork notes workspace at Primary.com.
  Use this skill whenever the user asks about their work notes, daily journal, JIRA tickets,
  ongoing projects, Whiplash, Shopify, pk-shopify-theme, refunds, SSN, helpdesk tasks, or
  anything they've "written down" or "logged" at work. Also trigger this skill when the user
  says things like "what did I work on", "what's the state of X", "find my notes on Y",
  "add to today's journal", "what tickets are in progress", "what did I do last week",
  or any variant of querying or updating their personal work log. ALWAYS trigger this skill
  for morning briefing requests: "brief me", "morning briefing", "what's my day look like",
  "prep today's journal", "what carried over", "what should I work on today".
---

# Techwork Notes Skill

You are helping Ethan navigate and manage his personal work notes at `~/org/techwork`. This
is a knowledge base and daily work journal, not a production codebase. Your primary role here
is to **read, find, summarize, and explain** — and only write/edit when explicitly asked.

## Workspace Overview

```
~/org/techwork/
├── journals/         # Daily work log (org-mode files, YYYY_MM_DD.org or YYYY-MM-DD.org)
├── pages/            # Stable topic notes (org-roam pages)
├── scripts/          # Shell utilities
├── CONTEXT.md        # Workspace conventions (READ THIS FIRST on any new session)
└── logseq/           # Logseq config (mostly ignore)
```

There are two file naming conventions in `journals/`:
- **Underscored** (canonical): `2026_03_18.org` — these are the primary files
- **Hyphenated** (`2026-03-18.org`): present in some sub-folders, may be newer/logseq-style
- Backup tilde files (`*.org~`) — ignore these

## Org-mode Conventions

Journal files use org-mode syntax:

```org
* TODO Task title :tag1:tag2:
** Sub-heading
*** Details

* STARTED In-progress task
* DONE Completed task
:LOGBOOK:
- State "DONE" from "STARTED" [2026-01-15 Thu 10:00]
:END:
```

**Key elements:**
- `TODO` / `STARTED` / `DONE` — task states
- `:tag:` — org tags on headlines (e.g., `:shopify:`, `:ux:`, `:performance:`)
- `[[file:../pages/foo.org][Display text]]` — org-roam links to pages
- `[[https://...][Link text]]` — external links (JIRA, GitHub PRs, Slack, Shopify admin)
- `#+begin_quote ... #+end_quote` — block quotes
- `:PROPERTIES: ... :END:` — metadata blocks
- `[YYYY-MM-DD Day HH:MM]` — timestamps
- `* HH:MM Heading` — time-marked headings (e.g., `* 11:00 Standup`, `* 09:22`)

## JIRA Ticket Conventions

Tickets follow the pattern `EPD-XXXX` (e.g., `EPD-2466`). They appear:
- In headline text: `* TODO EPD-2466: Convert collectionQuery to GraphQL`
- As org tags: `:epd_2466:`
- In linked JIRA URLs: `https://primary.atlassian.net/browse/EPD-2466`

When the user asks about a ticket number, search for it in multiple forms: `EPD-2466`,
`EPD_2466`, `epd-2466`, and `epd_2466`.

## Key Projects & Systems (Primary.com context)

These are recurring topics in the notes. When the user asks about one, search both journals
and pages:

| System / Topic         | Notes File(s)                              | Tags                        |
|------------------------|--------------------------------------------|-----------------------------|
| Shopify theme          | `pages/primary_systems.org`, journals      | `:shopify:`, `:pk_theme:`   |
| Short Ship Notifier    | `pages/short_ship_notifier.org`, journals  | `:ssn:`, `:whiplash:`       |
| Whiplash (3PL)         | `pages/whiplash.org`, journals             | `:whiplash:`                |
| Dual Discounts         | `pages/dual_discounts.org`, journals       | `:discounts:`               |
| Options Admin          | `pages/options_admin.org`, journals        | `:options_admin:`           |
| Shopify Collective     | `pages/shopify_collective.org`, journals   | `:collective:`              |
| Size Charts            | `pages/size_charts.org`, journals          | `:size_charts:`             |
| Avalara / Tax          | journals                                   | `:tax:`, `:avalara:`        |
| PLP / GraphQL          | `pages/plp_graphql_investigation.org`      | `:graphql:`, `:plp:`        |
| Recoil → Jotai         | journals                                   | `:jotai:`, `:recoil:`       |
| Nordstrom integration  | `pages/nordstrom_integration.org`          | `:nordstrom:`               |
| Helpdesk               | journals (weekly entries)                  | `:helpdesk:`                |

## Search & Retrieval Strategy

When the user asks "what do we know about X" or "find my notes on Y":

1. **Today's journal first** — open `journals/YYYY_MM_DD.org` (today's date)
2. **Tolerant search** — case-insensitive, allow spaces/underscores/hyphens in names
3. **Org tags as first-class signals** — `:whiplash:` headline is relevant to "Whiplash" queries
4. **Pages** — check `pages/` for a stable note on the topic
5. **Broad grep as last resort** — grep across workspace with tolerant patterns

When grepping, prefer `eca__grep` with case-insensitive patterns. For multi-word concepts,
search both forms: `bazaar_voice` and `bazaar voice`.

## Read-Only Default

**Default to read-only.** Analyze, summarize, and suggest — do not modify files unless
Ethan explicitly asks you to edit a specific file or section.

When edits ARE requested:
- Preserve his voice, style, and formatting
- Prefer small, incremental changes
- For journal entries: append under the relevant heading (or today's date heading if new)
- For pages: edit the specific section, not the whole file
- Ask before making structural changes

## Common Tasks

### Morning Briefing

**Trigger phrases:** "brief me", "morning briefing", "what's my day", "prep today's journal",
"what carried over", "what should I work on today"

This is the primary daily-use workflow. Your job is to ensure today's file exists (creating
it with carryover if needed), then do an intelligent second pass: read what carried over,
look at yesterday, and give Ethan something useful.

**Step 0: Ensure today's journal file exists**

Check whether today's file exists before reading anything. Construct today's filename using
the current date: `~/org/techwork/journals/YYYY_MM_DD.org`.

Use `emacs__eval-elisp` to check, create, and save in one shot:

```elisp
(let* ((today (format-time-string "%Y_%m_%d"))
       (journal-dir (expand-file-name "journals/" org-roam-directory))
       (today-file (expand-file-name (concat today ".org") journal-dir)))
  (if (file-exists-p today-file)
      :already-exists
    (org-journal-new-entry nil)
    (save-buffer)
    :created))
```

- If it returns `:already-exists` — proceed silently to Step 1.
- If it returns `:created` — org-journal has created the file, carried over all
  TODO/STARTED/REVIEW/BLOCKED items, and saved it to disk. Proceed to Step 1.
- If the elisp call errors or the file still doesn't exist on disk after `:created`,
  ask Ethan to save the buffer manually (`C-x C-s`) and confirm before continuing.

Don't mention this step to Ethan unless something went wrong. It should be invisible.

**Step 1: Read the files**
- Today: `journals/YYYY_MM_DD.org` (created by org-journal with carryover)
- Yesterday: `journals/YYYY_MM_DD.org` for the previous working day

To find yesterday's file, list the journal directory and take the file immediately before
today's. Don't assume it's exactly one calendar day ago — weekends and holidays create gaps.

**Read both files in full** — don't use `tail`, `line_offset`, or any partial read. Today's
file is the carryover dump: tasks accumulate at the top as they're carried over from older
journals, so the beginning of the file is just as important as the end. A partial read will
cause you to miss DONE tasks, stale items, or carried-over work that Ethan has already
closed out today.

**Step 2: Gather standup context**

Ethan will write his own standup — your job is to surface what he needs to remember. Look
at yesterday's file and collect signals in priority order:

1. **EOD notes (strongest signal)** — if yesterday's file has an EOD wrap-up section, or
   if task bodies have notes that look like they were added at end-of-day (prose bullets,
   PR links, findings logged), surface those first. These are deliberate records of what
   happened and should be highlighted verbatim or near-verbatim.

2. **Yesterday's standup entry (if present)** — if there's already a `* Standup` heading
   in yesterday's file that Ethan filled in, surface the "Yesterday" and "Today" bullets
   from it. He may want to carry "Today" forward as a starting point.

3. **State changes and LOGBOOK entries** — tasks that moved from TODO → STARTED, or
   STARTED → DONE yesterday (check LOGBOOK timestamps). These signal actual work happened.

4. **Inline notes added to tasks** — body text under STARTED tasks (bullets, decisions,
   blockers). Shows what was actively being worked on even if no state change.

5. **Meeting or 1:1 notes** — headings that look like meetings imply context that may be
   relevant to what's next.

Don't synthesize these into a standup. Present them as raw evidence — Ethan will use
them to recall the day and write his own update.

**Step 3: Produce the briefing output**

Output in this order:

---
**📋 Standup context** *(write your own — here's what happened yesterday)*

If yesterday had an EOD section or logged notes, show those prominently:
> **EOD notes from yesterday:**
> - [verbatim or near-verbatim bullets from EOD notes / task bodies]

If yesterday's standup already exists:
> **Yesterday's standup (Today bullets):**
> - [the "Today" bullets from yesterday's standup, as a memory jogger]

Then list other signals (state changes, LOGBOOK, inline notes) briefly — 1 line each.
Keep this section tight: the goal is jogging memory, not a wall of text.

If today's file does **not** already have a `* HH:MM Standup` heading, draft a standup
based on the context gathered above and offer to paste it in. Use `* 11:00 Standup` as the
heading unless Ethan specifies a different time. If the standup heading already exists, skip
this — Ethan has already started it.

---

**🔴 Active (work on today)**
Tasks that are STARTED, or TODO with recent journal activity in the last 3 days.

**🟡 Queued (carried over, lower urgency)**
Tasks that are TODO with no recent body updates. One line each, just the headline.

**⚪ Stale (hasn't moved in 7+ days)** — light touch in morning
Just mention the count: "3 stale tasks — run 'what's stale' or deal with them at EOD."
Don't list them all in the morning briefing — keep it lightweight. Full staleness triage
belongs in the end-of-day wrap-up when Ethan is in cleanup mode.

---

Keep the output scannable. Don't reproduce the full task bodies — just the headline and
any critical context (blocker, PR link, key decision pending). Ethan can ask to expand any
item.

**What to skip:** DONE items. Anything that looks like a meeting note or standup entry
(not a task). The blank `*  ` heading that org-journal adds as a spacer.

### Staleness Triage

**Trigger phrases:** "what's stale", "stuck tasks", "what should I close", "triage my tasks",
"what's been sitting around"

Staleness triage helps identify tasks that have been carried over repeatedly without progress.
This is useful both as part of morning briefing and as a standalone cleanup exercise.

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

### End-of-Day Wrap-Up

**Trigger phrases:** "wrap up", "end of day", "EOD", "close out today", "what did I do today",
"log my day"

This is the complement to morning briefing. At EOD, you're shifting from "doing" to "closing
the loop" — updating task states, logging what happened, and deciding what carries forward.
Staleness triage naturally fits here because you're in cleanup mode.

**Step 1: Ethan describes what happened**

Ethan gives a plain-English summary, e.g.:
> "Finished the nav character ticket, opened PR. Spent most of the day on the Whiplash
> investigation — found the issue but haven't fixed it yet. Didn't touch the discount
> code thing."

**Step 2: Parse against today's journal**

Read today's file and match Ethan's description to existing tasks:
- "Finished the nav character ticket" → find the STARTED task about nav/character, suggest DONE
- "Whiplash investigation" → find the STARTED Whiplash task, suggest adding notes to body
- "Didn't touch discount code" → note that it remains TODO, no changes needed

**Step 3: Suggest updates**

Output a proposed set of changes:

```
## Suggested updates

1. EPD-2367: CMS-able Sesame Street character on home page
   STARTED → DONE
   Add to LOGBOOK: [2026-03-18 Wed 17:30]
   Add note: "PR opened: #3245"

2. Solve Whiplash not releasing fulfillment on cancelled items
   Keep STARTED
   Add note under today's entry:
   - Found issue: Whiplash changed audit order behavior around 3/3
   - Need to follow up with Victoria

3. Enhance discount code error messaging
   Keep TODO (no changes)

## Next up
Things to carry forward into tomorrow / next week (derived from unfinished work and Ethan's description):
- Open Whiplash PR for review
- Follow up with Victoria on timeline

## Staleness check

⚪ These carried over again today with no activity:
- TODO Add INP metric (36 days)
- TODO Get the a/b calculator up (57 days)

Close, defer, or keep carrying?
```

The "Next up" section is important — don't leave it out of the proposal or summarize it only in chat. If Ethan mentions things he still needs to do or plans to pick up tomorrow, those belong in the proposal as a written section to be added to the file, not just noted conversationally.

**Notes are the morning's memory.** When writing notes to add to task bodies, phrase them so
they read well in isolation the next morning. A note like "Found the issue — Whiplash changed
audit order around 3/3, need to follow up with Victoria" is tomorrow's standup context.
Prefer concrete, self-contained bullets over vague ones like "made progress".

**Step 4: Apply on confirmation**

Don't edit anything until Ethan confirms. He might say:
> "Yes to all. Close the a/b calculator one."

Then apply the changes:
- Update task states (TODO → DONE, etc.)
- Add LOGBOOK entries with timestamps
- Append notes under task bodies
- Mark stale tasks as DONE with "Deprioritized, closing" note

**LOGBOOK format:**
```
:LOGBOOK:
- State "DONE" from "STARTED" [2026-03-18 Wed 17:30]
:END:
```

**EOD Notes heading format:**
Use a plain heading — no timestamp. The file already has the date in its title.
```
* EOD Notes
** Next up
- ...
```

**What NOT to do:**
- Don't invent work Ethan didn't mention
- Don't mark things DONE unless he said they're done
- Don't delete tasks — only change state or add notes
- Don't reorganize the file structure

**Relationship to morning briefing:**

| Time | Focus | Staleness handling |
|------|-------|-------------------|
| Morning | Orient, plan | Light mention: "3 stale tasks" (details on request) |
| EOD | Close out, clean up | Full triage: list stale tasks, prompt for decisions |

The morning briefing should stay lightweight — just flag the stale count so you're aware.
EOD is when you actually deal with them.

### Finding today's notes
Construct today's filename: `~/org/techwork/journals/YYYY_MM_DD.org` using today's date.
Read it and summarize open tasks (TODO/STARTED) and any notable activity.

### Summarizing a ticket
1. Search journals (recent first) for `EPD-XXXX`
2. Check if there's a dedicated page under `pages/`
3. Summarize: what is it, current status (TODO/STARTED/DONE), key decisions, blockers

### Jira Cross-Reference

**Trigger phrases:** "check jira", "sync with jira", "jira status", "compare jira",
"what's out of sync", "ticket status"

This recipe compares local journal state against live Jira ticket status. Useful for:
- Catching drift (you marked something DONE locally but forgot to update Jira, or vice versa)
- Sprint planning sanity checks
- Making sure your journal reflects reality before standup

**This is opt-in.** Don't automatically query Jira during morning briefing — it adds latency
and isn't always needed. Only run when Ethan explicitly asks.

**Step 1: Extract ticket IDs from today's journal**

Scan today's journal for `EPD-XXXX` patterns. Collect unique ticket IDs from:
- Task headlines: `* TODO EPD-2466: Convert collectionQuery to GraphQL`
- Org tags: `:epd_2466:`
- Inline mentions in body text
- Links: `https://primary.atlassian.net/browse/EPD-2466`

**Step 2: Query Jira for each ticket**

Use the `mcp-atlassian__jira_get_issue` tool to fetch current status:
```
jira_get_issue(issue_key="EPD-2466", fields="summary,status,assignee")
```

For multiple tickets, batch them if possible or query in parallel.

**Step 3: Compare and report divergence**

Map org states to Jira statuses:
| Org state | Expected Jira statuses |
|-----------|------------------------|
| TODO | To Do, Backlog, Open |
| STARTED | In Progress, In Review |
| REVIEW | In Review, Code Review |
| DONE | Done, Closed, Resolved |
| BLOCKED | Blocked (if your Jira has this) |

**Output format:**

```
## Jira Sync Check (5 tickets)

✅ In sync (3)
- EPD-2466: STARTED locally, "In Progress" in Jira
- EPD-2481: STARTED locally, "In Progress" in Jira  
- EPD-2482: TODO locally, "To Do" in Jira

⚠️ Diverged (2)
- EPD-2378: DONE locally but "In Progress" in Jira
  → Maybe update Jira? Or reopen locally?
- EPD-2400: TODO locally but "Done" in Jira
  → Maybe mark DONE locally?
```

**What to do with divergence:**
- **Local ahead of Jira** — remind Ethan to update Jira (or offer to add a comment)
- **Jira ahead of local** — suggest marking the local task DONE
- **Ambiguous** — just surface it, let Ethan decide

**Don't auto-fix.** Always surface the divergence and let Ethan choose what to do. Jira
is the system of record for the team; local journal is personal working memory. They serve
different purposes and occasional drift is normal.

**Bonus: tickets in Jira but not in journal**

If Ethan asks "what am I missing", query his assigned tickets in the current sprint:
```
jira_search(jql="assignee = currentUser() AND sprint in openSprints()")
```

Compare against tickets mentioned in recent journals. Surface any that are assigned to him
but have no local journal entry — these might need attention.

### What did I work on recently?
Read the last 3-5 journal files (sorted by date descending). Summarize STARTED/DONE items
and any standup notes or 1:1 notes.

### Adding to today's journal
When asked to log something, append a new `* TODO` or dated note under today's journal
heading. Use the user's phrasing and org-mode conventions. Confirm before writing.

### Status of a project/system
1. Check relevant page under `pages/`
2. Grep journals for recent mentions
3. Synthesize current status with any recent updates from journals overriding older page info

## Emacs & Org-roam Context

- Notes are managed via **org-roam** (graph navigation) and **Logseq** (block-based editor)
- Org-roam IDs appear as `:PROPERTIES: :ID: <uuid> :END:` — treat these as stable identifiers
- `[[id:uuid][Title]]` links are org-roam links; treat them like page references
- The `org-roam.db` is a SQLite database — don't read it, use file-based grep instead
