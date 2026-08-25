# Morning Briefing

The primary daily-use workflow. Ensure today's file exists (creating it with carryover if
needed), then do an intelligent second pass: read what carried over, look at yesterday, and
give Ethan something useful.

Contents:
- Step 0 — Ensure today's journal file exists
- Step 0.5 — File carried items into sections
- Step 1 — Read the files
- Step 2 — Gather standup context
- Step 2.5 — Reconcile work across systems, then audit the journal
- Step 3 — Produce the briefing output
- Step 4 — Draft the standup

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

- If it returns `:already-exists` — proceed silently to Step 0.5.
- If it returns `:created` — org-journal has created the file, carried over all
  TODO/STARTED/REVIEW/BLOCKED items, and saved it to disk. Proceed to Step 0.5.
- If the elisp call errors or the file still doesn't exist on disk after `:created`,
  ask Ethan to save the buffer manually (`C-x C-s`) and confirm before continuing.

Don't mention this step to Ethan unless something went wrong. It should be invisible.

**Step 0.5: File carried items into sections**

Read today's file. The file header template creates four sections: `* Standup`, `* Active`,
and `* Queued`. Carryover appends items as loose headings at the end of the file, after these
sections.

Look for any headings with TODO keywords (`TODO`, `STARTED`, `REVIEW`, `BLOCKED`) that are
**not** nested under `* Standup`, `* Active`, or `* Queued`. These are the carried items that
need filing.

For each loose carried item:
- `STARTED` state → move under `* Active`
- All others (`TODO`, `REVIEW`, `BLOCKED`) → move under `* Queued`

Do this by editing the file directly using `eca__edit_file` — move the heading and its full
subtree (including body text, LOGBOOK drawers, sub-headings) from its current location to
under the appropriate section. Preserve the heading level (`**`).

If today's file already has items correctly nested under `* Active` or `* Queued` (i.e. it
was already organized, not a fresh carryover), skip this step silently.

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

Collect these signals — you'll use them to write a brief "what happened yesterday" summary
in Step 3, and then incorporate them into the standup draft after Ethan adds his input.

**Step 2.5: Reconcile work across systems, then audit the journal (automatic)**

The journal is Ethan's **central work tracker** — the goal here is to keep it **accurate and
complete**, not just to report activity. His work is spread across systems that don't fully
agree, so reconcile all of them and surface every gap:

| Source | Role | What it records |
|--------|------|-----------------|
| **The journal** | **The only complete record** | Everything Ethan works — including non-ticketed work (Helpdesk, vendor/support like Whiplash, investigations, docs) that exists *nowhere else*. |
| **Jira** | Authoritative on **status** | Tickets assigned to him. Wins when a ticket's status disagrees with Multica — but is **not** a complete list of his work. |
| **Multica** (`primarykids`) | Subset | The slice he's actively agent-working. May lag or diverge from Jira. |
| **GitHub** (the repos below) | What landed | Merged/open PRs — catches ticketed and non-ticketed code work. |

**Reconciliation runs one direction: systems → journal.** The external systems are *inputs*
that (a) confirm or correct journal items that carry an `EPD`/`PW` key, and (b) surface keyed
work not yet tracked. They are **never** the source of truth for *what work exists*.

> **⚠️ Never flag, downgrade, or propose removing a journal item just because it has no Jira,
> Multica, or GitHub counterpart.** Ethan works tickets outside Jira and does plenty of
> untracked work; a journal-only entry is normal and legitimate, not a discrepancy. Only flag
> a journal item when a system *actively contradicts* it (e.g. journal `DONE` but Jira
> `In Review`) — not merely for absence.

**Consolidate, don't report.** Dedupe work seen in multiple systems into **one unit** so it's
never double-counted, then compare each unit to the journal and flag what's wrong or missing.

**The join key is the Jira `EPD-XXXX` key** (some are `PW-XXXX`). It appears everywhere:
Jira issue key, multica `metadata.jira_key`, GitHub PR titles/branches (`EPD-2597 — …`), and
journal headlines. Match on it (PR number as secondary link). A unit with no EPD key (RFDs,
chores, planning spikes) links by PR number or title.

Gather all three (multica + gh are local; Jira via MCP — no SSH):

```bash
# 1. Multica board — the whole primarykids workspace, not a filtered slice.
multica issue list --limit 100 --output json
#    Keep: identifier (PRI-N), title, status, assignee_type, updated_at,
#    metadata.jira_key, metadata.pr_url, metadata.pr_number.
#    IGNORE noise: onboarding/tutorial rows (PRI-1..~PRI-10, "N. …" titles),
#    TEST/harness rows, EPD-999x, and status "cancelled".

# 2. YOUR GitHub work (--author @me = Ethan's manual PRs AND his agents', which
#    push under his account; teammates excluded). Hardcoded repo list:
gh search prs --author @me \
  --repo PrimaryKids/pk-shopify-theme \
  --repo PrimaryKids/pk-workers-monorepo \
  --repo PrimaryKids/pk-skills \
  --repo PrimaryKids/pk-web-inventory \
  --repo PrimaryKids/rfd \
  --updated ">=$(date -v-4d +%Y-%m-%d)" \
  --json repository,number,title,url,state,isDraft,createdAt,updatedAt,closedAt
#    `state`: "merged" | "open" | "closed". (macOS date; on Linux use `date -d`.)
#    To confirm a specific merge: gh pr view <n> --repo PrimaryKids/<repo> --json state.
```

For Jira, use the MCP tool (see `references/jira-cross-reference.md`):
`jira_search(jql="assignee = currentUser() AND sprint in openSprints()", fields="key,summary,status,updated")`.
Note Jira uses custom statuses — map by **category**: `To Do`/`Backlog` → `TODO`,
`In Progress`/`Blocked` → `STARTED`/`BLOCKED`, `In Review` → `REVIEW`, `Done`/`Nope` → `DONE`
(closed; "Nope" = closed-no-action). **When Multica and Jira disagree on a ticket's status,
Jira is authoritative.**

**State evidence** (strongest first): Jira status → GitHub merge state → Multica status. A PR
**merged** means `DONE`. A PR **open** means in progress. You still have **no visibility into
Slack or any external hand-off channel** — never infer, assume, or mention one; `REVIEW` is
Ethan's to set (or comes from Jira `In Review`), not something you invent from an open PR.

**Map Multica epics onto existing journal structure.** Don't scatter tickets into `Queued`.
Multica groups work under parent epics (e.g. the curated-pages epic → the journal's
`STARTED EPD-2548 Curated Page Updates` block). Add related tickets as sub-items under the
matching existing heading, and update its `[n/m]` statistics cookie.

Consolidate into one deduped, EPD-keyed list and classify each unit:

- ✅ **Confirmed** — present in the journal and the sources agree. Accurate.
- 🔴 **Mismatch** — journal state contradicts authority (PR merged / Jira Done but journal not
  `DONE`; journal `DONE` but Jira `In Review` and no merged PR; Multica `todo` but Jira closed).
- 🟠 **Done, not in journal** — merged PR or Jira/Multica-done unit with no journal entry.
- 🟣 **In Jira, not in Multica or journal** — assigned/in-progress work done straight off Jira
  (no ticket in Multica, maybe no PR) → the completeness gap Multica alone can't catch.
- 🟡 **Planned/in-flight, not tracked** — Multica/Jira tickets (todo/in-progress) absent from
  the journal → candidates to add under the right epic heading.
- 🔵 **No ticket** — a PR/RFD with no EPD key → direct/manual work; still audit the journal.

**Never auto-write to the journal.** Surface findings; Ethan decides what enters his tracker
(see the output section and guardrail in Step 3). When he approves edits, add a short note +
`LOGBOOK` line citing the evidence (PR #, PRI-N, or "Jira: <status>") so each change is
traceable the next morning.

**Step 3: Produce the briefing output**

The goal is a short, conversational output that orients Ethan and then opens a dialogue
before drafting the standup. Think of it as: *here's what I found, here's where things
stand — what do you want to add?* The standup comes after that exchange, not before.

Output in this order:

---

**🗓 What happened yesterday** *(2–5 bullets, memory jog only)*

Synthesize signals from yesterday's file into a tight narrative: what was worked on, what
finished, what was waiting. Pull from EOD notes, standup "Today" entries, LOGBOOK changes,
and inline task notes — but don't reproduce them verbatim or in full. The goal is to jog
memory in 10 seconds, not recap the whole day.

If there was an EOD section or a standup "Today" list, those are the highest-signal sources
— lead with them. If there's nothing useful, say so briefly.

---

**📋 Task snapshot**

Two compact lists — headline only, no task body detail:

🔴 **Active** — STARTED tasks and any TODO touched in the last 3 days
🟡 **Queued** — carried-over TODOs with no recent activity

One line each. Ethan can ask to expand any item. If there are stale items (carried 7+ days
with no progress), just note the count: "N stale — deal with at EOD." (Full triage lives in
`references/staleness-triage.md`.)

---

**🤖 Overnight reconciliation** *(from Step 2.5 — review only, NOT the work log)*

A single deduped, EPD-keyed list of work units, each tagged with its audit classification.
Lead with journal-accuracy findings (🔴 first — those are the point); confirmed items can be
a count. Group planned items under their epic so the list stays scannable.

```
🔴 Mismatch (2)
  • EPD-2549  journal DONE  →  Jira "In Review", PR #3377 cancelled   (reopen → REVIEW?)
  • EPD-2590  journal TODO  →  PR #3419 merged, Jira Done            (mark DONE?)
🟠 Done, not in journal (1)
  • EPD-2593  PR #3423 merged                                        (add as DONE?)
🟣 In Jira, not in Multica/journal (1)
  • EPD-2539  Jira In Progress, worked off Jira, no PR yet           (track here?)
🟡 Planned, not tracked — Curated epic EPD-2548 (3)
  • EPD-2551 Quickshop · EPD-2553 content blocks · EPD-2306 size filters
🔵 No ticket (1)
  • rfd #8  RFD 5 draft, no EPD key                                  (in journal? if not, add)
✅ Confirmed: 7 (journal + sources agree)
```

⚠️ **These are findings, not entries.** Nothing here goes into the journal automatically.
This section exists so Ethan can line the work up against his tracker and decide what to
record, add, or fix. If Step 2.5 found nothing to reconcile, say so in one line and move on.

---

**Then ask — before drafting anything:**

End with a single open question. If Step 2.5 surfaced any 🔴/🟠/🔵 findings, fold the
reconciliation into it so it drives action:

> "Anything to add about yesterday, or what's your main focus today?"

or, when there are findings:

> "Want to pull any of those reconciliation items into your Active/Queued list or fix a
> state — and what's your main focus today?"

Wait for Ethan's response. He might clarify what he actually worked on, name a priority,
mention a blocker, ask to reconcile a specific item, or say "nothing, just draft it." All of
that shapes the standup. Only apply journal changes he explicitly confirms.

---

**Step 4: Draft the standup (after Ethan responds)**

Once he's replied — even briefly — draft the standup. Combine what the notes say with
what he just told you. Don't make him feel like he has to repeat himself; fill in from
the notes wherever he didn't correct them.

The standup has three subsections: `** Yesterday`, `** Today`, and `** Blockers`.

Use `* 11:00 Standup` as the heading unless Ethan specifies a different time.

After showing the draft, ask if he wants it written to the file. If today's file already
has a `* Standup` heading, skip drafting — he's already started it.

**What to skip:** DONE items. Meeting notes or standup entries (not tasks). Blank `*  `
spacer headings org-journal adds. Don't reproduce full task bodies unprompted.
