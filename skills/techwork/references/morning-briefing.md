# Morning Briefing

The primary daily-use workflow (runs at any hour — don't say "morning"). Ensure today's file exists (creating it with carryover if
needed), then do an intelligent second pass: read what carried over, look at yesterday, and
give Ethan something useful.

**Three phases.** (1) *Gather* everything since the last entry in parallel — journal, Jira,
Multica, GitHub, Wispr meetings, and Kingkong (`references/engineering-manager.md`) — as tagged
claims. (2) *Verify* state claims against the strongest source (git > Jira > Multica/Kingkong;
GitHub is fetched once in phase 1 and reused as the tiebreaker), then *interview* Ethan only on
what the sources can't settle. (3) *Reconcile*: propose journal edits with evidence, apply only
what he approves, then draft the standup. Slack isn't readable yet.

**Style.** Ethan finds long lists hard to read. Interview one question at a time; any list is
~5 items max (else give a count and ask). Do the verifying before asking so questions are
informed. Skip questions the journal or an earlier answer already settles, and explain any
agent jargon (e.g. Kingkong's "re-brief") in plain words. Wispr call notes are auto-summaries
and can get action-item owners wrong — confirm who owns each. Show anything new since the last
entry (a ticket Ethan created this morning, a PR he opened) as a proposal to add.

Contents:
- Step 0 — Ensure today's journal file exists
- Step 0.5 — File carried items into sections
- Step 1 — Read the files
- Step 2 — Gather standup context
- Step 2.5 — Reconcile work across systems, then audit the journal
- Step 3 — Produce the briefing output (summary, then interview, then standup)
- Step 3.5 — Roadmap check (dated, per-day timeline)
- Step 4 — Draft the standup

**Step 0: Ensure today's journal file exists**

Check whether today's file exists before reading anything. Construct today's filename using
the current date: `~/org/techwork/journals/YYYY_MM_DD.org`.

Use `emacs__eval-elisp` to check, create, and save in one shot (it checks for an unsaved
buffer too — the file can exist only in Emacs, and a second `org-journal-new-entry` adds a
duplicate time heading):

```elisp
(let* ((today (format-time-string "%Y_%m_%d"))
       (journal-dir (expand-file-name "journals/" org-roam-directory))
       (today-file (expand-file-name (concat today ".org") journal-dir)))
  (cond ((file-exists-p today-file) :already-exists)
        ((get-file-buffer today-file)
         (with-current-buffer (get-file-buffer today-file) (save-buffer))
         :already-exists)
        (t (org-journal-new-entry nil) (save-buffer) :created)))
```

Carryover **moves** items out of the previous file into today's. Never restore or edit
carried items in a past journal — write them into today's file.

- If it returns `:already-exists` — proceed silently to Step 0.5.
- If it returns `:created` — org-journal has created the file, carried over all
  TODO/STARTED/REVIEW/BLOCKED items, and saved it to disk. Proceed to Step 0.5.
- If the elisp call errors, times out (a minibuffer prompt can hang it — don't retry), or the
  file still doesn't exist after `:created`, ask Ethan to press `C-g` / save (`C-x C-s`) in
  Emacs and confirm. Gather in the meantime; don't wait.

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

**Section check.** Carryover has silently dropped whole sections before (`* Queued` and
`* Follow-ups` vanished for two weeks). Compare today's top-level headings with the last
journal that had them; if any are missing, say so and offer to restore the non-DONE items.

**Step 1: Read the files**
- Today: `journals/YYYY_MM_DD.org` (created by org-journal with carryover)
- Yesterday: `journals/YYYY_MM_DD.org` for the previous working day

To find yesterday's file, list the journal directory and take the file immediately before
today's. Don't assume it's exactly one calendar day ago — weekends and holidays create gaps.
**Ethan doesn't work Fridays (standing day off).** So a Thursday→Monday gap (no Friday file)
is normal, not a signal that something went unlogged — don't flag it or ask about it. When
referring to that file in the briefing, call it "your last entry (Thursday)" rather than
"yesterday" if there's a gap.

**Read both files in full** — don't use `tail`, `line_offset`, or any partial read. Today's
file is the carryover dump: tasks accumulate at the top as they're carried over from older
journals, so the beginning of the file is just as important as the end. A partial read will
cause you to miss DONE tasks, stale items, or carried-over work that Ethan has already
closed out today.

**Speed:** the read tool truncates large output (a ~570-line journal got cut after ~330 lines,
forcing a second read). Get the line count first (`wc -l`), then read today's file in chunks of
~150 lines **in parallel in one step** — still in full, but without a truncate-and-reread. Read
yesterday's file in the same step.

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

4. **Inline notes added to tasks** — the `Log` child heading under STARTED tasks (and, on
   older items, loose body text): bullets, decisions, blockers. Shows what was actively being worked on even if no state change.

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
| **GitHub** (all PrimaryKids repos) | What landed | Merged/open PRs — catches ticketed and non-ticketed code work. |

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

Gather all of these in parallel, in one step (multica + gh are local; Jira via MCP). Kingkong
is two things per `references/engineering-manager.md`: **`kk-board` run directly** (about half
a second, gives board state by epic with Jira keys), and the **agent asked a narrow question,
detached, with its output saved to a file** (about 1.5 minutes; start it in this same step and
read the file later). Never ask the agent for ticket lists — that is what made it take 5 minutes.
**If the agent returns nothing, that must be the first line of the briefing** (see Step 3);
never let a missing Kingkong section go unmentioned.

```bash
# 1. Multica board — the whole primarykids workspace, not a filtered slice.
multica issue list --limit 100 --output json
#    Keep: identifier (PRI-N), title, status, assignee_type, updated_at,
#    metadata.jira_key, metadata.pr_url, metadata.pr_number.
#    IGNORE noise: onboarding/tutorial rows (PRI-1..~PRI-10, "N. …" titles),
#    TEST/harness rows, EPD-999x, and status "cancelled".

# 2. YOUR GitHub work (--author @me = Ethan's manual PRs AND his agents', which
#    push under his account; teammates excluded). Search the whole org, not a fixed repo
#    list: his work also lands in pk-options-admin, terraform, pk-knowledge-base, etc.
gh search prs --author @me --owner PrimaryKids \
  --updated ">=$(date -v-4d +%Y-%m-%d)" \
  --json repository,number,title,url,state,isDraft,createdAt,updatedAt,closedAt
#    `state`: "merged" | "open" | "closed". (macOS date; on Linux use `date -d`.)
#    To confirm a specific merge: gh pr view <n> --repo PrimaryKids/<repo> --json state.
```

**Code reviews Ethan is doing for teammates.** GitHub's "review requested" list is almost
always empty: the team asks for reviews in Slack, not GitHub. Check all three of these:

1. **Slack #pool-party:** search `in:#pool-party after:<last entry date>`. Teammates post
   PR links there, and a bot posts "Ethan Miller is starting their review!" and "Ethan Miller has given their approval!". A
   start with no approval means a review in progress. An approval means done, and it goes in
   the standup's Yesterday.
2. **Kingkong's code-review topic in the Primary Kids group:** add "anything in the
   code-review topic of the Primary Kids group" to the "waiting on Ethan" part of the agent
   prompt (see `references/engineering-manager.md`). Kingkong also makes a Multica
   "Review PR #N" row for each review packet it builds.
3. **GitHub**, to confirm state:
   `gh pr view <n> --repo PrimaryKids/<repo> --json state,mergedAt,reviews`.

Each review becomes one entry under `* Code Review` (STARTED while in progress, DONE once
approved, with a LOGBOOK line citing the #pool-party or GitHub evidence). A Multica review row
still "in progress" after the PR merged is stale: mention it as housekeeping, not a finding.

For Jira, use the MCP tool (see `references/jira-cross-reference.md`):
`jira_search(jql="assignee = currentUser() AND sprint in openSprints()", fields="key,summary,status,updated")`.
**Always pass a narrow `fields` list and never request `description`**: pulling descriptions for
a single epic's children returned ~25k tokens of text nobody needed. For children use
`fields="summary,status,assignee,updated"`.
Note Jira uses custom statuses — map by **category**: `To Do`/`Backlog` → `TODO`,
`In Progress`/`Blocked` → `STARTED`/`BLOCKED`, `In Review` → `REVIEW`, `Done`/`Nope` → `DONE`
(closed; "Nope" = closed-no-action).

**⚠️ Jira statuses are unpredictable — never treat one as authoritative on its own.**
Automations, bulk edits, board hygiene sweeps, and stale syncs can flip or misreport a
ticket's status without matching reality (e.g. a ticket showing `Done` with a fresh
`updated` timestamp that Ethan never touched). This applies even when `jira_get_issue`
confirms the same status directly — a matching re-fetch doesn't make it true, it just
means Jira is internally consistent with itself.

**State evidence** (strongest first): **GitHub merge/PR state → Jira status → Multica
status.** Git is ground truth. A PR **merged** means `DONE`. A PR **open** means in
progress. Before reporting any ticket as newly `DONE`, newly `REVIEW`, or reopened based
on Jira, confirm it against git (`gh search prs`, `gh pr view`, commit history) — if there's
no corresponding PR/commit activity, say so and ask rather than asserting the Jira status
as fact. When Multica and Jira disagree and neither is confirmed by git, prefer Jira's
category over Multica's, but still flag it as unconfirmed rather than authoritative.

You still have **no visibility into Slack or any external hand-off channel** — never infer,
assume, or mention one; `REVIEW` is Ethan's to set (or comes from Jira `In Review` *and*
a matching open PR), not something you invent from Jira alone.

**Never assert a PR's existence or state from a Jira label alone** (e.g. "Jira says In Review,
so a PR is probably open"). If a claim depends on PR/commit state, look it up
(`gh search prs`/`gh pr view`) before writing it down. If you didn't check, don't say it —
either verify or state it as an open question, not a fact.

**An open PR does not mean it was sent for colleague review.** Ethan often has PRs open that
nobody has been asked to review yet. Never say a PR is "in review" or "waiting on review"
because it's open. Check `gh pr view <n> --repo PrimaryKids/<repo> --json reviewRequests,reviews,isDraft`:
no `reviewRequests` and no `reviews` → "open, review not requested". If you still can't tell
(e.g. review was asked for in Slack, which you can't see), ask. Slack access isn't available
yet; adding it would help here.

**Map Multica epics onto existing journal structure.** Don't scatter tickets into `Queued`.
Multica groups work under parent epics (e.g. the curated-pages epic → the journal's
`STARTED EPD-2548 Curated Page Updates` block). Add related tickets as sub-items under the
matching existing heading, and update its `[n/m]` statistics cookie.

**Jira epics are the unit, not loose tickets.** Every epic Ethan owns or works must be one
journal heading with its children under it. Query epics
(`issuetype = Epic AND statusCategory != Done AND (assignee = currentUser() OR reporter = currentUser())`),
then their children (`parent in (...)`), and diff both against the journal: missing epics,
missing children, stale child statuses.

**Read the history before explaining it.** Before saying why a PR/ticket closed or stalled,
read the PR body and comments (`gh pr view --json body,comments`) and the Multica comment
thread (`multica issue comment list`). Statuses and titles alone mislead. `multica issue list`
caps at 100 — page with `--offset`.

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

**Make journal edits one `eca__edit_file` call at a time — never in parallel.** Concurrent
edits to the same file collide and some fail ("file changed since it was read"). After a batch
of edits, grep the file to confirm every one landed, and redo any that didn't.

**Follow-through outside the journal (only with explicit approval).** When a journal item
becomes DONE or dropped, Jira and GitHub may now be out of date (e.g. Jira still "In Review",
PR still open). Mention it and offer to fix it, but **never transition a Jira ticket, comment
on it, or close a PR unless Ethan explicitly says to.** When approved: call
`jira_get_transitions` first — EPD tickets name the transitions with a suffix, e.g.
`Done (2)` and `Nope (2)`; PW tickets use `Done` and `Won't Do`; Multica's Nope is
`cancelled` (`multica issue status <id> cancelled --no-start`). Use **Nope** for dropped/no-action work, **Done** for finished
work, and add a short comment saying why — with `jira_add_comment`, since the transition
tool's `comment` argument is silently dropped. Show Ethan the comment text first. Don't close PRs yourself unless asked; give him
the link if he wants to do it.

**Step 3: Produce the briefing output**

The goal is a short, conversational output that orients Ethan and then opens a dialogue
before drafting the standup. Keep every list to ~5 items; fold the rest into the interview
rather than a report. Think of it as: *here's what I found, here's where things
stand — what do you want to add?* The standup comes after that exchange, not before.

**Failed sources come first.** Before anything else, if any gather source failed or returned
nothing (Kingkong, Jira, `gh`, `multica`, Wispr), open with one line per failure, e.g.
"⚠️ Kingkong gave no answer (output was lost / timed out). His conversation-only decisions and
'waiting on you' items are missing from everything below." Say what is missing and offer a
re-run. A failure mentioned only in passing, or inferred later, is a skill violation.

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

🔴 **Active** — **every** open heading under `* Active` (STARTED, BLOCKED, REVIEW, and TODO),
including old ones. Never drop an Active item for looking stale — flag it instead, e.g.
"(no activity since 9/8)". Ethan has had important items fall off this way (the 429 work).
🟡 **Queued** — carried-over TODOs under `* Queued` with no recent activity

One line each, using the full journal headline (which already includes a description, e.g.
"EPD-2548 Curated Page Updates") — never just the bare ticket ID. Ethan can ask to expand any
item. If there are stale items (carried 7+ days with no progress), just note the count: "N
stale — deal with at EOD." (Full triage lives in `references/staleness-triage.md`.)

---

**🤖 Overnight reconciliation** *(from Step 2.5 — review only, NOT the work log)*

A single deduped, EPD-keyed list of work units, each tagged with its audit classification.
Lead with journal-accuracy findings (🔴 first — those are the point); confirmed items can be
a count. Group planned items under their epic so the list stays scannable.

**Lead with the description, ID in parentheses after — never a bare `EPD-XXXX`**
(see "Lead with the plain description" in `SKILL.md`). Pull the description
from whichever source you already queried: the Jira summary, the Multica title, or the
journal heading.

```
🔴 Mismatch (2)
  • Hide OOS filter (EPD-2549)        journal DONE  →  Jira "In Review", PR #3377 cancelled   (reopen → REVIEW?)
  • Sticky nav alignment (EPD-2590)   journal TODO  →  PR #3419 merged, Jira Done             (mark DONE?)
🟠 Done, not in journal (1)
  • shop_by_collection image linking bug (EPD-2593)  PR #3423 merged                          (add as DONE?)
🟣 In Jira, not in Multica/journal (1)
  • Loop change-of-address (EPD-2539)  Jira In Progress, worked off Jira, no PR yet            (track here?)
🟡 Planned, not tracked — Curated Page Updates epic (EPD-2548) (3)
  • Quickshop (EPD-2551) · content blocks (EPD-2553) · size filters (EPD-2306)
🔵 No ticket (1)
  • rfd #8 (RFD 5 draft), no EPD key                                                          (in journal? if not, add)
✅ Confirmed: 7 (journal + sources agree)
```

⚠️ **These are findings, not entries.** Nothing here goes into the journal automatically.
This section exists so Ethan can line the work up against his tracker and decide what to
record, add, or fix. If Step 2.5 found nothing to reconcile, say so in one line and move on.

---

**Interview every major project (and code review) — before the open question:**

Walk through every major active and blocked project, plus everything awaiting code review —
not just the stuck ones — asking what's going on. This is where journal items get updated and
where the standup draft in Step 4 gets its real content.

A **major project** is a `STARTED`/`BLOCKED` heading with real history — a subtree, dated
notes, or something that's spanned more than a day or two (e.g. the Curated Pages epic, CTS
merged-PDP support, the Short-ship refund resilience epic, SchemaApp, Bazaar Voice, Whiplash,
Loop change-of-address). Skip bare one-line `TODO`s with no history — the task snapshot
already covered those.

**Order: `* Follow-ups` first, then `* Active` (ticketed/code work), then `* Code Review`.**
Within each, go one project (or, for Code Review, one PR) at a time, in journal order.

Ask one open question per project, always naming what it is — e.g. "What's the latest on the
Loop change-of-address fix?" or "Where's the Curated Pages epic at?" Never ask about a bare
ticket ID — plain description first, ID in parens (or omitted). Ask one question at a time
(or a numbered list when batching leftovers). For an epic, don't ask about each unstarted
child — treat them as not yet reached.

**Code Review is different: cover every item, not just "major" ones.** Those entries are
single-line TODOs by nature (a bare PR link, no subtree), so the major-project filter above
doesn't apply — ask about each one, e.g. "Did you get a look at #3510 (the gift bag fallback
PR)?" Reviewed/merged → mark DONE; still pending → leave as TODO and note any feedback given.

For each answer:
- State change (unblocked, finished, new info) → update that heading now: change the
  TODO/STARTED/BLOCKED/DONE keyword, add a dated note in Ethan's voice at the top of the item's `*** Log` child heading
  (create it if missing; see "Notes on an item" in `SKILL.md`), add a `LOGBOOK` line. Only write changes he explicitly confirms (same guardrail as Step 2.5).
- New blocker → capture verbatim for `** Blockers` in the standup.
- Nothing new → move on immediately.

One question per project, a follow-up only if the answer implies a change — this is a
check-in across everything live, not an inquisition.

**Then ask — before drafting anything:**

Once every major project has been covered, close with a single open question. If Step 2.5
surfaced any 🔴/🟠/🔵 findings not already resolved in the walk-through, fold them in:

> "Anything else to add, or what's your main focus today?"

or, when unresolved findings remain:

> "Want to pull any of those remaining reconciliation items into your Active/Queued list or
> fix a state — and what's your main focus today?"

Wait for Ethan's response. He might name a priority, mention a new blocker, or say "nothing,
just draft it." Only apply journal changes he explicitly confirms.

---

**Step 3.5: Roadmap check**

Each journal has a `* Roadmap` section: a ~6-week timeline of Ethan's priorities, so he can
see and adjust his time frame. **Every day keeps its own copy.** Past days' roadmaps are a
history of how the plan shifted, so never edit or remove a roadmap in a past journal. The
heading has no TODO keyword, so org-journal carryover leaves it where it is.

1. **Start from the last roadmap.** Find the most recent journal that has a `* Roadmap`
   section and copy it into today's file at the **top**, right after the `#+` header lines and
   before `* Active` (Ethan moved it there). If none exists,
   build the first one from Active items with dates, recent 1:1 commitments, and known
   calendar events.
2. **Roll the columns forward.** The first column is the current week (Monday's date). If a
   new week started, drop past weeks and add weeks at the end so there are 6.
3. **Propose updates from the briefing.** Use what the interview and reconciliation turned
   up, like a finished item (remove its row), a new commitment with a date, a slipped due
   date, or a new calendar disruption. Show the proposed table, then ask one question:
   "Anything shift on the timeline?"
4. **Write today's copy only after Ethan confirms.** Put a new `Changes:` line at the top
   with what moved and why, e.g.
   `Changes: [2026-11-12] Migration tool due 11/18 → 11/25 (hackathon overlap).`
   Keep only today's line. Older lines stay in older journals. If nothing changed, write
   `Changes: [date] No changes.`

**Format.** A plain org table that reads as-is in org mode, with no image or rendering step.
Ethan's top requirement is that it's easy to read with perfectly aligned columns. Copy this
template exactly:

```org
* Roadmap
Changes: [2026-11-12] Migration tool due 11/18 → 11/25 (hackathon overlap).
| Priority        | Due     | 11/9  | 11/16 | 11/23 | 11/30 | 12/7  | 12/14 |
|-----------------+---------+-------+-------+-------+-------+-------+-------|
| Migration tool  | 11/25   | █████ | ░░░░░ | █████ |       |       |       |
| SchemaApp       | ongoing | ░░░░░ | ░░░░░ | ░░░░░ | ░░░░░ | ░░░░░ | ░░░░░ |
| Unification doc | 11/30   | ░░░░░ |       | ░░░░░ |       |       |       |
| Accessibility   | Jan     |       |       |       | █████ | █████ | █████ |
|-----------------+---------+-------+-------+-------+-------+-------+-------|
| Calendar        |         |       | ▚▚▚▚▚ | ▄▄▄▄▄ |       |       |       |
| Code freeze     | 12/7    | ═════ | ═════ | ═════ | ═════ |       |       |
█ focus  ░ background  ▚ hackathon  ▄ short week (holiday)  ═ code freeze
```

Alignment rules. These were tested by screenshot in Ethan's Emacs (Iosevka font, org tables
drawn in a fixed-width font):
- **Patterns, not words, in week cells.** Words like `Thx` were confusing, so every week
  cell is one pattern repeated exactly 5 times, or blank. No partial bars.

  | Pattern | Means                                   | Rows        |
  |---------|-----------------------------------------|-------------|
  | `█████` | focus that week                         | priorities  |
  | `░░░░░` | background / ongoing that week          | priorities  |
  | `▚▚▚▚▚` | hackathon                               | Calendar    |
  | `▄▄▄▄▄` | short week (holiday, or part-week away) | Calendar    |
  | `·····` | vacation / away the whole week          | Calendar    |
  | `═════` | code freeze                             | Code freeze |

  Blank priority cells in a vacation week. For a new kind of event, pick an unused pattern
  from `▌ ┄ ─` (tested single-width in Ethan's font) that looks clearly different from the
  others; Ethan rejected `▓` for vacation as too similar to `░`. Never use `■ ▪ ◆ ● ✓`: they
  draw double-wide and break the columns. Check Ethan's personal calendar for trips and
  days off with `gws calendar events list` (calendarId `primary`).
- **The legend lists only the patterns that appear in the table**, on one line directly
  under it, in the format `█ focus  ░ background  ▚ hackathon ...`, under 78 characters.
- **The whole table must be 78 characters wide or less.** Ethan's journal window is only
  about 84 characters wide, so a wider table wraps and becomes unreadable (the first version
  was 91 wide and every row wrapped). The template above is 77 wide. Don't make any column
  wider than it is there.
- **`Due` is the second column, never the last.** A text column at the right edge drifted
  out of line in testing. The last column must always be a week column.
- Week headers are `M/D`, the Monday of that week (e.g. `11/9`). Due dates use the same
  format (`12/2`). Use `ongoing` or a month (`Jan`) when there's no hard date.
- Item names are 15 characters max, plain descriptions (`Migration tool`, not
  `Seasonal product data migration tool`), with no ticket IDs or links.
- Keep about 6 priority rows, not every TODO. Below the rule go the `Calendar` row
  (hackathon, holidays, days off) and, while it applies, a `Code freeze` row.

**Check alignment after writing.** Edit with `eca__edit_file`, padding every cell to its
column's width. Then confirm all table lines have the same width (this reads the file on
disk, so it won't disturb Ethan's open buffer):

```elisp
(with-temp-buffer
  (insert-file-contents "~/org/techwork/journals/YYYY_MM_DD.org")
  (goto-char (point-min))
  (re-search-forward "^\\* Roadmap")
  (let (ws)
    (while (and (zerop (forward-line 1)) (not (looking-at "^\\* ")))
      (when (looking-at "^|") (push (string-width (buffer-substring (point) (line-end-position))) ws)))
    (delete-dups ws)))
```

One number back, 78 or less, means it's aligned and won't wrap. More than one number means
fix the padding. A number over 78 means shorten names or cells. Fix either problem before
moving on.

---

**Step 4: Draft the standup (after Ethan responds)**

Once he's replied — even briefly — draft the standup. Combine what the notes say with
what he just told you. Don't make him feel like he has to repeat himself; fill in from
the notes wherever he didn't correct them.

The standup has three subsections: `** Yesterday`, `** Today`, and `** Blockers`.

Use `* 11:00 Standup` as the heading unless Ethan specifies a different time.

After showing the draft, ask if he wants it written to the file. The new journal comes with
empty Standup headings; only skip drafting if they already have bullets in them (he's
started it).

**If Ethan wants the standup first** ("write the standup as quickly as possible, interview
after"), still run the Step 2.5 gather before drafting: it takes about 2 minutes, mostly
waiting on Kingkong. Skipping it on 11/12 put a wrong merge in Yesterday and missed a
review he approved and the og-merged cleanup epic. If he can't wait even that long, say
plainly in the draft which sources weren't checked.

**Standup gotchas:** "Yesterday" means the last working day. Check merge times with
`gh pr view --json mergedAt` before putting a merge there; a PR merged this morning goes
under Today. Approved reviews from #pool-party belong in Yesterday too.

**What to skip:** DONE items. Meeting notes or standup entries (not tasks). Blank `*  `
spacer headings org-journal adds. Don't reproduce full task bodies unprompted.
