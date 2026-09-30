# End-of-Day Wrap-Up

The complement to the morning briefing. At EOD you shift from "doing" to "closing the loop":
updating task states, logging what happened, and deciding what carries forward. Like the
morning routine, this is an **interview** — Ethan doesn't have to recall and narrate his whole
day unprompted. You walk through every live project and ask; his answers become the journal
updates and tomorrow's starting point.

Staleness triage naturally fits at the end because you're in cleanup mode
(see `references/staleness-triage.md`).

Contents:
- Step 1 — Read today's file and gather evidence
- Step 2 — Interview every live project (and code review)
- Step 3 — Closing questions
- Step 4 — Propose the updates
- Step 5 — Apply on confirmation

**Step 1: Read today's file and gather evidence**

Read today's `journals/YYYY_MM_DD.org` **in full** — no `tail`, `line_offset`, or partial
reads. The carryover means the top of the file is as important as the bottom.

Then collect quick evidence of what actually happened today, so your questions can be specific
instead of generic. Don't present this as a report — it's fuel for the interview:

- **Today's LOGBOOK entries** and any notes already added to task bodies today.
- **GitHub (ground truth for PR state)** — Ethan's PRs touched today:
  ```bash
  gh search prs --author @me \
    --repo PrimaryKids/pk-shopify-theme \
    --repo PrimaryKids/pk-workers-monorepo \
    --repo PrimaryKids/pk-skills \
    --repo PrimaryKids/pk-web-inventory \
    --repo PrimaryKids/rfd \
    --updated ">=$(date +%Y-%m-%d)" \
    --json repository,number,title,url,state,isDraft,updatedAt
  ```
  A merged PR means the matching ticket is `DONE`; an open PR means in progress. Never
  assert a PR's existence or state without looking it up.
- **Jira/Multica are optional here.** Only check them if Ethan asks, or if a question needs
  it (see `references/jira-cross-reference.md`). Never treat a Jira status as authoritative on
  its own — confirm against git first. Never flag a journal item just because it has no
  Jira/Multica/GitHub counterpart; untracked work is normal.

Don't dump any of this on Ethan up front. Open with one short line ("Let's close out the day —
I'll walk through what's live") and start the interview.

**Step 2: Interview every live project (and code review)**

Walk through **every major active and blocked project, plus everything in code review** — not
just the ones you suspect moved. Order matches the morning briefing:

1. `* Follow-ups` first
2. `* Active` (ticketed/code work)
3. `* Code Review`

A **major project** is a `STARTED`/`BLOCKED` heading with real history — a subtree, dated
notes, or something spanning more than a day or two. Skip bare one-line `TODO`s with no
history (the staleness check in Step 3 covers those).

**One project at a time, one open question each, always naming what it is** — never a bare
ticket ID (see "Always pair a ticket ID with a short description" in `SKILL.md`).

- Open: "What happened with the Loop change-of-address fix today?"
- When evidence exists, use it to make the question concrete: "PR #3510 (gift bag fallback)
  shows merged — is that one done?" or "You logged a Whiplash note this morning — did that go
  anywhere by end of day?"
- If Ethan says "nothing" / "didn't touch it" → move on immediately. No follow-up.
- Ask a follow-up only if the answer implies a change or an unstated next step (e.g. "found
  the issue" → "Is a fix in progress, or does it need someone else?").

**Code Review is different: cover every item.** Those entries are single-line TODOs (a bare PR
link), so the major-project filter doesn't apply — ask about each one by name: "Did you get to
#3510 (the gift bag fallback PR)?" Reviewed/merged → `DONE`; still pending → leave `TODO`, note
any feedback given.

**Keep a running tally** as he answers (don't write yet). For each project note:
- state change (TODO → STARTED, STARTED → DONE, → BLOCKED, etc.)
- concrete facts to log (PR numbers, findings, decisions, who said what)
- what he says is next
- any new blocker

**Step 3: Closing questions**

After the walk-through, ask these — one at a time, keep each short:

1. **Unlogged work:** "Anything you worked on today that isn't in the journal — helpdesk,
   ad-hoc requests, meetings, investigations?" Offer new entries for anything he mentions.
2. **Next up:** "What do you want to pick up first tomorrow — or anything you're waiting on
   someone for?" (Fill in from what he said during the interview where you can, and confirm
   rather than re-asking.)
3. **Staleness check:** If any tasks have carried 7+ days with no activity (see
   `references/staleness-triage.md`), list them with their age and ask: close, defer, or keep
   carrying? Skip this silently if nothing is stale.

If he already covered any of these during the interview, don't ask again — just confirm in the
proposal.

**Step 4: Propose the updates**

Now — and only now — output one consolidated proposal built from his answers plus the
evidence. Every ticket gets a short description.

```
## Suggested updates

1. EPD-2367 (CMS-able Sesame Street character on home page)
   STARTED → DONE
   LOGBOOK: [2026-03-18 Wed 17:30]
   Note: "PR #3245 opened and merged"

2. Whiplash not releasing fulfillment on cancelled items
   Keep STARTED
   Note:
   - Found issue: Whiplash changed audit order behavior around 3/3
   - Need to follow up with Victoria

3. #3510 (gift bag fallback PR review)
   TODO → DONE (reviewed, approved)

4. NEW: Helpdesk — reset customer's password (from "unlogged work")
   DONE

## Next up
- Open Whiplash PR for review
- Follow up with Victoria on timeline

## Blockers
- Waiting on Victoria re: Whiplash audit order

## Staleness
- Close: Add INP metric (36 days) — "Deprioritized, closing"
- Keep carrying: Get the a/b calculator up
```

The **Next up** section is required — don't leave it out or only mention it in chat. Anything
Ethan says he'll pick up tomorrow or is waiting on belongs in the proposal as a written section
that goes into the file.

**Notes are the morning's memory.** Phrase every note so it reads well in isolation the next
morning — it's tomorrow's standup context. Prefer concrete, self-contained bullets ("Found the
issue — Whiplash changed audit order around 3/3, need to follow up with Victoria") over vague
ones ("made progress"). Write in Ethan's voice, using his phrasing from the interview.

**Step 5: Apply on confirmation**

Don't edit anything until Ethan confirms. He might say "yes to all, close the a/b calculator
one" or "skip #3, change #2's note." Apply exactly what he approved:

- Update task states (TODO → DONE, etc.)
- Add LOGBOOK entries with timestamps
- Append notes under task bodies
- Add new entries for previously unlogged work
- Mark stale tasks he chose to close as `DONE` with a "Deprioritized, closing" note
- Write the `* EOD Notes` section

**LOGBOOK format:**
```
:LOGBOOK:
- State "DONE" from "STARTED" [2026-03-18 Wed 17:30]
:END:
```

**EOD Notes heading format:** plain heading — no timestamp (the file title already has the
date). Put it at the end of the file.
```
* EOD Notes
** Next up
- ...
** Blockers
- ...
```
Omit `** Blockers` if there are none.

After applying, confirm in one line what changed. Don't recap the whole day.

**What NOT to do:**
- Don't invent work Ethan didn't mention or that the evidence doesn't show
- Don't mark things DONE unless he said so (or a merged PR confirms and he agrees)
- Don't write anything to the file during the interview — propose first, then apply
- Don't ask about every one-line `TODO` — only major projects and code review items
- Don't delete tasks — only change state or add notes
- Don't reorganize the file structure
- Don't drop bare ticket IDs into questions or proposals
