# End-of-Day Wrap-Up

The complement to the morning briefing. At EOD, you're shifting from "doing" to "closing the
loop" — updating task states, logging what happened, and deciding what carries forward.
Staleness triage naturally fits here because you're in cleanup mode
(see `references/staleness-triage.md`).

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
