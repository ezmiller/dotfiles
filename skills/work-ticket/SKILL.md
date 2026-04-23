---
name: work-ticket
description: >
  Execute a ticket from Ethan's techwork journal end-to-end: pickup, worktree setup,
  implementation, PR, review iteration, and merge cleanup. The journal is a textual kanban
  board and every phase of the work is recorded there. Companion to the `techwork` skill
  (techwork reads; this skill acts). Use this whenever Ethan says things like "work on
  EPD-XXXX", "let's tackle X", "work this ticket", "pick up the next ticket", "start on
  [ticket]", "ship this ticket". Also trigger for in-flight PR operations: "check PR
  comments", "pull review feedback", "address Yue's comments", "respond to the review",
  "push a follow-up". And for close-out: "close out the ticket", "mark it done", "clean up
  the worktree after merge". Do NOT trigger for read-only journal queries like "what did I
  work on", "brief me", or "what's in progress" — those belong to `techwork`. Trigger when
  Ethan is actively executing work, not just reading about it.
---

# Work-Ticket Skill

You are helping Ethan ship a ticket. This skill is the acting companion to `techwork`:
techwork reads and briefs the journal, `work-ticket` acts on it. The journal is not a
sidecar to the work — it *is* the kanban board, and every meaningful phase of the work is
recorded there.

## Core principle: the journal is the kanban board

`~/org/techwork/journals/YYYY_MM_DD.org` is where work happens and is recorded. Treat it as
a system of record, not a diary.

- **Columns** are state keywords on task headlines. Canonical flow:
  `TODO` (queued) → `STARTED` (in flight, pre-PR) → `REVIEW` (PR open, awaiting review or
  iterating on it) → `DONE` (merged/closed). Each transition is a column move and must be
  recorded with a LOGBOOK entry.
- **Cards** are task subtrees: the `* STATE ... :tags:` heading + any `:LOGBOOK:` block +
  body notes.
- **Progress log** is a chronological list of `- [YYYY-MM-DD Day] …` bullets appended under
  the card body. These are written for tomorrow's morning briefing — phrase them so they
  make sense in isolation.
- **State transitions** with LOGBOOK entries are how the "column change" is recorded.
- **Journal is authoritative over Jira.** Jira is a downstream mirror the *human*
  reconciles. If Jira and the journal disagree on a title or state, the journal's framing is
  usually closer to the code (we've caught drift this way).

Implications:

- When Ethan invokes you, the first thing you do is **read today's journal** to see where
  the ticket currently stands. That determines whether this is a fresh pickup, a resume, a
  review-iteration round, or a close-out.
- Every lifecycle phase ends with a journal write. This is *the* way the work is recorded,
  not a polite bookkeeping afterthought. If you didn't write the journal, the work isn't
  logged.

## Human owns visibility (non-negotiable firewall)

Some actions are visible to teammates or external systems. Those require a human hand on
the trigger. The skill prepares; Ethan fires.

**Skill DRAFTS, Ethan EXECUTES:**

- Pushing to remote
- Opening PRs (`gh pr create`)
- Posting PR comments or replies to reviewers
- Transitioning Jira tickets (the skill *never* modifies Jira — read-only only)
- Sharing to Slack or any external channel

**Skill acts freely (local, reversible, personal):**

- Create/remove worktrees, create/delete local branches (after merge)
- Make commits on a local branch
- Edit the journal
- Read anything (Jira, GitHub, Slack, the codebase, memory)

The rule of thumb: if an action would be seen by anyone but Ethan, draft it and hand it to
him. If it's local and reversible, proceed (but announce significant moves so he can push
back).

## Workspace conventions (quick reference)

- Journal files: `~/org/techwork/journals/YYYY_MM_DD.org` (canonical underscored format).
- Today's file is where all new writes land, even for tasks carried over from older files.
- State keywords: `TODO`, `STARTED`, `DONE`, sometimes `REVIEW` or `BLOCKED`.
- Tickets: `EPD-XXXX` in headline text, as `:epd_XXXX:` tag, or as a Jira URL.
- LOGBOOK format:
  ```
  :LOGBOOK:
  - State "DONE"       from "STARTED"    [2026-04-23 Thu 11:54]
  :END:
  ```
- Progress note format (under task body):
  ```
  - [2026-04-23 Thu] Yue CHANGES_REQUESTED: push further, use $spacer-7.5. Done (commit abc1234).
  ```

See the `techwork` skill for deeper org-mode conventions.

## Lifecycle

The lifecycle is a state machine driven by the journal. Your behavior on invocation depends
on what the ticket's current state is in today's journal.

| Journal state                | You are…            | Skip to section           |
|------------------------------|---------------------|---------------------------|
| TODO (or not yet in journal) | Picking it up       | Pickup                    |
| STARTED                      | Resuming in-flight  | Implement / Commit & Push |
| REVIEW                       | Iterating on review | Review Iteration          |
| REVIEW (after merge)         | Closing out         | Merge Cleanup             |

### 1. Pickup

1. **Read today's journal** (`~/org/techwork/journals/YYYY_MM_DD.org`). Find the task —
   match by ticket ID (`EPD-XXXX`) or by Ethan's phrasing. If today's file doesn't exist,
   ask Ethan to run the morning briefing via `techwork` first; don't create it yourself.
2. **If the ticket isn't in the journal:** ask Ethan to add a TODO heading, or offer to
   draft one for him to paste. Don't create journal headings unilaterally — the morning
   routine owns that.
3. **If there's no ticket ID in the heading:** per Ethan's global CLAUDE.md, ask whether
   there's a Jira ticket. He may confirm ticketless is intentional; in that case, ask for a
   branch prefix (e.g., `fix/`, `chore/`, `docs/`).
4. **Read the task body** — any notes, context, decisions, links. These are often the most
   valuable context you have.
5. **Cross-reference Jira** if a ticket ID exists. Fetch via the Atlassian MCP. Compare
   titles and flag drift to Ethan before proceeding — we've caught mismatches this way
   (e.g., a Jira title "Color Story Functionality Update" vs. journal framing "Curated
   Pages / Styling Ideas"; the journal was accurate to the code). Don't try to reconcile —
   just surface it.

### 2. Scope & plan

1. **Explore the codebase** for scope. Use Grep/Glob/an Explore subagent depending on
   breadth. Find the files involved, any related patterns, precedent for similar work.
2. **Present a plan** before coding. The plan should cover: what changes, where, why,
   expected scope (1 file or many), open questions. Brief, not exhaustive.
3. **Wait for alignment.** Ethan may redirect or tighten scope. Don't start edits until he
   greenlights.
4. **Once confirmed, update the journal** as part of the pickup write (see next section).

### 3. Journal pickup write

Before touching any code, transition the task in the journal and log the plan. This is the
"STARTED" column move.

- Change heading: `* TODO EPD-XXXX …` → `* STARTED EPD-XXXX …`
- Add LOGBOOK entry above existing ones:
  ```
  :LOGBOOK:
  - State "STARTED"    from "TODO"       [YYYY-MM-DD Day HH:MM]
  :END:
  ```
- Append an initial scope note under the body:
  ```
  - [YYYY-MM-DD Day] Branch `epd-NNNN-slug`, worktree at `.claude/worktrees/epd-NNNN`.
    Approach: <one-sentence summary>. <optional: flagged drift or dependency>.
  ```
- Announce the write in chat:
  `Journal: EPD-XXXX STARTED, scope note added`

### 4. Isolate (worktree by default)

Default behavior is to create a worktree off the latest `origin/main`. Opt-out only if both
conditions are met:
  - The original checkout is clean (no uncommitted changes, not mid-flight on another
    branch).
  - Ethan explicitly asks to branch-on-checkout.

Worktrees are preferred because they isolate this work from Ethan's other in-flight
branches (he often has multiple tickets open).

**Steps:**

```bash
# Always fetch first — we want latest main.
git fetch origin main

# Create the worktree on a new branch off origin/main.
# Path convention: .claude/worktrees/<slug>
# Branch convention: epd-NNNN-short-description (or <prefix>/<slug> if no ticket).
git worktree add -b <branch> .claude/worktrees/<slug> origin/main
```

Then enter the worktree via the `EnterWorktree` tool with `path:` (not `name:`) — pointing
at the worktree you just created. This is important: `EnterWorktree` with `name:` would
create a fresh worktree branched off the current HEAD, which is not what we want when the
user is mid-flight on another branch.

**Install dependencies if needed.** For yarn berry repos with `nodeLinker: node-modules`
(detected by `.yarnrc.yml` + `.yarn/` directory), run `yarn install` in the worktree before
the first commit. Pre-commit hooks typically need dependencies available. Cache-warm
installs are ~30–60s for a typical repo; budget for it. Other common cases:
  - pnpm: `pnpm install`
  - npm: `npm install` (check for `package-lock.json`)
  - Bundler: `bundle install` (check for `Gemfile`)

Use judgment based on what you see in the repo.

### 5. Implement

- Make the minimum change that solves the problem as scoped. No unrequested refactors,
  reformatting, or "while I'm here" cleanups.
- Present the diff (`git diff`) before committing. Describe what you did and why, briefly.
- If you discover mid-work that scope needs to change (not just grows — *changes*),
  pause and talk to Ethan.

### 6. Commit & push

**Read the repo's commit convention** before writing the message. Check:
- `git log --oneline -15` for style (conventional commits? ticket-prefix? something custom?)
- CLAUDE.md and memory for recorded project conventions

Examples by repo style:
- pk-shopify-theme: `EPD-XXXX: description` (ticket-prefix, lowercase description)
- strict conventional commits: `feat(scope): description`
- loose: freeform short summary

**Staging:** use `git add <file>` explicitly, not `git add -A` or `git add .`. Other files
may have been touched by Ethan or other agents. Guard against surprise commits.

**Push:** there is a known gotcha when the worktree was created with
`git worktree add -b NEWBRANCH path origin/main` — the new branch's upstream is set to
`origin/main`. A plain `git push -u origin <branch>` fails because `main` is protected.
Workaround:

```bash
git push origin HEAD:refs/heads/<branch>
git branch --set-upstream-to=origin/<branch>
```

Then commits flow normally.

### 7. PR

**ALWAYS show the PR description to Ethan for review/edit before `gh pr create`.** This is
a firm rule, not a suggestion. Even if he's OK'd the commit, the PR body is visible to the
team and needs his hand.

Draft structure:

```markdown
## Summary
<1–3 sentences on what the change does and why>

Jira: https://primary.atlassian.net/browse/EPD-XXXX

## Test plan
- [ ] <concrete test step, keyed to project QA workflow>
- [ ] <…>
```

The test plan's shape depends on the project's QA workflow. Check memory for project-QA
notes. For example, for pk-shopify-theme: tests run on a CI-provisioned test theme per
branch (preview URL posted as a PR comment by GitHub Actions). Test-plan items should name
specific pages, breakpoints, or data conditions to check on the test theme. See memory
`project_pk_theme_qa.md` if present.

Once Ethan confirms the draft:

```bash
gh pr create --title "<conventional title>" --body "$(cat <<'EOF'
<body>
EOF
)"
```

Then announce the PR URL in chat.

**Journal write:**

- Change heading: `* STARTED EPD-XXXX …` → `* REVIEW EPD-XXXX …`
- Add LOGBOOK entry above existing ones:
  ```
  :LOGBOOK:
  - State "REVIEW"     from "STARTED"    [YYYY-MM-DD Day HH:MM]
  <preserve existing entries below>
  :END:
  ```
- Append under the task body:
  ```
  - [YYYY-MM-DD Day] PR open: [[<url>][#<num>]]. <brief note about test expectations or next step>.
  ```
- Announce: `Journal: EPD-XXXX REVIEW, PR note added (#<num>)`

### 8. Review iteration

This is a loop. Each pass: pull feedback, summarize, work through with Ethan, push,
journal.

**Pull feedback:**

```bash
# Top-level reviews (state, body)
gh api repos/<owner>/<repo>/pulls/<num>/reviews

# Inline review comments (specific lines)
gh api repos/<owner>/<repo>/pulls/<num>/comments

# General PR issue comments (CI bot, discussion)
gh api repos/<owner>/<repo>/issues/<num>/comments
```

Use `--jq` filters to keep output tight. Three buckets to report:

1. **Review state changes** (APPROVED, CHANGES_REQUESTED, COMMENTED).
2. **Inline comments** (file, line, what they want).
3. **Discussion comments** (CI bot preview links, back-and-forth).

**Summarize to Ethan**, grouped by reviewer. Bold the changes being requested; leave
approvals as a one-liner. Examples:

> **yzhang1110 (CHANGES_REQUESTED)**
> - `src/styles/views/curated-page.scss:39` — "Let's push it further and use `$spacer-7\.5`"
>
> **mlue** — APPROVED, no comments.

**Work through each item *with* Ethan.** Do not auto-apply review feedback, even if it's
obvious. Review feedback is social — the reviewer is communicating with Ethan, not you.
For each item:
1. Read the context (the file/line they pointed at).
2. Propose the specific change.
3. Wait for Ethan's OK.
4. Apply, commit, push.

**Draft but don't post replies.** If a reviewer asks a question or Ethan wants to respond,
draft the reply in chat and hand it to him to post. Never `gh api` a comment yourself.

**Journal writes (one per round):**

- When feedback arrives:
  ```
  - [YYYY-MM-DD Day] <Reviewer> <state>: <brief summary of requested change>.
  ```
- When addressed:
  ```
  - [YYYY-MM-DD Day] Addressed: <what changed> (commit <sha>).
  ```

### 9. Merge cleanup

Triggered when Ethan says the PR is merged (or you can verify via `gh pr view --json state`).

**Journal close-out:**

- Change heading: `* REVIEW EPD-XXXX …` → `* DONE EPD-XXXX …`
- Add LOGBOOK entry above existing ones:
  ```
  :LOGBOOK:
  - State "DONE"       from "REVIEW"     [YYYY-MM-DD Day HH:MM]
  <preserve existing entries below>
  :END:
  ```
- Final note: `- [YYYY-MM-DD Day] Merged. Closing out.`
- Announce: `Journal: EPD-XXXX DONE, closed out`

**Worktree cleanup:**

The `EnterWorktree` tool will not remove worktrees it didn't create itself. When the
worktree was made via `git worktree add` (as this skill does), `ExitWorktree action:remove`
rejects the call. Instead:

1. `ExitWorktree action: "keep"` — returns session to the original checkout.
2. Manually: `git worktree remove .claude/worktrees/<slug>`
3. Delete the local branch: `git branch -d <branch>` (safe-delete; succeeds because origin
   has the merged branch).

Per Ethan's global CLAUDE.md, destructive git actions require explicit user permission.
`git worktree remove` and `git branch -d` are safe variants of delete, and Ethan's "clean
up the worktree" or "close out" utterance counts as explicit. Don't use `-f` or `-D` unless
the safe variants fail — and in that case, diagnose before force.

**What NOT to touch:**
- Don't transition Jira. Ethan does that on his own timeline (often tied to Slack share).
- Don't post a merge comment on the PR or in Slack.

## Journal write triggers (consolidated)

Use this as a checklist for mandatory writes. Every phase listed must end with a journal
write.

| Phase                | Write                                                      |
|----------------------|------------------------------------------------------------|
| Pickup               | `TODO → STARTED` + LOGBOOK + scope note                    |
| PR opened            | `STARTED → REVIEW` + LOGBOOK + PR link + test-plan summary |
| Review round (in)    | Append reviewer + state + requested change summary         |
| Review round (out)   | Append what changed + commit sha                           |
| Merge                | `REVIEW → DONE` + LOGBOOK + "Merged. Closing out." note    |

**Discretionary** (bar: "would this matter in tomorrow's briefing?"):

- Title/scope drift discovered.
- Approach changed mid-flight.
- New blocker surfaced (with owner).
- Tracking doc created.

**Never:**

- Create new task headings from scratch. If a ticket has no journal entry, ask Ethan or
  draft text for him to paste.
- Delete or restructure existing notes.
- Rewrite date stamps in LOGBOOK or notes you didn't write this session.
- Touch entries for other tickets, even to "clean up."

**Transparency:** every write is announced in chat as one line. Not a confirmation prompt
(that would be noisy given multiple writes per ticket). A visible log line so Ethan can
push back on anything he disagrees with.

## Project config discovery

Project-specific conventions (commit style, QA workflow, test-plan shape) vary per repo.
Discover them in this order:

1. **Read project CLAUDE.md** (and any CLAUDE.md in the home directory / global).
2. **Check memory** for project-scoped entries (look for `project_*.md` matching the repo).
3. **Inspect the repo** — `git log --oneline`, `package.json`, `.yarnrc.yml`, etc.
4. **Ask Ethan** if still ambiguous. Save what you learn to memory for reuse.

Example existing memory: `project_pk_theme_qa.md` (pk-shopify-theme tests on CI-provisioned
test themes, not local dev).

## Ticket ID policy

Jira-linked tickets are preferred but not required. Some fixes ship without tickets.

- **Ticket present** (`EPD-XXXX` in heading or tag): proceed as above. Branch: `epd-NNNN-slug`.
- **Ticket absent**: per Ethan's global CLAUDE.md, ask if there's one. He may confirm
  ticketless is intentional. Then ask for a branch prefix (`fix/`, `chore/`, `docs/`, etc.)
  and a slug. Branch: `<prefix>/<slug>`.

All other lifecycle steps are the same.

## Known gotchas

Keep these in mind; they bit us in the originating session and will recur.

1. **Worktree tracks `main` after `git worktree add -b`.** When you create a worktree with
   `git worktree add -b NEWBRANCH path origin/main`, the new branch's upstream is set to
   `origin/main`. A plain push fails because main is protected. Always push with an
   explicit refspec (`HEAD:refs/heads/<branch>`) and then fix tracking.

2. **`EnterWorktree` won't remove what it didn't create.** If the worktree was made by
   `git worktree add`, the tool rejects `action:remove`. Use `action:keep` then
   `git worktree remove` manually.

3. **Yarn berry requires install in each worktree.** `.yarnrc.yml` + `.yarn/` + no
   `node_modules` in the worktree ≠ ready. Run `yarn install` before the first commit so
   pre-commit hooks work. 30–60s from a warm cache is normal.

4. **Running dev servers are bound to the original checkout.** If Ethan has a
   `shopify theme dev` or similar running, it reads from the original path — not the
   worktree. Don't propose "temporarily apply the diff to the original checkout" as a
   workaround; that defeats the isolation. Two clean options: (a) start a second dev
   server from the worktree on a different port, (b) defer verification to
   PR-preview/CI-test-theme if the project has one. For pk-shopify-theme, option (b) is the
   default (see `project_pk_theme_qa.md`).

5. **Journal date stamps.** Always use *today's* journal file for writes, even if the task
   started on a previous day's file. Org-journal's carryover means the "current copy" of
   the task lives in today's file. Don't edit a task in an older file unless Ethan asks.

6. **Multiple commits per review round are fine.** Don't amend or rebase to squash locally.
   GitHub will typically squash on merge; the team may have their own conventions — respect
   them. If in doubt, ask.

## Non-goals

To stay lean and avoid overlap with other skills:

- **Not a Jira integration.** Read-only only. No transitions, no comment posting, no issue
  creation.
- **Not a morning-briefing tool.** That's `techwork`. If Ethan says "brief me," hand off.
- **Not a staleness triage tool.** Also `techwork`.
- **Not a journal creator.** Org-journal (via Emacs) or `techwork`'s morning-briefing step
  creates today's file. If it's missing, don't fabricate one.
- **Not an auto-replier.** All reviewer-facing speech goes through Ethan.

## Relationship to `techwork`

The two skills are intentionally split:

| Skill       | Role     | Posture            | Typical triggers                       |
|-------------|----------|--------------------|----------------------------------------|
| `techwork`  | Reader   | Default read-only  | "brief me", "what's stale", "find X"   |
| `work-ticket` | Actor  | Default acting     | "work on EPD-X", "address feedback"    |

When in doubt, if the request is a query about state → `techwork`. If it's a verb
("work", "tackle", "ship", "close out") → `work-ticket`.
