---
name: gi-triage
description: >
  Diagnose and propose fixes for failing Ghost Inspector E2E tests in the
  pk-shopify-theme project. Trigger whenever the user mentions Ghost Inspector,
  GI tests, "e2e failed", "consistent failures", a `app.ghostinspector.com`
  link, a Ghost Inspector test ID, or pastes the Slack alert from the
  "alerts" or "e2e-testing" channel about daily E2E test failures. Also
  trigger for "triage GI", "what broke the e2e tests", "Quickshop test
  failing", "PDP suite failing", or any phrasing where the user wants to
  figure out *why* one or more GI tests are red and what to do about it.
  Use this skill even when the user names only a single failing test —
  the diagnostic loop is the same.
---

# Ghost Inspector Triage

Diagnose failing Ghost Inspector tests in pk-shopify-theme, classify each
failure, and recommend a concrete next step: edit the GI test, change
theme code, or wait (transient/QA-env issue).

## Inputs you'll typically receive

- A Slack alert from `#alerts` or `#e2e-testing` listing test IDs and
  `app.ghostinspector.com/tests/<id>` links.
- A GitHub Actions run URL (the workflow is `daily_e2e_test_run.yml` or
  `pr_e2e_test_run.yml`).
- One or more bare test IDs (24-char hex), e.g. `654175c44ee1df25e0e2dd8f`.
- A vague "the GI tests are failing — figure out what's going on."

Parse whichever you got into a list of test IDs. If the user only points
at a GitHub run, fetch failing test IDs from the run log first (see
`references/ci-context.md`). For a Slack paste, the IDs are in
`(ID: <hex>)` and the suite headings tell you which suite each belongs
to.

## What the suites cover

Four suites run against `https://shop-qa.primary.com` at two viewports
(`1280x800` and `375x667`). A test failing only at one viewport is a
strong signal — that maps onto the desktop/mobile theme split.

| Suite | Suite ID | Focus |
|---|---|---|
| PLP | `6536eb605d4962e44b0bef6a` | Collection / listing pages, filters, quickshop |
| PDP | `652fe64c4ee1df25e01cf3fe` | Product detail pages |
| Additional-PDP | `653acfec5d4962e44bfbaa16` | PDP edge cases (bundles, subscriptions) |
| Cart | `6613f2df752b1a838b96fc12` | Cart drawer, checkout entry |

The `daily_e2e_test_run.yml` retries each failing test up to 4 times
with a 30-second pause. A Slack alert means *every* attempt failed —
flakiness alone has been ruled out by the harness. Treat the failure as
real until proven otherwise.

## The triage loop

For each failing test, do these steps. They form one pass; repeat for
each test in the batch.

### 1. Pull the test definition and last result

```
mcp__ghostinspector__get_test(test_id=<id>)
```

This gives you the test name, steps, target URL, and a pointer to the
last result. From the steps, you know what the test is *trying* to
verify. Skim the step list before reading the failure — context first,
error second.

Then fetch the most recent failed result:

```
mcp__ghostinspector__list_test_results(test_id=<id>, count=5)
# pick the most recent failing one, then:
mcp__ghostinspector__get_test_result(result_id=<id>)
```

The result payload contains per-step pass/fail, the error message, a
screenshot URL for the failing step, and the variables that were live.

### 2. Classify the failure

Read `references/classification.md` for the full rubric, but the top of
the decision tree:

- **Selector not found / element not visible** → almost always a markup
  change in the theme OR a flaky selector that needs hardening.
- **Assertion mismatch** (text, count, URL, attribute) → either content
  drift in QA, a real regression, or the test's assertion is stale.
- **Timeout waiting for navigation/network** → either QA env latency,
  or a JS error blocking page interactivity. Check console errors in
  the result payload.
- **Step that depends on prior state** (cart count, login session) →
  often QA data drift; the seed product was deleted, the discount code
  expired, the SKU changed.
- **Only fails at one viewport** → responsive markup divergence; the
  desktop and mobile flows in the theme use different partials.

### 3. Cross-check against the theme

If the failure looks like a real markup/behavior change, find the
responsible code. **Before grepping or git-logging, fetch and pin to
`origin/main`** — the local repo is almost always on a feature branch
and the local `main` is almost always stale:

```bash
git -C /Users/ethanmiller/Projects/pk-shopify-theme fetch origin main
# then use origin/main everywhere instead of HEAD/main:
git -C /Users/ethanmiller/Projects/pk-shopify-theme log --since=<date> --oneline origin/main
git -C /Users/ethanmiller/Projects/pk-shopify-theme show origin/main:src/path/to/file.tsx
```

This is non-negotiable: if you grep or read files from the working
tree without verifying it matches origin/main, you may be looking at
code from weeks before the failure and miss the change that caused it.
Use `git diff <last-known-good-sha>..origin/main -- <path>` to see
what actually changed between a previously passing CI run and the
failed one.

- **Selector failures:** grep the failing selector (or a stable
  substring of it) against `origin/main` — usually in `src/components/`,
  `src/containers/`, `sections/`, or `snippets/`. Don't trust
  `git blame` on the *last-changed line* of a class definition: a
  selector can break because a *new wrapper element* was inserted in
  the parent component, even though the class list and the source line
  haven't moved. Look at the surrounding markup structure (parent
  elements, sibling order) in addition to the class chain itself.
- **Behavior failures** (cart drawer doesn't open, quickshop doesn't
  fire): look in the relevant JS/TSX module. Cross-reference with
  recent commits on `origin/main` since the last green CI run.
- **For a single-viewport failure**, look for `{% if device.mobile %}`
  / media-query branches near the suspect selector.
- **Don't anchor on "code unchanged" before checking the full git log
  on origin/main.** It's the most common false negative in this work:
  the local branch's HEAD predates the failure, so files *look*
  unchanged when they're actually weeks behind production. If the
  failure pattern is consistent across many tests, lean toward "we
  haven't found the recent change yet" rather than "no recent change
  exists."

### 4. Decide the next action

For each test, pick one:

- **Edit the GI test** — if the test's assertion or selector is stale
  but the app behavior is correct. The MCP does not have a write API
  for editing steps; you produce a precise change list (step number,
  current value, proposed value, why) for the user to apply in the GI
  web UI. Include the test link.
- **Fix the theme** — if the regression is real. Describe the change
  needed in the theme repo. Don't open a PR from inside this skill —
  hand the diagnosis to the user, who will use `work-ticket` if they
  decide to fix it.
- **QA-environment / data fix** — if a seed product is gone, a discount
  expired, etc. The fix is usually re-seeding QA, not code. Flag it
  clearly: this is an ops problem, not an engineering one.
- **Retry to confirm** — when classification is ambiguous, use
  `mcp__ghostinspector__execute_test(test_id=<id>, viewport=<viewport>)`
  to re-run against QA. A test that passes on a manual single retry
  *after* 4 CI retries failed is suspicious — usually means QA was in
  a transient bad state, or the test itself has order-dependence on
  the suite.

### 5. Produce the triage report

After running through the batch, output a single report in this exact
shape. The user reads this and acts; it's the deliverable.

```markdown
# GI Triage — <date> — <suite or run summary>

## Summary
- N failing tests across <suites>
- Verdicts: X theme regression, Y stale test, Z QA data, W transient

## Recommended order of operations
1. <highest-leverage action first>
2. ...

## Per-test details

### [Suite] Test Name (<test_id>)
- **Link:** https://app.ghostinspector.com/tests/<id>
- **Viewports failing:** 1280x800, 375x667
- **Failing step:** <step number + verb + target>
- **Error:** <one-line from result payload>
- **Verdict:** theme regression | stale test | QA data | transient
- **Action:** <concrete next step>
  - If theme: file path(s) + what to change
  - If test edit: step-by-step diff for the GI web UI
  - If QA data: what's missing/wrong + who to ping
- **Evidence:** screenshot URL, suspected commit, console error, etc.
```

If a single root cause explains many failures (common — one selector
rename can take down a dozen tests), say so up top under Summary and
then list the affected tests compactly under that single root cause
rather than repeating the verdict per-test.

## Constraints and gotchas

- **No silent test edits.** The MCP does not include a tool for
  modifying test steps; even if it did, never auto-edit a GI test —
  Ethan applies these changes manually in the GI web UI so the diff
  is visible and reversible. Your job is to produce a precise change
  list, not to apply it.
- **Don't open PRs.** Hand off diagnoses; let `work-ticket` and the
  user drive the fix branch.
- **`shop-qa.primary.com` is the start URL** — the same theme code is
  on QA and staging, so failures here usually reflect what just
  merged to `main` (which deploys to QA on merge). Cross-reference
  the last green CI run vs. the first red one to bound the suspect
  commit range.
- **Single-viewport failures are real signals.** Don't dismiss a test
  failing only at `375x667` as flaky — the mobile theme partials
  diverge enough that mobile-only regressions are common.
- **Retry budget matters.** Each `execute_test` call costs run time and
  GI usage. Re-run for verification only when classification is
  genuinely ambiguous after reading the existing result.

## Reference files

- `references/classification.md` — full failure-type taxonomy with
  examples of how each appears in the GI result payload.
- `references/ci-context.md` — how the daily/PR workflows pick failing
  IDs, what "consistent failure" really means, and where the IDs live
  in the GH Actions log if you only got a run URL.
- `references/common-fixes.md` — recurring fix patterns: selector
  hardening idioms in GI, viewport-specific assertions, data-seed
  expectations for QA.
