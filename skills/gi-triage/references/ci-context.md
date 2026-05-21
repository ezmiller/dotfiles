# CI Context

How the E2E pipeline actually runs, so you can interpret what a Slack
alert or run URL means and extract the right test IDs.

## The two workflows

- `.github/workflows/daily_e2e_test_run.yml` — runs at 9:05am ET daily,
  and again at 2:05pm ET if there are new commits. Runs all 4 suites
  against `https://shop-qa.primary.com`.
- `.github/workflows/pr_e2e_test_run.yml` — runs on PRs; same suites,
  but the start URL is the PR's preview theme on QA (provisioned by
  the PR CI, see project memory `project_pk_theme_qa.md`).

Both delegate to `.github/actions/run-gi-suite/action.yml`, which:

1. Runs the suite once via `ghostinspector/cli` (dockerized).
2. Extracts the suite result ID from the `✖️ Suite result: (...)` line
   in the docker output.
3. Calls the GI REST API directly (curl, not the docker CLI) to list
   failed test IDs and their viewports.
4. Retries each failed test up to `E2E_TEST_ATTEMPTS` (default 4)
   times, with 30s pauses, at the same viewport that originally failed.
5. Reports a test as "consistently failed" only if all retry attempts
   fail at the `FAILURE_THRESHOLD_PERCENT` (default 100).

This is why a Slack alert is high-signal: flakiness alone has been
ruled out by the harness. Don't waste time second-guessing whether
"maybe it's just GI being flaky."

## What's in a Slack alert

Body shape, from the workflow:

```
🚨 Daily E2E tests have CONSISTENT FAILURES!
📋 Branch: <branch>

The following test suites failed across all 4 retry attempts:

PLP Suite:
- **[AllPLP] Quickshop** (ID: 654175c44ee1df25e0e2dd8f) failed 4/4 attempts
  Link: https://app.ghostinspector.com/tests/<id>

PDP Suite:
- ...
```

Note that the same test ID can appear twice (once per viewport that
failed). The Slack message currently doesn't surface the viewport —
get that from the GH Actions log or by checking `viewportSize` on the
result via the MCP.

The "Unknown Test" name in the message means the run-gi-suite action's
test-name lookup failed (it depends on a `/tmp/suite-tests.txt` file
that isn't always written). Look up the real name with
`mcp__ghostinspector__get_test`.

## Extracting failing test IDs from a GitHub run

If the user gave you a run URL instead of a Slack paste:

```bash
gh run view --job=<job-id> --repo PrimaryKids/pk-shopify-theme --log 2>&1 \
  | grep -E '🚨 Test [a-f0-9]+ for [0-9]+x[0-9]+ consistently failed' \
  | awk '{print $3, $5}' | sort -u
```

You'll get one line per `<test_id> <viewport>` pair that consistently
failed. The job IDs are visible from `gh run view <run-id>`.

For one-shot result discovery the GI REST API also works:

```bash
# Suite results
curl "https://api.ghostinspector.com/v1/suite-results/<suite_result_id>/results/?apiKey=$GI_API_KEY&count=100"
```

But prefer the MCP tools — they're already in scope.

## What "consistent failure" rules out vs. doesn't

**Rules out:**
- Random network flakiness on a single attempt.
- GI runner cold-start issues.
- A one-off third-party script blip.

**Does NOT rule out:**
- A persistent bad state in QA (broken seed product, expired discount,
  inventory at zero) — these will fail consistently until reset.
- A deploy-induced regression that's now stable on QA — consistently
  reproducible *because* the bug is real.
- A flaky test pattern that's reproducibly flaky (e.g. a race condition
  that fires within 4 retries about 100% of the time).

The decision tree for what to do still belongs in
`classification.md`. CI context just tells you what state the test is
in when the Slack alert fired.
