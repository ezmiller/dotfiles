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

## Bulk-inventorying failing-step data via the REST API

When a batch of >3 tests is failing, fetch the failing-step data for
all of them in one pass via curl + jq rather than via the MCP. The
MCP's `get_test_result` returns 60–110KB payloads per test that spill
to disk by default. Curl avoids the spill and lets you build a single
JSONL file you can group on.

```bash
KEY="$GHOST_INSPECTOR_API_KEY"
# IDS=(<list of failing test ids>)

# Sanitize raw GI API responses through Python first — see "Why
# the python prefilter is non-optional" below. Pipe everything
# through this before jq.
sanitize() {
  python3 -c "import sys,json; print(json.dumps(json.loads(sys.stdin.read(), strict=False)))"
}

extract_one() {
  local id=$1
  local results_json result_id result
  results_json=$(curl -s --compressed \
    "https://api.ghostinspector.com/v1/tests/${id}/results/?apiKey=${KEY}&count=5" | sanitize)
  result_id=$(echo "$results_json" | jq -r '[.data[] | select(.passing == false)] | .[0]._id // empty')
  [ -z "$result_id" ] && { echo "{\"test\": \"$id\", \"error\": \"no failing result\"}"; return; }
  result=$(curl -s --compressed \
    "https://api.ghostinspector.com/v1/results/${result_id}/?apiKey=${KEY}" | sanitize)
  echo "$result" | jq -c --arg id "$id" --arg result_id "$result_id" '{
    test_id: $id,
    result_id: $result_id,
    name: .data.name,
    viewport: .data.viewportSize.width,
    fail: ([.data.steps[] | select(.passing == false and .optional != true)] | .[0] | {
      seq: .sequence,
      source_test: .extra.source.test,
      source_seq: .extra.source.sequence,
      cmd: .command,
      notes: (.notes // "" | gsub("\n"; " ")),
      target: (if (.target | type) == "string" then .target else (.target[0].selector // "?") end),
      value: (.value // "")
    })
  }'
}

for id in "${IDS[@]}"; do
  extract_one "$id" &
  while [ $(jobs -rp | wc -l) -ge 4 ]; do sleep 0.2; done
done
wait
```

### Why the python prefilter is non-optional

GI API responses occasionally contain raw control characters
(newlines, tabs) inside string fields — most commonly inside the
`notes` field of imported util steps. jq's strict JSON parser
rejects these with:

```
parse error: Invalid string: control characters from U+0000 through U+001F must be escaped
```

The failure is intermittent because not every test's result contains
a problem string. The script worked on 2026-05-21; on 2026-05-22 it
errored on the same query against different tests. Piping through
`python3 -c "import sys,json; print(json.dumps(json.loads(sys.stdin.read(), strict=False)))"`
fixes it: Python's parser is lenient about control characters with
`strict=False`, and the round-trip through `json.dumps` re-emits
properly-escaped JSON that jq accepts. Apply this filter to *every*
curl response from the GI API in this pipeline.

Output is one JSON line per test in `/tmp/gi-failures.jsonl`. Each
line has the failing step's selector, source test/seq (util or self),
notes, and command — exactly the inputs the selector-fix workflow
needs to group and act on.

Always use `--compressed` with curl against `api.ghostinspector.com`;
the API returns gzip and jq will error on the bare bytes.

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
