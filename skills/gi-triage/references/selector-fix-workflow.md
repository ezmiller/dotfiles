# Selector-Fix Workflow

Use this loop when the verdict for a group of failures is "Edit the
GI test." It assumes you've already inventoried the failing tests (see
SKILL.md step 1 + `ci-context.md`) and classified the failure as a
stale selector that needs updating in GI.

The loop has six phases. Do not skip phases. The friction points are
real: edits without backups risk data loss; edits without canary runs
propagate errors silently across the next CI cycle.

## Phase 0 — Setup once for the batch

Before working any group:

```bash
KEY="$GHOST_INSPECTOR_API_KEY"  # already exported in this shell
```

Bulk-inventory the failing-step data for every failing test using the
curl+jq script in `ci-context.md`. Save to `/tmp/gi-failures.jsonl`.

Open a Chrome session via the `chrome-devtools` MCP. The user signs
into `shop-qa.primary.com` past the Shopify password gate once;
subsequent `mcp__chrome-devtools__navigate_page` and
`mcp__chrome-devtools__evaluate_script` calls reuse that session.

## Phase 1 — Group by source step

Each failure record has `extra.source.test` and `extra.source.sequence`.
Group failures by `(source_test, source_seq)` — that's the fix unit.
A single util shared by 5 tests is 1 fix point, not 5. Many failures
will collapse this way; the rest are direct steps in parent tests.

Pipeline:

```bash
jq -s '
  group_by(.fail.source_test + "|" + (.fail.target // "")) |
  map({
    fix_unit_source_test: .[0].fail.source_test,
    fix_unit_source_seq: .[0].fail.source_seq,
    cmd: .[0].fail.cmd,
    target: .[0].fail.target,
    value: .[0].fail.value,
    notes: .[0].fail.notes,
    affected_count: length,
    affected_tests: [.[] | {test_id, name, viewport}]
  }) | sort_by(-.affected_count)' \
  /tmp/gi-failures.jsonl > /tmp/gi-groups.json
```

Present the group summary to the user with counts; work the
highest-leverage groups first (most affected tests per fix).

## Phase 2 — Understand the failing step

For one group:

1. Fetch the source test (or util) via `mcp__ghostinspector__get_test`.
   For a util (importOnly=true), the step list is typically tiny — often
   one or two steps. Read the step `target`, `command`, `value`, and
   `notes` (the notes often say what the step is intended to do, e.g.
   "Click the OOS size for waitlist testing").
2. Identify which parent tests use this util — these are your canary
   candidates for Phase 5.
3. The util's standalone `startUrl` (if present) tells you which QA
   page to inspect. If absent (some utils are launched from any page),
   pick one of the parent tests' running pages from its result payload.

## Phase 3 — Verify against the live DOM

Navigate Chrome to the page the util/step runs on:

```
mcp__chrome-devtools__navigate_page(url=<page_url>)
```

The Chrome instance reuses the user's authenticated session. If the
page hits the password gate, ask the user to log in via the visible
browser — don't try to type the password yourself.

Probe with `evaluate_script` to confirm:

- The old selector matches 0 (sanity-check "the failure is real today").
- The new candidate selectors each match exactly 1 element.
- The matched element is the *right* element (text content, sizeValue
  via the `<input>` inside, classes).

Template:

```javascript
() => {
  const old = '<old selector>';
  const candidates = ['<A>', '<B>', '<C>', '<D>'];
  return {
    oldMatches: document.querySelectorAll(old).length,
    candidates: candidates.map(s => {
      const els = document.querySelectorAll(s);
      return {
        selector: s,
        count: els.length,
        text: els[0]?.textContent.trim().slice(0, 30) || null
      };
    })
  };
}
```

## Phase 4 — Propose candidates, get user confirmation

Show the user a small table (3–4 rows) of selector candidates with
trade-offs. Use this hierarchy, in order of preference:

| Tier | Strategy | When to prefer |
|---|---|---|
| **C — Semantic class** | e.g. `.pw-action-group__item--disabled`, `--selected` | When a state class exists that matches the step's stated intent (the step is "click the disabled size" or "verify the selected swatch"). Most robust. |
| **D — Stable attribute / value** | e.g. `label:has(input[value="12"])` | When intent is "this specific value" rather than "this state." Robust as long as the value stays. |
| **B — Drop class-chain noise** | e.g. `.pw-size-buttons > label.pw-action-group__item:nth-of-type(N)` | When the test really is positional and there's no semantic alternative. Leaner than A. |
| **A — Minimal swap of the breaking parent** | e.g. `.pw-action-group` → `.pw-size-buttons` | Last resort. Smallest diff, but preserves the brittleness of the original. |

Always include in the proposal:
- The exact old target string.
- The exact new target string.
- Why this candidate over the others (one sentence).
- The list of affected tests (so the user sees the blast radius).

Wait for an explicit "go" / "yes" / "apply" — not a clarifying
question, not a screenshot, not a "looks right." If the user is
verifying state in the GI web UI, that's verification, not approval.
Re-ask if ambiguous.

## Phase 5 — Backup + write

Once approved:

```
mcp__ghostinspector__duplicate_test(test_id=<source_test_id>)
```

The duplicate appears in the same suite with `(Copy)` appended to
its name. Note the new test ID — that's your rollback handle. The
copy is referenced by ID, not by name, so it won't get accidentally
imported into other tests.

Then patch via REST:

```bash
TEST_ID=<source_test_id>
NEW_TARGET='<the approved new selector>'
SEQ=<source_seq>  # which step within the test

# Fetch current state
curl -s --compressed "https://api.ghostinspector.com/v1/tests/$TEST_ID/?apiKey=$KEY" \
  | jq '.data' > /tmp/gi-test-before.json

# Patch
jq --arg t "$NEW_TARGET" --argjson s "$SEQ" \
  '.steps[$s].target = $t' \
  /tmp/gi-test-before.json > /tmp/gi-test-after.json

# Show the diff
diff <(jq -S . /tmp/gi-test-before.json) <(jq -S . /tmp/gi-test-after.json)
```

Send only the `steps` field to minimize blast radius. JSON body works:

```bash
jq '{steps}' /tmp/gi-test-after.json > /tmp/gi-patch.json

curl -s --compressed -X POST \
  "https://api.ghostinspector.com/v1/tests/$TEST_ID/?apiKey=$KEY" \
  -H "Content-Type: application/json" \
  --data @/tmp/gi-patch.json | jq '{code, message}'
```

Expect `{"code": "SUCCESS"}`. If JSON body is rejected, fall back to
form-encoded:

```bash
curl -s --compressed -X POST \
  "https://api.ghostinspector.com/v1/tests/$TEST_ID/?apiKey=$KEY" \
  --data-urlencode "steps=$(jq -c '.steps' /tmp/gi-test-after.json)"
```

Re-fetch to verify the target updated:

```bash
curl -s --compressed "https://api.ghostinspector.com/v1/tests/$TEST_ID/?apiKey=$KEY" \
  | jq ".data.steps[$SEQ].target"
```

## Phase 6 — Canary run

Pick one affected parent test (not the util itself — utils with
`importOnly=true` won't return a useful result on standalone runs
because they assume context from a parent test that sets up the page).
Run it at the same viewport that failed in CI:

```
mcp__ghostinspector__execute_test(
  test_id=<parent_test_id>,
  start_url="https://shop-qa.primary.com",
  viewport="375x667",
  immediate=true
)
```

`immediate=true` is required — synchronous calls without it exceed
the MCP timeout. The call returns a result ID; the test runs in GI
for 30–180s depending on step count.

Poll for completion:

```bash
RESULT_ID=<from execute_test response>
for i in $(seq 1 10); do
  sleep 30
  PASS=$(curl -s --compressed "https://api.ghostinspector.com/v1/results/$RESULT_ID/?apiKey=$KEY" \
    | jq -r '.data.passing')
  echo "[poll $i] passing=$PASS"
  [ "$PASS" != "null" ] && [ -n "$PASS" ] && break
done
```

(Use `--compressed`; the GI REST API returns gzip and bare curl
produces a parse error on jq.)

If `passing=true`: the fix propagates. Move on to the next group.
If `passing=false`: read the new failing step. Either:
- It's the same step with a still-broken selector → revise candidate
  (re-do Phase 3 with what you learned).
- It's a *different* downstream step that's also broken → that's a
  separate group; finish this one as "fix applied, but parent test
  still has further issues," log it, move on.

Do not roll back automatically on a single failed canary — the
backup is preserved if the user wants to revert. Tell the user the
canary failed and let them decide.

## Phase 7 — Cleanup (after the whole batch)

When all groups are fixed and all canaries pass, delete the `(Copy)`
backup tests so the GI account stays tidy. The GI MCP has no
`delete_test` tool; either:

- Delete manually in the GI web UI (visible, slow).
- Use the REST API: `curl -X DELETE "https://api.ghostinspector.com/v1/tests/<id>/?apiKey=$KEY"`.

Confirm with the user before bulk-deleting; backups are cheap to keep
for a day or two in case a later CI cycle reveals an issue.

## Per-group output for the user

After each group, summarize:

```markdown
### Group: <util-or-test-name> (<source_test_id>) — N affected tests

- Old selector: `<old>`
- New selector: `<new>` (Tier <A/B/C/D>)
- Backup: `<copy_test_id>` (`<name> (Copy)`)
- Canary: `<parent_test_name>` — PASS (33/33 steps, 117s, viewport 375x667)
- Remaining: <ids and names>
```

This is what the user reads to know what changed and what's still
open. Keep it terse.
