# Failure Classification

The Ghost Inspector result payload (`mcp__ghostinspector__get_test_result`)
contains per-step `passing`, `error`, `command`, `target`, `value`,
`url` (the page URL at the time of the step), `notes`, and
`extra.source.test` / `extra.source.sequence` (the original
test+step where the step was authored — useful for finding imported
utils). There is *one* top-level screenshot at
`.screenshot.original.defaultUrl` showing the final page state; GI
does **not** store per-step screenshots. The result also has `console`
(an array of console messages) and `urls` (visited URLs).

For batches of more than ~3 tests, fetch results via the REST API
rather than the MCP — see `ci-context.md`. The MCP spills to disk on
any non-trivial result; jq directly is faster.

Classification is mostly about reading the error string carefully and
correlating it to recent theme changes. Always filter out
`optional: true` steps before identifying the real blocker — they
fail without breaking the run and chasing them wastes time.

## Categories

### 1. Selector not found / not visible

**Error patterns:**
- `Could not find element matching <selector>`
- `Element is not visible`
- `waitForElementVisible timed out`

**Likely cause:** The theme markup changed and the selector no longer
matches. Either the CSS class/data attribute was renamed, the element
was moved into a Shadow DOM/iframe, or it's hidden by new CSS.

**How to confirm:**
- Use the chrome-devtools MCP to open the live page (`url` field from
  the failing step) in the user's authenticated Chrome session, then
  `evaluate_script` to run `document.querySelectorAll(<selector>)`.
  Zero matches confirms the selector is broken right now. Inspect the
  parent element's children to see what selector *would* match the
  intended element.
- Grep the failing selector against `origin/main` of pk-shopify-theme
  (see `common-fixes.md` for the fetch + grep pattern). If you get
  zero matches the selector is stale; if you get matches, the markup
  still uses that class but something structural changed (new wrapper
  element inserted, sibling order changed, etc.).
- Check the `extra.source.test` on the failing step — if it's
  different from the parent test ID, the step is imported from a
  util. The fix may need to land in the util, which is the unit other
  parent tests share.

**Fix path:**
- If selector is stale → use the selector-fix workflow in
  `selector-fix-workflow.md`. Apply via REST API after explicit
  per-group user confirmation.
- If markup is broken → it's a theme regression. Locate the
  responsible commit, name the file/line. Hand to the user.

### 2. Assertion mismatch

**Error patterns:**
- `Expected text "X" not found in element`
- `Expected URL to match "X" but was "Y"`
- `Expected element count to be N but was M`

**Likely cause:** Three flavors. The page text/URL/count changed
intentionally (copy update, route change, new feature added) and the
assertion is stale. OR a real regression broke the rendered output. OR
QA seed data drifted (a product was renamed, a collection was emptied).

**How to confirm:**
- Compare the assertion value to what's actually on the page (screenshot).
- If the test asserts a product title or SKU, check QA: does the
  product still exist? Has its title been edited?
- If the assertion is on copy ("Add to cart", price format, etc.),
  grep `~/Projects/pk-shopify-theme/locales/` and `sections/` for the
  expected vs actual strings.

**Fix path:**
- Stale assertion → GI test edit, update the expected value.
- Real regression → theme fix.
- QA data drift → flag for re-seed; do not change the test to match
  bad QA data. If the data drift was intentional (product
  discontinued), then the test needs to point at a different product.

### 3. Timeout waiting for navigation / network

**Error patterns:**
- `Timed out waiting for page load`
- `Navigation timeout exceeded`
- Step takes >30s and times out

**Likely cause:** Either QA env latency (rare but real), or a JS error
blocked rendering, or a network request hung (often Shopify API or a
third-party script).

**How to confirm:**
- Check the `console` and `networkRequests` arrays in the result
  payload — `get_test_result` includes them. A red 500 or a JS
  uncaught error there is the smoking gun.
- Re-run with `mcp__ghostinspector__execute_test`. If it passes on
  retry, it was transient — note it but don't change anything.

**Fix path:**
- Transient → no action; if a pattern emerges across runs, file an
  ops ticket for QA infra.
- JS error → theme regression; find the script.
- Hung third-party → consider mocking or removing the dependency from
  the test path.

### 4. Step depends on prior state (cart, session, fixtures)

**Error patterns:**
- Steps 1–N pass, then a "Could not find element" or assertion fail at
  a step that assumes a product is in the cart, user is logged in,
  inventory is non-zero, etc.

**Likely cause:** A previous step silently failed to mutate state.
Common case: "Add to cart" step succeeded visually but the line item
didn't actually persist (drawer animation finished before XHR
completed). Or a QA seed product is now out of stock.

**How to confirm:**
- Re-read the step list. Identify which earlier step established the
  state the failing step depends on.
- Check the screenshot at the failing step — is the cart actually
  empty, or does the assertion just not match?
- If the test uses an `extract` or `eval` step earlier, the extracted
  value might be wrong; the result payload shows extracted values.

**Fix path:**
- If a setup step is racy → GI test edit adding a `waitForElement` or
  `assertElementPresent` between the mutation and the dependent step.
- If a seed product is broken → QA data issue.

### 5. Single-viewport failure (mobile-only or desktop-only)

**Pattern:** The same test ID fails at `375x667` but passes at
`1280x800` (or vice versa).

**Likely cause:** The theme's mobile and desktop partials diverge.
Common offenders: mobile nav drawer, mobile filter UI, sticky CTA,
mobile-only quickshop overlay.

**How to confirm:**
- Look at the failing step's selector — is it a mobile-only element
  (e.g. `.menu-drawer-toggle`, `.mobile-filters`)?
- In the theme: search for `{%- if device.mobile -%}` or media-query
  rules near the suspect markup.

**Fix path:**
- Same as the relevant category above (selector, assertion, etc.) —
  but the fix likely touches only the mobile or desktop branch.

### 6. Truly transient

**Signs:**
- Failure passes on a single manual retry, with no obvious
  environmental change.
- Different steps fail across the 4 CI attempts (the failure isn't
  reproducibly at one step).
- Console shows network blips, not application errors.

**Fix path:** Note it in the report ("appears transient, no action").
If this test shows up transient repeatedly across multiple days, it's
no longer transient — promote to "stabilize this test" work.

## Decision shortcut

If you're unsure between two categories:

1. Open the failing step's screenshot.
2. Ask: "Is the page rendering correctly to a human eye?"
3. If yes → the test is wrong (category 1 stale selector or 2 stale
   assertion). Recommend a GI test edit.
4. If no → the theme is wrong. Find the commit.
