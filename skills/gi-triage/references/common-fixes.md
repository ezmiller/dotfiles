# Common Fix Patterns

Recurring shapes for what to put in the "Action" field of the triage
report. These are templates — fill in the specifics from the actual
test.

## GI test edits

The MCP can't write step edits. Recommendations go to the user, who
applies them in the GI web UI. Keep edit recommendations precise:

```
Step 4 (assertElementVisible, target: .quickshop-button)
  Current target: .quickshop-button
  Proposed target: [data-quickshop-trigger]
  Reason: .quickshop-button was renamed to a data attribute in
          <commit-sha> (sections/product-card.liquid:42). The data
          attribute is more stable across redesigns.
```

### Selector hardening idioms

When recommending a new selector, in priority order:

1. **`data-test-*` / `data-testid` attributes** if the theme defines
   them on the relevant element. Greatest stability.
2. **Semantic ARIA roles + accessible names**, e.g.
   `[role="button"][aria-label="Add to cart"]` — survives class
   renames and is accessibility-positive.
3. **`data-*` attributes meant for JS hooks**, e.g.
   `[data-quickshop-trigger]`, `[data-cart-drawer-open]`. These
   typically have a reason to exist and are less likely to be deleted
   than presentational classes.
4. **Element + nth-child only as a last resort** — fragile to layout
   changes.

Avoid recommending selectors that chain three or more class names —
they break the next time someone reorganizes the SCSS.

### When the assertion is the problem

For text assertions on copy that's translated, point at the
`locales/en.default.json` (or relevant locale file) entry rather than
hardcoding the English string in the test. The cleanest GI test step
quotes the same locale value the theme uses.

For price assertions, prefer regex assertions over exact strings —
`/\\$\\d+\\.\\d{2}/` won't break when the seed product's price changes.

## Theme fixes

When the verdict is "theme regression," the report should name:

- The file(s) involved (sections, snippets, assets/*.js, src/**/*.tsx).
- The relevant commit range (between last green and first red CI runs).
- The specific function/Liquid block / React component.
- A one-paragraph proposed fix — *not* a diff. The user will branch
  via `work-ticket` and write the patch themselves.

### Finding the breaking commit reliably

Always work from `origin/main`, never the local working tree. The
local repo is almost always on a feature branch with a stale base.

```bash
cd ~/Projects/pk-shopify-theme
git fetch origin main

# Recent commits on production:
git log --since=<last-green-date> --format='%h %ad %s' --date=short origin/main

# What changed in a specific file between two points:
git diff <last-known-good-sha>..origin/main -- src/components/SizeList/SizeList.tsx

# What a file looked like before the suspect change vs. now:
git show <sha-before>:src/components/SizeList/SizeList.tsx
git show origin/main:src/components/SizeList/SizeList.tsx
```

For a wide search of "what touched the area lately":

```bash
git log --since=<date> --name-only origin/main -- src/components/ src/containers/ sections/ snippets/ \
  | grep -E '\.(tsx|jsx|liquid|js|scss)$' | sort -u
```

### Class-chain selectors and the "new wrapper" trap

A selector like `.parent > label.child:nth-of-type(N)` depends on
*both* the class list AND the parent-child structure. A common
failure mode: someone wraps the labels in a new `<div>` for layout
reasons, which leaves the label classes untouched but moves them from
direct children to grandchildren. The class chain still matches, but
the `>` combinator now fails.

When a class-chain selector breaks and `git blame` says the class
list hasn't moved, check whether the *parent component* recently
added a wrapper element. Look at the immediate parent in the JSX/Liquid
tree, not just the file the class lives in. For pk-shopify-theme: if
the failing class is in `src/components/X/X.tsx`, check the recent
history of `src/components/XList/`, `src/containers/`, and any
`sections/` that render the component.

Don't speculate about root cause if you can't pin a commit. "Markup
for `.cart-drawer__line-item` changed; commit <sha> is the most
likely cause based on path and timing" is fine. "Probably a race
condition in some JS file" is not.

## QA data drift

When the verdict is "QA data," include:

- What changed (e.g., "product `purple-hippo-rattle` is no longer
  published on QA").
- How to verify (URL on shop-qa.primary.com that should show the
  product).
- What the fix is (re-seed product, restore discount, refill
  inventory).
- Who owns it. If unsure, default to flagging it for Ethan to chase
  with the QA/ops owner; do not silently rewrite the test to target a
  different product unless explicitly approved.

## Viewport-specific notes

The theme uses both `device.mobile` Liquid branches and CSS media
queries. For a test that fails only at one viewport, the likely
locations:

- `sections/header.liquid` / `sections/header-mobile.liquid` — mobile
  nav diverges hard.
- `snippets/product-card.liquid` — quickshop renders differently per
  viewport.
- `assets/cart-drawer.js` — drawer animation timing differs on touch
  devices.
- Any partial with a name containing `-mobile`, `-desktop`, or
  `-tablet`.

## Re-running for confirmation

When you want to verify a current state (e.g., "is this still failing
right now or was it just temporarily broken at 9:05am?"), use:

```
mcp__ghostinspector__execute_test(
  test_id=<id>,
  start_url="https://shop-qa.primary.com",
  viewport="<the viewport that failed in CI>"
)
```

One re-run is usually enough. Three runs is the budget — if it
passes once and fails twice, treat it as flaky (which the harness
should have caught; investigate the harness too).

`execute_on_demand_test` is useful when you want to test a *modified*
test step in isolation (e.g., the proposed new selector) without
editing the persistent test. It needs an org ID — find one via
`mcp__ghostinspector__get_running_tests` or by checking an existing
test's metadata, then construct a minimal steps array containing just
the step you want to verify.
