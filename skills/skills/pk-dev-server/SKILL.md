---
name: pk-dev-server
description: >
  Start, authenticate, and monitor the pk-shopify-theme local dev server. Use
  this skill whenever working in pk-shopify-theme and the user asks to start
  dev, run the dev server, spin up local, authenticate with Shopify, fix dev
  server errors, or check on a running dev session. Also trigger when the user
  mentions "yarn serve", "shopify theme dev", webpack HMR not working, a 502
  introspection error, or a rogue node process.
---

# pk-shopify-theme Dev Server

## Overview

The dev environment requires **two concurrent terminals**:

| Terminal | Command | Purpose |
|----------|---------|---------|
| 1 | `yarn serve` | webpack-dev-server (HMR, asset compilation) |
| 2 | `shopify theme dev` | Syncs theme files to Shopify, provides preview URL |

Both must be running simultaneously. The preview URL comes from terminal 2.

---

## Important: How to invoke Shopify CLI

**Always use `yarn shopify` — never bare `shopify`.** The CLI is a local project dependency, not a global install. All commands below use `yarn shopify` accordingly.

## Prerequisites (one-time)

### SSL certs (required for `yarn serve`)

```bash
brew install mkcert
mkdir -p .certs
mkcert -key-file .certs/localhost-key.pem -cert-file .certs/localhost.pem localhost
mkcert -install  # trust the cert system-wide
```

---

## Starting the Dev Server

When starting from ECA, both processes are backgrounded so control returns
immediately. Logs go to `/tmp/pk-serve.log` and `/tmp/pk-theme-dev.log`.

### Step 0 — Check for existing processes

Before doing anything, check what's already running:

```bash
pgrep -fl "webpack serve\|shopify theme dev"
lsof -i :9292 2>/dev/null | grep LISTEN
```

Then decode and tail the logs to see if they're healthy or stale. The logs contain ANSI escape codes, so always strip them before reading:

```bash
perl -pe 's/\x1b\[[0-9;?]*[mGKHJlh]//g; s/\r//g' /tmp/pk-serve.log 2>/dev/null | tail -5
perl -pe 's/\x1b\[[0-9;?]*[mGKHJlh]//g; s/\r//g' /tmp/pk-theme-dev.log 2>/dev/null | tail -5
```

> Never use bare `tail` or `grep` on these logs — they're binary-encoded due to ANSI sequences and will return `Binary file matches` with no usable content. Always pipe through the `perl` strip first.

**Interpret the results:**

- Both processes alive **and** logs show recent healthy output (`compiled`, `Syncing theme`, preview URL) → **already healthy**, extract and report the preview URL to user (see URL extraction below); **stop here**.
- Port 9292 is listening but no matching process in `pgrep` (or log shows an error) → **stale/broken run**, kill and restart.
- Processes running but logs show `EADDRINUSE` or crash → **port conflict from a previous probe**, kill and restart.

**Kill all related processes before restarting:**

```bash
pkill -f "webpack serve" 2>/dev/null
pkill -f "shopify theme dev" 2>/dev/null
pkill -f "node.*shopify" 2>/dev/null
sleep 2  # let ports release
```

Then proceed to Step 1.

> **Why this matters:** The Step 1 auth probe starts a real `shopify theme dev`
> process that binds port 9292. If not killed before Step 3, the new process
> crashes with `EADDRINUSE`. Always clean up before starting fresh.

### Step 1 — Check Shopify auth

Auth tokens are stored in the OS keychain — there's no file to inspect directly.
The only reliable check is to probe `yarn shopify theme dev` briefly. Use this
macOS-compatible pattern (no `timeout` command on macOS):

```bash
( yarn shopify theme dev --store connors-bone-yard & PID=$! ; sleep 8 ; kill $PID 2>/dev/null ; wait $PID 2>/dev/null ) 2>&1 | head -40
```

**Interpret the output:**
- Contains `Syncing theme` or `http://127.0.0.1:9292` → **already authenticated**, kill the probe's leftover process (`pkill -f "shopify theme dev"`), then proceed to Step 2
- Contains `Log in` or `browser` or `https://accounts.shopify.com` → **auth required**, kill the probe, skip Step 1a, and proceed to Step 2. The background process in Step 3 will prompt for auth in its log — have the user authenticate through that instead to avoid double auth.
- Contains `command not found` or the probe itself errors → **stop and tell the user** something is wrong with the environment; do not keep trying variants

> **Important:** The probe starts a real process that binds port 9292. Always kill it before Step 3:
> ```bash
> pkill -f "shopify theme dev" 2>/dev/null; sleep 1
> ```

#### Step 1a — Auth required (run in foreground)

Tell the user auth is needed and ask them to complete the browser flow:

```bash
yarn shopify theme dev --store https://connors-bone-yard.myshopify.com
```

Wait for the user to confirm they've authenticated and seen the preview URL,
then `Ctrl-C` and proceed to Step 2.

### Step 2 — Start webpack in background

```bash
nohup yarn serve > /tmp/pk-serve.log 2>&1 &
echo "webpack PID: $!"
```

### Step 3 — Start Shopify theme dev in background

```bash
nohup yarn shopify theme dev --store connors-bone-yard > /tmp/pk-theme-dev.log 2>&1 &
echo "shopify theme dev PID: $!"
```

> **Note:** The background process may prompt for auth again independently of
> the foreground auth flow. Check the log after ~10s — if it shows a new
> verification code/URL, have the user authenticate once more, then it will
> proceed automatically.

### Step 4 — Confirm both are healthy

Wait ~10 seconds, then check (always strip ANSI first):

```bash
perl -pe 's/\x1b\[[0-9;?]*[mGKHJlh]//g; s/\r//g' /tmp/pk-serve.log | grep -E "compiled|error|Error" | tail -5
perl -pe 's/\x1b\[[0-9;?]*[mGKHJlh]//g; s/\r//g' /tmp/pk-theme-dev.log | grep -E "127\.0\.0\.1|Syncing|error|Error" | tail -5
```

Then extract and **always report** the preview URLs to the user — both the local and Shopify preview:

```bash
perl -pe 's/\x1b\[[0-9;?]*[mGKHJlh]//g; s/\r//g' /tmp/pk-theme-dev.log \
  | grep -oE "https?://[a-zA-Z0-9./?=&_:-]+" | grep -v "^https://cdn\|^https://fonts" | sort -u
```

The user needs these URLs every time — don't skip this step even if everything looks healthy. Look for:
- `http://127.0.0.1:9292` — local preview
- `https://connors-bone-yard.myshopify.com/?preview_theme_id=...` — Shopify preview

### Stopping the servers

```bash
pkill -f "webpack serve"
pkill -f "shopify theme dev"  # matches the yarn shopify process too
```

---

## Re-authenticating (token expired or stale)

If the auth probe (Step 1) shows a 502 introspection error:

```bash
yarn shopify auth logout
```

Then run `yarn shopify theme dev` in the foreground to re-authenticate (Step 1a above),
and restart backgrounded afterward.

---

## Monitoring & Common Problems

### Rogue node process (asset timeouts)

**Symptom:** webpack appears stopped but theme JS/CSS returns timeouts.

**Fix:**

```bash
# find the stale process
lsof -i :8080

# kill it
kill <PID>
```

Then restart `yarn serve`. As a nuclear option: restart your machine.

### TypeScript errors in the build

`yarn serve` runs webpack with `fork-ts-checker-webpack-plugin`, which surfaces
type errors as **webpack warnings** — the build still completes and HMR still
works, but the errors appear in terminal 1's output.

To see all type errors clearly, or to check without running the dev server:

```bash
yarn typecheck:build
```

Fix reported errors in source, save, and webpack will recompile automatically.
Type errors here are a common source of CI failures even when the local dev
server appears to be working fine.

### webpack won't compile

- If port is already in use: `lsof -i :8080` and kill the occupant.

### Theme changes not syncing

- Check terminal 2 for auth errors — if present, re-authenticate (see above).
- Try stopping and restarting `yarn shopify theme dev`.

### After `shopify theme pull`

If you pulled the `main` theme to get fresh JSON/locale files, Shopify CLI may
have changed `templates/index.json`, `sections/header-group.json`, or
`locales/en.default.json`. Add those back to `.shopifyignore` before committing
and checkout any unintended changes.

---

## Env File Notes

- `.env` → production defaults
- `.env.development` → overrides for local dev (used automatically by `yarn serve`)

Sensitive keys live in 1Password / are never committed.
