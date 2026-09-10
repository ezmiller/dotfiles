# botserver (ThinkCentre M75q Gen 2)

**Primary server.** Runs the OpenClaw and hermes agent gateways plus supporting services.

### Connection
- `ssh ethan@192.168.86.122` (LAN) or `ssh ethan@100.117.184.4` (Tailscale) — admin, sudo
- `ssh openclaw@192.168.86.122` — for user services (systemctl --user). Must SSH directly, `su -` doesn't get a systemd session.
- ⚠️ The LAN IP is **DHCP and has drifted** (was `.36`, now `.122` as of 2026-08-03).
  Confirm with `tailscale netcheck` (logs `self=<ip>`) or `ip -4 addr show enp1s0`
  before trusting either number — and remember `SRC=<that IP>` in EGRESS_DENIED
  logs is botserver's own traffic, not another machine's.

### NixOS Config
- **Repo:** `~/Projects/botserver-nix` (private, github.com/ezmiller/botserver-nix)
- **On server:** `/etc/nixos` (git clone of repo)
- **Apply changes:** `cd /etc/nixos && sudo git pull && sudo nixos-rebuild switch`
- **Edit secrets:** `sudo sops /etc/nixos/secrets/botserver.yaml`

### OpenClaw (v2026.7.2-beta.7)

> **On the beta line deliberately** (upgraded 2026-08-08 from 6.11). The "see attached
> image" placeholder fix landed ONLY in the 7.2 beta — neither `latest` (2026.7.1-2) nor
> `extended-stable` (2026.6.34) has it. Pin the **exact** version; never track the `beta`
> dist-tag. Full record: `~/.tracking/openclaw-upgrade-2026.7.2-beta.7.md`.
>
> ⚠️ **Node floor.** 2026.7.2-beta.x requires node `>=22.22.3`, and the main nixpkgs pin
> ships 22.22.2. `nodejs_22` therefore comes from the **`nixpkgs-agents`** pin via the
> overlay in `flake.nix` (22.23.2). Don't "tidy" it back onto the main pin — the gateway
> will crash-loop on a version check.
>
> ⚠️ **Upgrade ordering.** Run `openclaw gateway install --force` **before**
> `openclaw doctor --fix`. Doctor restarts the gateway when it finishes, and a stale unit
> still hardcodes the old node store path → 5 crash-loops and a systemd start-limit hit.
> Harmless but alarming. Use `--fix`, never `--force` (the latter overwrites the drop-ins).
- **Status:** `ssh openclaw@botserver systemctl --user status openclaw-gateway`
- **Restart:** `ssh openclaw@botserver systemctl --user restart openclaw-gateway`
- **Logs:** `ssh openclaw@botserver journalctl --user -u openclaw-gateway --since "1 hour ago" --no-pager`
- **Config:** `~openclaw/.openclaw/openclaw.json`
- **Install:** `~openclaw/.local/opt/openclaw` (built from source)
- **Secrets:** sops-nix decrypts to `/run/secrets/openclaw.env` at boot

#### "I switched the model but it says it fell back" (diagnosed 2026-08-11)

Four separate things cause this and they look identical from chat. Check in this order.

**1. Is the config you edited even the one in effect?** `agents.entries.<agent>.model`
**overrides** `agents.defaults.model`. All three agents (`main`, `hope`, `thoth`) had their
own identical blocks, so editing `defaults` did nothing for five days. Check both:

```
jq -c '.agents.defaults.model' ~/.openclaw/openclaw.json
jq -r '.agents.entries | to_entries[] | "\(.key): \((.value.model // "inherits")|tojson)"' ~/.openclaw/openclaw.json
```

**2. Is the auth profile blocked?** This is the one that hides. OpenClaw records provider
rate limits locally and refuses the model **without calling the provider** until the block
expires — instantly, with no 401/429 in the log, so it looks like a config fault:

```
node -e 'const{DatabaseSync}=require("node:sqlite");
const db=new DatabaseSync(process.env.HOME+"/.openclaw/agents/main/agent/openclaw-agent.sqlite",{readOnly:true});
console.log(db.prepare("SELECT state_json FROM auth_profile_state WHERE state_key=?").get("primary").state_json)'
```

Look for `usageStats.<profile>.blockedUntil` and `blockedReason`. `subscription_limit` is
**normal** on the cheaper Codex plan and the block can last days — Aug 7 → Aug 12 once.
The tell in the journal is a `candidate_failed` with `detail=Auth profile … is temporarily
unavailable` and a sub-second duration. Re-authorising in the dashboard clears `usageStats`
outright rather than waiting.

**3. Is the model routed through the Codex app-server?** If so it needs *three* things this
box does not have by default, and it fails before the network every time:

- `tools.exec.mode` must not be `deny` or `allowlist` — that is a hard mode check
  (`assertCodexAppServerAllowedForOpenClawExecMode`), the allowlist contents are never read
- `agents/main/agent/codex-home/auth.json` must exist — it does not, since 2026-07-03
- **IPv6 egress must work — it does not.** No v6 default route, but DNS returns AAAA.
  Node's fetch falls back to v4 ("sticky IPv4-only dispatcher" in the logs); the Rust
  app-server does not, and dies with `ENETUNREACH` on `wss://chatgpt.com/…`
  **This is a host-level fault, not a Codex one** — see the box-wide note below.

Models on the plain transport (`openai-transport … /backend-api/codex/responses`) need none
of this. Prefer them unless you specifically want app-server behaviour.

**4. Did onboarding change things you did not ask it to?** Re-authorising via the dashboard
on 2026-08-11 also wrote `plugins.enabled = false` (a global kill switch — every channel
reported "unconfigured" while the tokens sat untouched in the file) and rewrote
`agents.defaults.model.primary`. After any `onboard`, diff against a backup.

**A trailing comma makes the gateway silently run the previous config.** Hand-edits to
`openclaw.json` should be followed by `jq . openclaw.json > /dev/null`. Invalid JSON does
not crash anything; it just means what you think you changed is not live.

#### Upgrading OpenClaw (procedure proven 2026-08-08, 6.11 → 7.2-beta.7)

Install is **imperative**: git checkout a tag + `pnpm build` → `dist/`. Nix does not pin the
version. Past write-ups: `~/.tracking/openclaw-upgrade-*.md` — read the most recent one first,
each upgrade has left a distinct trap behind.

```
0. Node floor  — check target's engines.node vs `node --version`. If short, bump via the
                 nixpkgs-agents overlay in flake.nix, NOT the main pin.
1. Baseline    — `doctor --lint` (NOT --dry-run, doesn't exist) and save the output, so
                 afterwards you can tell new breakage from pre-existing findings.
2. Backup      — STOP the gateway first, then `openclaw backup create --output <dir> --verify`.
                 ⚠️ Always pass --output: the default writes INSIDE the checkout, which
                 step 4 rebuilds. Also tar `dist dist-runtime` for a rebuild-free rollback.
3. Checkout    — `git checkout <tag>`; wipe `dist dist-runtime .artifacts/tsgo-cache`
                 (incremental builds have shipped stale hashed chunks).
4. Build       — `pnpm install --frozen-lockfile` then `pnpm build` (~7 min). pnpm
                 self-provisions the version in `packageManager`; no nix change needed.
5. Unit FIRST  — `openclaw gateway install --force`, THEN `doctor --fix`.
6. Verify      — version, `models auth list`, channels, EGRESS_DENIED, and the actual
                 behaviour you upgraded for.
```

⚠️ **Ordering matters.** `doctor --fix` restarts the gateway when it finishes. If the unit
still hardcodes the old node store path, every start dies on the version check and systemd
hits its start-limit after 5 tries — alarming, harmless, and avoided by regenerating the
unit first. Use `doctor --fix`, **never** `doctor --force` (overwrites custom service config,
i.e. your drop-ins). `doctor --non-interactive` alone only runs "safe" migrations and will
just tell you to run `--fix`.

⚠️ **Agent DB migrations are a hard gate, not advice.** New versions refuse to use agent
SQLite stores until persisted media is migrated
(`OpenClawAgentDatabaseMediaMigrationRequiredError: ... uses schema version 1`). Until
`--fix` runs, config validation fails hard on legacy keys and most doctor checks are skipped.

#### No IPv6 route (host-level — bites more than Codex)

botserver has IPv6 *addresses* but **no IPv6 default route**, so nothing off the LAN is
reachable over v6. IPv6 is not disabled (`disable_ipv6 = 0`); `accept_ra = 0` on `enp1s0`
and nothing in `.nix` sets it. Node 22's Happy Eyeballs never checks the routing table, so
it tries AAAA anyway → `ENETUNREACH` / connect timeout. Usually v4 wins the race; when it
doesn't, the request just fails.

Known victims: the Codex app-server (above), and **Telegram slash-command replies**
(2026-08-18) — those go to the DM, which needs fresh connections, while group sends
survive on warm pooled v4 ones. OpenClaw's sticky-IPv4 workaround fixes it until its
**recovery probe re-enables v6**, which is why the symptom comes and goes.

⚠️ `curl -6` fails in 12ms here and will fool you into ruling IPv6 out — undici behaves
differently. Trust the `codes=…ENETUNREACH` in the fetch-fallback log line instead, which
needs `diagnostics.flags = ["telegram.http"]`.

⚠️ Writing **any** `diagnostics` key to `openclaw.json` forces a gateway restart — the
hot-reload watcher treats it as restart-requiring. Do not use it to capture live state.

Full write-up, fixes on deck, and an upstream bug: `~/.tracking/botserver-ipv6-telegram.md`.

#### Agents

| Agent | Channel | Workspace |
|-------|---------|-----------|
| main (KingKong) | Telegram + Discord | ~/kingkong |
| hope | Telegram | ~/hope-bot |
| thoth | Telegram (capture) | ~/thoth-bot |

> **Don't record the models here.** Ethan changes the fallback chain often (minimax,
> deepseek, others). Read it live instead — `jq '.agents.defaults.model'` plus the
> per-agent `.agents.entries[].model` overrides, per the four-step check above.

> The **WhatsApp** channel here is **disabled** (`channels.whatsapp.enabled=false`); the
> old `family` WhatsApp agent was retired 2026-07-20 and now runs under **hermes** (below).

#### Cron Jobs

| Job | Agent | Schedule | Notes |
|-----|-------|----------|-------|
| health-checkin-afternoon | main | 1:30pm ET | Telegram message |
| health-checkin-evening | main | 9:30pm ET | Telegram message |
| thoth-daily-feed-check | thoth | 10am ET | RSS feed check |
| email-check-redfin | main | 8am/8pm ET | ProtonMail via hydroxide |

#### User Services (agent-managed, under openclaw user)

| Service | Port | Runtime | Status command |
|---------|------|---------|---------------|
| family-board | 3456 | Node.js | ⚠️ **NOT a systemd unit** — `systemctl --user status family-board` says "unit could not be found" while the app is serving fine. Check with `curl -sI http://127.0.0.1:3456/` instead (corrected 2026-08-08). |
| hydroxide | 8081 | Go | `ssh openclaw@botserver systemctl --user status hydroxide` |
| rengine | 8888 | Babashka | `ssh openclaw@botserver systemctl --user status rengine` |

### Friendly links (Caddy reverse proxy + internal CA)

Internal services answer to friendly hostnames over HTTPS. Caddy owns `:443` directly on
both the Tailscale IP and the LAN IP, with its own internal CA (`tls internal`);
`tailscale serve` is no longer used. Declared in `botserver-nix` `modules/proxy.nix`.
Design notes: `docs/internal-dns-tls.md`.

| Friendly link | Proxies to | Notes |
|---|---|---|
| `openclaw.dashboard` | `127.0.0.1:18789` | OpenClaw Control UI — **needs a token login**, see below |
| `hermes.dashboard` | `127.0.0.1:9119` | needs `header_up Host localhost` + `Origin` rewrite or the chat WebSocket fails |
| `family.board` | `127.0.0.1:3456` | |
| `botserver.tail86a93e.ts.net` | `:8091` (`/api/*`) + `:3001` | Multica — kept at this name so its baked-in COOKIE_DOMAIN/CORS keep working |

Each link needs **three** independent layers — resolution, proxy, and app auth:

1. **DNS:** a rewrite in AdGuard on farsika (`dns.rewrites` in `AdGuardHome.yaml`) →
   `100.117.184.4`. ⚠️ These live **only** in that file, which is a manual install in no
   git repo — rebuilding farsika loses every friendly name at once. The laptop also keeps
   `/etc/hosts` fallbacks, which matter because its Tailscale has MagicDNS **off** —
   without a hosts entry a name resolves **only** on the home LAN.
2. **Caddy vhost** in `modules/proxy.nix`.
3. **App auth** — separate, and the layer that most looks like a network fault.

> **⚠️ The ephemeral-route trap (cost a session 2026-08-04).** A route pushed into Caddy's
> **admin API** (`localhost:2019`) works instantly and looks perfectly healthy, but is
> **absent from `/etc/caddy/caddy_config`** — so the next `reload caddy` /
> `nixos-rebuild switch` / reboot silently deletes it, with nothing pointing at the cause.
> `openclaw.dashboard` had lived that way since ~Jul 21.
> **Always diff live vs file:** `curl -s localhost:2019/config/` against
> `grep '^<host> {' /etc/caddy/caddy_config`. In the first but not the second = ephemeral;
> declare it in `proxy.nix`. If one friendly link mysteriously dies, check this first.

#### Debugging a broken friendly link

Every vhost writes its own access log at `/var/log/caddy/access-<host>.log` (root-only).
The decisive first question is whether the **browser's** request even arrived:

```bash
ssh ethan@botserver 'sudo grep -c Mozilla /var/log/caddy/access-openclaw.dashboard.log; \
  sudo grep -c "curl/" /var/log/caddy/access-openclaw.dashboard.log'
```

- **No `Mozilla` entries** → the request never reached Caddy: DNS, a stale service worker
  (these UIs are PWAs and ship `sw.js`), or QUIC — see the HTTP/3 trap immediately below.
- **`200`s plus a `101` WebSocket upgrade** → proxy and TLS are fine; the fault is app-level
  auth. Look for a `401`.

> **⚠️ `ERR_SOCKET_NOT_CONNECTED` = the HTTP/3 trap (diagnosed 2026-08-04, now fixed).**
> Caddy used to stamp `Alt-Svc: h3=":443"; ma=2592000` on every response, but **QUIC has
> never once completed over this tailnet** — `grep -c "HTTP/3.0"` across all six vhost access
> logs (~57k requests) returned **0**. Chrome cached that 30-day promise, tried UDP 443 on a
> later visit, got nothing, and hard-failed. Because the connection never completes there is
> **no server-side log line at all**, so it masquerades as broken DNS or a missing vhost.
> It looks *intermittent* because Chrome marks QUIC broken after a failure, uses TCP for a
> while, then retries.
> **Fix (in `modules/proxy.nix`):** `globalConfig` restricts `protocols h1 h2`, so no
> Alt-Svc is advertised and the UDP :443 listener is gone. Each vhost additionally sends
> `header Alt-Svc clear`, because merely *omitting* the header does **not** evict an
> already-cached mapping — only the explicit `clear` value does (RFC 7838 §3.1). Those four
> `clear` lines are removable after 2026-09-03.
> **Note a hard reload does NOT fix this** — the alt-svc mapping lives in the network stack,
> not the page cache. Verify with `curl -sD - https://<host>/ | grep -i alt-svc` (expect
> `clear`, never `h3=`) and `sudo ss -ulnp | grep :443` (expect nothing).

#### OpenClaw Control UI auth

`gateway.bind = loopback`, `gateway.auth.mode = token`, `gateway.auth.token` (48 chars) in
`~openclaw/.openclaw/openclaw.json`. A **`401` on `/control-ui-config.json` is normal when
unauthenticated** — it returns 401 even over plain loopback, so it is *not* a proxy bug; the
UI renders an "Auth required" screen. Only `Authorization: Bearer <token>` is accepted
(`?token=` and `?access_token=` both 401).

```bash
# print the gateway token (run as the openclaw user)
ssh openclaw@botserver 'python3 -c "import json;print(json.load(open(\"/home/openclaw/.openclaw/openclaw.json\"))[\"gateway\"][\"auth\"][\"token\"])"'
```

Paste it into the Gateway Token field (the WebSocket URL self-fills as
`wss://openclaw.dashboard`) and press Connect. **This is a one-time handshake per
browser+hostname:** the raw token goes to sessionStorage (per-tab, discarded), but the UI
exchanges it for a durable operator device credential in localStorage
(`openclaw.device.auth.v1` + keypair in `openclaw-device-identity-v1`), with no expiry. A
fresh tab then connects with no prompt. Re-login is only needed after clearing site data,
on a new browser profile or device, or if the gateway token is rotated.

### Google Workspace CLI (gws)

Shared by both the openclaw and hermes agents (scopes: Docs, Sheets, Tasks, Calendar, userinfo).

- **Install:** declarative + pinned in `botserver-nix` `modules/gws.nix` — fetches the
  upstream GitHub release ELF (`google-workspace-cli-x86_64-unknown-linux-gnu.tar.gz` for
  `v<version>`), autoPatchelf's it (self-contained, no nix-ld dependency), installs it
  system-wide at `/run/current-system/sw/bin/gws`. This replaced the old per-user
  `npm install -g @googleworkspace/cli` + PATH shim. **Upgrade:** bump `version` + `hash`
  in `modules/gws.nix`, then `nixos-rebuild switch` (a build prints the expected sha256 on
  mismatch; upstream also ships a matching `<artifact>.sha256`).
- **Per-user config:** `~/.config/gws/` (owned by each user; created by home-manager
  activation in `home/{openclaw,hermes}.nix`). `client_secret.json` → the sops-decrypted
  OAuth *client*; each user holds its OWN grant (`credentials.enc` + `token_cache.json`),
  so neither agent rotates the other's refresh token.
- **Secret:** ONE encrypted value `gws-credentials-json` in `secrets/botserver.yaml`,
  decrypted twice (per owner) by `sops.secrets` in `modules/secrets.nix` →
  `/run/secrets/gws-credentials.json` (openclaw) and `…-hermes.json` (hermes, which sets
  `key = "gws-credentials-json"` to reuse the same value). No duplicate secret to sync.
- **Check auth:** `ssh <user>@botserver gws auth status` — `auth_method: oauth2` = good;
  `none` = needs a grant. Quick API smoke test: `gws tasks tasklists list`.
- **⚠️ Headless `gws auth` gotcha:** the flow opens a loopback callback on
  `botserver:<port>`, but the browser redirect targets *your* machine's `localhost` and
  can't reach it. Two fixes: (a) `ssh -L <port>:localhost:<port> <user>@botserver` before
  running, so the redirect tunnels through; or (b) let the browser fail, copy the full
  redirect URL (with `?code=…&scope=…`), and `curl` it **on botserver** while `gws auth`
  is still waiting — the callback server returns "Success" and the flow completes.

### Multica (self-hosted tracker + agent daemon)

Two halves: the **server** (three containers, `modules/multica.nix`) and the **daemon**
(`multicad` user, `modules/multicad.nix` + `home/multicad.nix`) that runs Claude/Codex
agents. Three things are pinned *independently* — know which one you're looking at:

| Thing | Pinned in | As of 2026-08-07 |
|---|---|---|
| `claude-code`, `codex` | `nixpkgs-agents` flake input + overlay in `flake.nix` | 2.1.222 / 0.146.0 |
| `multica` CLI | `version` + sha256 in `modules/multicad.nix` | 0.3.17 |
| server containers | `version` in `modules/multica.nix` | 0.3.17 |

> **⚠️ The dashboard's "Update" button can never work here (cost a detour 2026-08-07).**
> Runtime → Diagnostics shows `CLI Version: 0.3.17 → v0.4.21` with an Update button. It
> always errors, and there is nothing to fix: every binary it wants to replace lives in the
> **read-only Nix store**, and nixpkgs builds the `claude` wrapper with
> `DISABLE_AUTOUPDATER=1` besides. Also note that panel means the **`multica` CLI** — *not*
> Claude Code. Misreading it sends you upgrading the wrong package.

**Bumping the agent CLIs** — `nix flake update nixpkgs-agents`, **never** a bare
`nix flake update` (that moves the whole system off its main pin: Caddy, dnsmasq, Docker,
Postgres). Then rebuild, then restart the daemon so it re-registers versions:

```bash
# check nothing is mid-run first — the restart kills in-flight sessions
ssh ethan@botserver 'pgrep -u multicad -a -f "claude|codex" | grep -v "daemon start"'
ssh ethan@botserver 'sudo -u multicad XDG_RUNTIME_DIR=/run/user/$(id -u multicad) \
  systemctl --user restart multica-daemon'
# confirm: look for `agent version detected … name=claude version="…"`
```

Daemon logs are drowned in `heartbeat:` lines — always `grep -v "heartbeat:"` first.

**The CLI and the server must move together.** Never bump the CLI alone; that skews it
against the backend. Staged procedure (snapshot → bump → verify migrations → smoke test)
lives in the repo at `docs/multica-upgrade-2026.5.md`. The 0.3.17 → 0.4.21 jump was
**deliberately deferred 2026-08-07**: ~115 migrations with a visibly messy history
(prefix collisions, renumberings, a self-host backfill blocker), against no exploitable
issues on a private tailnet-only instance. Only real cost of waiting: Opus 5 is missing
from the runtime catalog (upstream added it in v0.4.11).

**⚠️ Known-broken: PK repo cache fetches.** `pk-shopify-theme`, `pk-workers-monorepo` and
`pk-skills` are registered with **HTTPS** URLs, but `multicad` only holds an SSH key
(`~/.ssh/id_ed25519_ezmiller`). Every fetch dies with `could not read Username for
'https://github.com'` and the daemon logs `agent will see possibly stale code` — agents
then work against an old checkout with no visible failure. Ongoing since ≥2026-08-04.
Fix: SSH URLs in the workspace config, or `url.<ssh>.insteadOf` in multicad's gitconfig.

### hermes (agent gateway)

Separate agent gateway under its own **`hermes`** user (uid 1003) — a different codebase
from OpenClaw. Runs the **WhatsApp** agent (**Saul**) that replaced the retired openclaw
`family` bot. Model: `openai-codex/gpt-5.4`.

- **Connection:** `ssh hermes@botserver` directly for `systemctl --user` (same rule as
  openclaw — `su -`/`sudo -u` don't get a systemd session). `ethan` (sudo) can read state
  but the config dir is `0700 hermes`.
- **Services (user):** `hermes-gateway.service` (WhatsApp + cron scheduler),
  `hermes-dashboard.service` (web UI on `127.0.0.1:9119`).
  - Status/restart: `ssh hermes@botserver systemctl --user status|restart hermes-gateway.service`
  - Logs: `ssh hermes@botserver journalctl --user -u hermes-gateway.service -n 50 --no-pager`
- **Config dir:** `/home/hermes/.hermes/` — `config.yaml` (agent, channels, toolsets),
  `.env` (secrets). Both `0600`; edit as the `hermes` user. Config is versioned by
  timestamped `.bak` copies in place (no git repo).
- **WhatsApp allowlist lives in TWO spots — keep them in sync:** `.env`
  `WHATSAPP_ALLOWED_USERS` and `config.yaml` `whatsapp.allow_from` (both are
  comma-separated phone numbers + LIDs). Restart the gateway after editing either.
- **Egress:** WhatsApp/Meta reachability rides the retained broad WhatsApp CIDRs (see
  Egress note); hermes also needs `models.dev` — both restored after the Phase-4 tighten.

#### Egress Firewall (DNS-driven allowlist, reworked 2026-07-21)
- Default deny outbound at nft L3; the allowlist is now **DNS-driven**: a local
  **dnsmasq** (127.0.0.1:53, the host resolver) injects each allow-listed
  domain's resolved IPs into the nft `allowed_hosts`/`allowed_hosts_v6` timeout
  sets as they resolve (30m self-expiring, refreshed on use). No more dig loop,
  no 6h re-resolve, no manual CIDR widening — IP rotation self-heals.
- **The allowlist = the domain list** in `modules/dnsmasq.nix` (`egressDomains`).
  To allow a new domain: add it there, `nixos-rebuild switch`.
- **dnsmasq upstream:** farsika's AdGuard on its LAN address `192.168.86.26`
  (blocklist + query log + the `.dashboard`/`.board` rewrites), failover to Quad9
  `9.9.9.10` (strict-order). `.ts.net` → MagicDNS `100.100.100.100`. Tailscale
  `accept-dns` is OFF (dnsmasq owns resolv.conf).
  ⚠️ This pointed at the **LAN router** `192.168.86.1` until PR #149 (2026-09-10),
  back when the old Google Wifi box forwarded to AdGuard as its upstream. The
  router was replaced by a UDR7 ("popcorn") on 2026-09-09 and a new gateway does
  **not** inherit that forwarding — so the router path silently stopped reaching
  AdGuard: filtering and query logging gone, and the three friendly names failed
  to resolve on this host at all (each lookup hanging ~20s before Quad9 answered
  NXDOMAIN). Nothing was *down*; public names kept working via the fallback,
  which is exactly why it went unnoticed. Use farsika's **LAN** address, not its
  tailnet address `100.70.53.80` (known AdGuard bind fragility).
- **Static CIDRs** (things with no clean domain to inject): tailscale CGNAT,
  github SSH, vercel, plus the broad **WhatsApp/Meta** CIDRs (v4 incl. `57.144.0.0/14`
  + v6) — WhatsApp rotation is too opaque to inject, so it stays on broad CIDRs (the
  one exception; restored in Phase 4 for hermes/Saul). In `modules/firewall.nix`.
- **Status:** `sudo nft list table inet egress_filter` (watch `allowed_hosts`
  fill with `expires` entries); dnsmasq: `systemctl status dnsmasq`.
- **Denied connections:** `sudo journalctl -k | grep EGRESS_DENIED`.
  ⚠️ **`SRC=192.168.86.122` is botserver ITSELF** — this doc previously called
  that traffic "pre-existing background noise from another host". It is not
  another host, and treating it as noise cost two investigations (2026-07-29 and
  2026-07-30) their actual root cause. The ~100/min baseline was diagnosed
  2026-08-03: **tailscaled retrying DERP relay servers** (`derp*.tailscale.com`,
  `199.38.181.x` / `209.177.x` / `199.165.136.x` on **TCP 443**), which are not
  allow-listed. Consequence: Tailscale relaying over TCP 443 does not work — UDP
  STUN (3478) and NAT-PMP (5351) are allowed, so `tailscale netcheck` still
  reports healthy DERP latency and hides this. Smaller contributors: SSDP/UPnP
  discovery (`239.255.255.250:1900`, `192.168.86.1:1900`) and DHCP renewal
  (`192.168.86.1:67`).
  Also note the log rule is rate-limited, so what you see is a **sample**, not
  the volume — check the `counter` on the drop rule for real totals.
- **Containers resolve via the host dnsmasq** on the multica bridge gateway
  (`--dns=172.18.0.1`), NOT the router. Both the backend and — since PR #149
  (2026-09-10) — the frontend. Resolving *through* dnsmasq is load-bearing, not a
  convenience: dnsmasq's nftset bindings inject each answer's IPs into the egress
  allowlist, so a container resolving anywhere else gets addresses the filter then
  rejects — a working lookup and a dead connection.
  ⚠️ Pointing a container at the router is what broke Multica for ~3 weeks: farsika
  went down ~2026-08-15, and because the containers bypassed dnsmasq they never saw
  its Quad9 failover, so the backend could not resolve `api.resend.com` and sign-in
  emails stopped silently. The backend was fixed then; the **frontend was missed**
  and kept `--dns=192.168.86.1` (with a comment claiming parity with the backend)
  until #149. Full design + rationale:
  `~/.tracking/botserver-egress-dns-allowlist.md`.
- **Disable (emergency):** `sudo systemctl stop openclaw-egress && sudo nft delete table inet egress_filter`
- **Rollback the DNS rework:** `sudo tailscale set --accept-dns=true && sudo nixos-rebuild switch --rollback`

#### Health Check
```bash
ssh ethan@botserver << 'EOF'
# NOTE: `sudo -u openclaw bash -l -c "systemctl --user ..."` (the old form here) does NOT
# work — no systemd session. Pass XDG_RUNTIME_DIR explicitly instead (uid 1001=openclaw,
# 1003=hermes), or just `ssh openclaw@botserver` directly. Corrected 2026-08-08.
sudo -u openclaw XDG_RUNTIME_DIR=/run/user/1001 systemctl --user status openclaw-gateway --no-pager
sudo -u hermes   XDG_RUNTIME_DIR=/run/user/1003 systemctl --user is-active hermes-gateway.service
systemctl status openclaw-egress --no-pager
tailscale status
uptime
df -h /
free -h
EOF
```
