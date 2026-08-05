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

### OpenClaw (v2026.3.7)
- **Status:** `ssh openclaw@botserver systemctl --user status openclaw-gateway`
- **Restart:** `ssh openclaw@botserver systemctl --user restart openclaw-gateway`
- **Logs:** `ssh openclaw@botserver journalctl --user -u openclaw-gateway --since "1 hour ago" --no-pager`
- **Config:** `~openclaw/.openclaw/openclaw.json`
- **Install:** `~openclaw/.local/opt/openclaw` (built from source)
- **Secrets:** sops-nix decrypts to `/run/secrets/openclaw.env` at boot

#### Agents

| Agent | Channel | Workspace | Model |
|-------|---------|-----------|-------|
| main (KingKong) | Telegram + Discord | ~/kingkong | gpt-5.4 (fallback minimax-m2.7) |
| hope | Telegram | ~/hope-bot | gpt-5.4 (fallback minimax-m2.7) |
| thoth | Telegram (capture) | ~/thoth-bot | gpt-5.4 (fallback minimax-m2.7) |

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
| family-board | 3456 | Node.js | `ssh openclaw@botserver systemctl --user status family-board` |
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
- **dnsmasq upstream:** LAN router `192.168.86.1` (→ farsika AdGuard for logging
  + blocklist), failover to Quad9 `9.9.9.10` (strict-order). `.ts.net` → MagicDNS
  `100.100.100.100`. Tailscale `accept-dns` is OFF (dnsmasq owns resolv.conf).
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
- **Containers** use `--dns=192.168.86.1` (router), NOT the host dnsmasq — see
  the docker-dns memory. Full design + rationale:
  `~/.tracking/botserver-egress-dns-allowlist.md`.
- **Disable (emergency):** `sudo systemctl stop openclaw-egress && sudo nft delete table inet egress_filter`
- **Rollback the DNS rework:** `sudo tailscale set --accept-dns=true && sudo nixos-rebuild switch --rollback`

#### Health Check
```bash
ssh ethan@botserver << 'EOF'
sudo -u openclaw bash -l -c "systemctl --user status openclaw-gateway --no-pager"
systemctl status openclaw-egress --no-pager
tailscale status
uptime
df -h /
free -h
EOF
```
