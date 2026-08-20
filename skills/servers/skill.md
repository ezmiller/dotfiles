---
name: servers
description: >
  Manage, troubleshoot, and answer questions about Ethan's personal infrastructure nodes on the
  Tailscale VPN. Trigger when the user names a specific node (botserver, farsika, moltbot-aws,
  ethan-duster, songster, pixel), or mentions Tailscale, OpenClaw, Ollama, Resilio Sync, Plex, S3
  backups, NixOS, hydroxide, rengine, family-board, hermes, Caddy, AdGuard, or UniFi. Also
  trigger for the internal friendly links (openclaw.dashboard, hermes.dashboard, family.board)
  and any "dashboard won't load / link isn't working" report about them. Do NOT trigger on bare "server"
  or "server status" when the workspace is pk-shopify-theme — that means the local webpack/
  Shopify CLI dev server and belongs to the `pk-dev-server` skill instead.
user_invocable: true
---

# Ethan's Server Fleet

All servers are accessed via **Tailscale mesh VPN**. No public SSH.

## Tailscale Network

| Host | Tailscale IP | OS | Role | Status | Details |
|------|-------------|-----|------|--------|---------|
| botserver | 100.117.184.4 | NixOS | Primary — OpenClaw + hermes agent gateways, Ollama | Active | `references/botserver.md` |
| farsika | 100.70.53.80 | Linux | Backup server — S3 sync, Resilio; **LAN DNS (AdGuard Home)**, Home Assistant | Active | `references/farsika.md` |
| ethan-duster | 100.126.203.96 | Linux | Media server — Plex, Sunshine, Steam | Active | `references/duster.md` |
| moltbot-aws | 100.97.168.36 | Amazon Linux 2023 | Legacy EC2 — gateway disabled | Reference only | `references/other-nodes.md` |
| songster | 100.64.228.27 | NixOS | UniFi controller for the parents' network | Active — built, awaiting cutover | `references/songster.md` |
| pixel | 100.115.153.79 | Android (Termux) | Phone — Resilio Sync peer | Active | `references/other-nodes.md` |

## How to Respond

1. **Read the relevant reference file first** (see routing below) — each holds hard-won
   gotchas that will cost a session if skipped. Don't answer from this index alone.
2. **SSH in second** — don't guess from docs. Get live state.
3. **Show actual output** — log lines, service status, disk numbers.
4. **Be conservative with MCP tools** — limit results, one page at a time.

## Routing — which reference file to read

| If the question involves… | Read |
|---|---|
| OpenClaw, KingKong/hope/thoth agents, gateway, agent cron jobs | `references/botserver.md` |
| hermes / Saul / WhatsApp channel | `references/botserver.md` |
| the friendly links (`openclaw.dashboard`, `hermes.dashboard`, `family.board`), Caddy, internal CA/TLS, a dashboard that won't load | `references/botserver.md` |
| egress firewall, `EGRESS_DENIED`, dnsmasq, allow-listing a domain | `references/botserver.md` |
| NixOS rebuilds, `/etc/nixos`, sops secrets, `gws` (Google Workspace CLI) | `references/botserver.md` |
| family-board, hydroxide, rengine, Multica | `references/botserver.md` |
| S3 backups, snapshots, a red backup healthcheck | `references/farsika.md` |
| **AdGuard / LAN DNS / any whole-house "internet is down"** | `references/farsika.md` |
| the `org` repo git sync, Resilio, conflict markers, stuck rebase | `references/farsika.md` |
| Home Assistant | `references/farsika.md` |
| Plex, Sunshine/Moonlight, Steam, Transmission, duster disk space | `references/duster.md` |
| UniFi, the parents' network, songster (**either** of them), AMT / vPro remote recovery | `references/songster.md` |
| the phone's Resilio shares, legacy EC2 | `references/other-nodes.md` |

## Cross-cutting facts worth knowing up front

- **LAN DNS is a single point of failure on farsika.** AdGuard Home there serves DNS to the
  whole house *and* the tailnet. If several unrelated things break at once, or a device only
  works after switching it to `1.1.1.1`, suspect AdGuard before anything else.
- **LAN IPs are DHCP and have drifted.** botserver was `.36`, now `.122` (2026-08-03).
  Confirm before trusting any LAN IP; Tailscale IPs above are stable.
- **Two machines are called songster**, and three sites use `192.168.1.0/24` with a
  device at `.112`. Always confirm *which* machine answered before acting — check the
  SSH banner or hostname, not the address. See `references/songster.md`.
- **A Tailscale key expiring silently removes a node from the tailnet.** It cost 173 days
  of songster being unreachable. Disable key expiry on every unattended node, and
  re-check it after deleting or re-registering one — the setting does not survive that.
- **User services need a direct SSH as that user.** `ssh openclaw@botserver` /
  `ssh hermes@botserver` for `systemctl --user` — `su -` and `sudo -u` don't get a systemd
  session. (Exception: `sudo -u <user> XDG_RUNTIME_DIR=/run/user/<uid> systemctl --user …`
  *does* work from the `ethan` account when you only need to read state — uid 1001 =
  openclaw, 1002 = multicad, 1003 = hermes.)
- **Run anything slow under `nohup`, then poll.** Long foreground SSH commands (backups,
  builds, `nixos-rebuild`, migrations) get their connection dropped, and the drop surfaces
  as **exit 255 with completely empty output** — indistinguishable from the command itself
  failing. On 2026-08-08 a *successful* `openclaw backup create` looked like a failure
  twice this way. Launch with `nohup <cmd> > /tmp/<name>.log 2>&1 &`, poll with short
  separate calls, and before believing a failure check `uptime` (did it reboot?), load
  average, and whether the output artifact exists and passes its own integrity check.
- **Two agents write to the `org` repo.** farsika's 3am job and KingKong both push to
  `main`; collisions have caused multi-day outages. Details in `references/farsika.md`.
