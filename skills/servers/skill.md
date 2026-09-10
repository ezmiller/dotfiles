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
| ~~moltbot-aws~~ | ~~100.97.168.36~~ | Amazon Linux 2023 | Legacy EC2 | **Terminated 2026-09-08** | `references/other-nodes.md` |
| songster | 100.64.228.27 | NixOS | Wired vantage point at the white house (8743, parents') — UniFi controller **retired**, UDR7 runs the network | Active | `references/songster.md` |
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
| UniFi, UDR7, the parents'/white-house network (8743), the red house (8739), songster, Xfinity XB8-T or SB8200v3 modems, AMT / vPro remote recovery | `references/songster.md` |
| the phone's Resilio shares, legacy EC2 | `references/other-nodes.md` |

## Cross-cutting facts worth knowing up front

- **LAN DNS is a single point of failure on farsika.** AdGuard Home there serves DNS to the
  whole house *and* the tailnet. If several unrelated things break at once, or a device only
  works after switching it to `1.1.1.1`, suspect AdGuard before anything else.
- **A router swap does not carry the AdGuard hand-off with it.** Filtering reaches devices
  because the gateway *hands out* farsika as the DHCP DNS server (popcorn: Networks → LAN →
  DHCP → DNS Server → Manual → `192.168.86.26`, that entry alone — a second/fallback entry
  makes clients round-robin and silently skip filtering). A new gateway comes blank, so
  ad-blocking and the `.dashboard`/`.board` rewrites vanish house-wide with nothing looking
  broken. Clients only pick the setting up on lease renewal (`sudo ipconfig set en0 DHCP`,
  or a Wi-Fi off/on) — until then it looks like the change did nothing.
- **Wired nodes do not notice a router swap.** Their switch port never blinks, so they keep
  a lease from the old range and sit there unreachable — showing **offline on Tailscale**,
  looking dead while running fine. To find them, put the laptop temporarily on the old
  range: `sudo ifconfig en0 alias <old-subnet>.200 255.255.255.0`, probe, then
  `sudo ifconfig en0 -alias <old-subnet>.200`. (`ping6 ff02::1%en0` is a no-sudo first pass,
  but UniFi filters some multicast to Wi-Fi clients, so a miss there proves nothing.)
- **Do not diagnose botserver LAN reachability with `ping`.** It blocks outbound ICMP — it
  cannot ping its own gateway while happily using it for DNS. Test the actual port instead
  (`host <name> <server>` for DNS; `host` is installed, `dig` is not).
- **farsika and botserver live at the NY apartment**, not Vashon. Their LAN IPs are now
  **fixed** on popcorn (2026-09-10): farsika `192.168.86.26`, botserver `192.168.86.122`.
  Before that they were DHCP and drifted (botserver was `.36`, then `.122`). Tailscale IPs
  are stable regardless and remain the safest way to address either box.
- **Three sites, three gateways — never assume which one you are on.**

  | Site | LAN | Gateway | Nodes there |
  |---|---|---|---|
  | **NY apartment** — 520 Lincoln Pl. Apt 6D | `192.168.86.0/24` | UDR7 **"popcorn"** @ `.86.1` | farsika, botserver, the laptop |
  | **white house** — 8743, Vashon (parents') | `192.168.1.0/24` | UDR7 **"Songster"** @ `.1.1` | songster, barn AP |
  | **red house** — 8739, Vashon | `192.168.1.0/24` | older **UDM** | — |

  The NY apartment's `192.168.86.0/24` is a *deliberate* choice — it was the old Google Wifi
  default, and a great deal of config hardcodes `192.168.86.x`. Keep it. See
  `references/home-router-udr7` notes in memory.

- **Two Vashon houses both use `192.168.1.0/24`:** red house = **8739**, white house =
  **8743** (the parents'). songster is at 8743. A full tunnel into one site cannot reach
  `192.168.1.x` at the other. Always confirm *which* site answered by hostname or public
  IP, never by address. See `references/songster.md`.
- **The parents' gateway (the UDR7 named "Songster", at 8743) is in the data path.** It replaced both the
  Xfinity-box-as-router setup and songster's UniFi controller (now off). If the UDR7 is
  down, their internet is down — this is no longer a management-only outage.
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
