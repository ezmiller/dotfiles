---
name: servers
description: >
  Manage, troubleshoot, and answer questions about Ethan's personal infrastructure nodes on the
  Tailscale VPN. Trigger when the user names a specific node (botserver, farsika, moltbot-aws,
  ethan-duster, songster), or mentions Tailscale, OpenClaw, Ollama, Resilio Sync, Plex, S3
  backups, NixOS, hydroxide, rengine, family-board, or UniFi. Do NOT trigger on bare "server"
  or "server status" when the workspace is pk-shopify-theme — that means the local webpack/
  Shopify CLI dev server and belongs to the `pk-dev-server` skill instead.
user_invocable: true
---

# Ethan's Server Fleet

All servers are accessed via **Tailscale mesh VPN**. No public SSH.

## Tailscale Network

| Host | Tailscale IP | OS | Role | Status |
|------|-------------|-----|------|--------|
| botserver | 100.117.184.4 | NixOS | Primary — OpenClaw, Ollama | Active |
| farsika | 100.70.53.80 | Linux | Backup server — S3 sync, Resilio | Active |
| moltbot-aws | 100.97.168.36 | Amazon Linux 2023 | Legacy EC2 — gateway disabled | Reference only |
| ethan-duster | 100.126.203.96 | Linux | Media server — Plex | Active |
| songster | 100.84.34.106 | Raspberry Pi OS | Ubiquiti UniFi controller (parents' house) | Mostly dormant |

## How to Respond

1. **SSH in first** — don't guess from docs. Get live state.
2. **Show actual output** — log lines, service status, disk numbers.
3. **Be conservative with MCP tools** — limit results, one page at a time.

---

## botserver (ThinkCentre M75q Gen 2)

**Primary server.** Runs all OpenClaw agents and supporting services.

### Connection
- `ssh ethan@192.168.86.36` (LAN) or `ssh ethan@100.117.184.4` (Tailscale) — admin, sudo
- `ssh openclaw@192.168.86.36` — for user services (systemctl --user). Must SSH directly, `su -` doesn't get a systemd session.

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
| main (KingKong) | Telegram + Discord | ~/kingkong | openai-codex/gpt-5.4 |
| family | WhatsApp | ~/family-bot | openrouter/minimax-m2.5 |
| hope | Telegram | ~/hope-bot | openrouter/minimax-m2.5 |
| thoth | Telegram (capture) | ~/thoth-bot | openrouter/minimax-m2.5 |

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

#### Egress Firewall
- Default deny outbound + domain/CIDR allowlist
- **Config:** `/etc/openclaw/egress-allowlist.conf`
- **Status:** `sudo nft list table inet egress_filter`
- **Denied connections:** `sudo journalctl -k | grep EGRESS_DENIED`
- **Disable (emergency):** `sudo systemctl stop openclaw-egress && sudo nft delete table inet egress_filter`
- **Re-resolve DNS:** `sudo systemctl restart openclaw-egress`
- DNS re-resolves automatically every 6 hours

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

---

## farsika

**Backup and sync server.** Syncs `~/sync/` (~12GB) to S3 with tiered retention.

### Connection
- `ssh farsika` (configured in ~/.ssh/config, user `ezmiller`)
- Tailscale IP: 100.70.53.80

### Services

#### S3 Backup System

| Schedule | Script | Target | Log |
|----------|--------|--------|-----|
| Daily 2 AM | `~/bin/sync-backup.sh` | `s3://farsika-sync-backup/current/` | `~/logs/sync-backup.log` |
| Weekly Sun 5 AM | `~/bin/weekly-snapshot.sh` | `s3://farsika-sync-backup/weekly/YYYY-WNN/` | `~/logs/weekly-snapshot.log` |
| Quarterly | `~/bin/quarterly-snapshot.sh` | `s3://farsika-sync-backup/YYYY-QN/` | `~/logs/quarterly-snapshot.log` |
| Yearly Mar 1 | `~/bin/yearly-archive.sh` | `the-vault` bucket (Glacier Deep Archive) | `~/logs/yearly-archive.log` |

**What's backed up:** `~/sync/` containing `org/` (personal notes), `Documents/` (archive), `digital-library/` (books)

#### Resilio Sync
- Runs as: `rslsync` user
- Sync directory: `/home/ezmiller/sync/`
- Status: `systemctl status resilio-sync`

#### Configuration
- **Config repo:** `~/.farsika-config` (remote: `git@github.com:ezmiller/farsika-config.git`)
- Scripts in `~/bin/` are symlinks to `~/.farsika-config/bin/`
- To update: `cd ~/.farsika-config && git pull && ./install.sh`

#### Health Check
```bash
ssh farsika << 'EOF'
systemctl status resilio-sync --no-pager
tailscale status
tail -5 ~/logs/sync-backup.log
df -h /
uptime
EOF
```

---

## moltbot-aws (LEGACY)

**EC2 instance — OpenClaw gateway disabled.** Kept for reference. Will be terminated.

- `ssh moltbot@moltbot-aws` (Tailscale)
- Infra repo: `~/Projects/moltbot-aws-terraform`
- OpenClaw gateway: **disabled** (`systemctl --user disable openclaw-gateway`)
- All agents migrated to botserver as of 2026-04-10
- Pre-migration backup: `~/.openclaw/openclaw.json.pre-kingkong-migration`

---

## ethan-duster

**Media server / gaming host.** Plex, Sunshine (Moonlight streaming), Transmission, Steam.

- `ssh ethan@ethan-duster` (Tailscale IP: 100.126.203.96, LAN 192.168.86.216)
- **OS:** Manjaro Linux (Arch-based) — use `parted`/`wipefs`, not `sgdisk`

### Storage

| Device | Size | FS | Mount | Role |
|--------|------|-----|-------|------|
| sda | 119G | — | — | OS SSD (partitioned) |
| sda3 | 28G | ext4 | `/` | Root — **chronically ~96% full, watch closely** |
| sda4 | 73G | ext4 | `/home` | User home |
| sdb1 | 931G | ext4 (label `games`) | `/mnt/games` | **Steam library** (SSD, fast) |
| sdc1 | 931G | ext3 | `/srv` | Bulk storage (HDD, slow — migrate to ext4 someday) |

- Steam library folder: `/mnt/games/SteamLibrary`
- fstab uses UUID for `/mnt/games`
- `put.io` rclone mount appears at `/mnt/putio`

### Services
- **Plex Media Server**
- **Sunshine** (game streaming) — see `~/.tracking/duster-sunshine-setup.md` for X11/NVIDIA setup notes
- **Transmission** (system service, `transmission` user) — Web UI http://192.168.86.216:9091

---

## songster

**Raspberry Pi at parents' house.** Runs Ubiquiti UniFi network controller.

- `ssh songster` (Tailscale IP: 100.84.34.106)
- Mostly dormant — used for managing parents' UniFi Wi-Fi setup
- May be offline for extended periods (last seen can be weeks)
