# farsika

**Backup and sync server.** Syncs `~/sync/` (~12GB) to S3 with tiered retention.

### Connection
- `ssh farsika` (configured in ~/.ssh/config, user `ezmiller`)
- Tailscale IP: 100.70.53.80
- LAN IP: **192.168.86.26, a fixed IP** reserved on popcorn (the NY apartment's UDR7) as of 2026-09-10. It is no longer DHCP-assigned, and it must not change: popcorn hands this exact address to every device as its DNS server, so if farsika moves, DNS dies for the whole apartment.
- `~/.ssh/config` now pins `HostName 100.70.53.80`, so `ssh farsika` no longer depends on MagicDNS resolving. (An earlier version of this file said the alias had no `HostName` — it does.) If the tailnet is down, fall back to `ssh ezmiller@192.168.86.26` on the LAN.
- **Physically at the NY apartment (520 Lincoln Pl. Apt 6D), not Vashon.** "The home LAN" in this file means the apartment's `192.168.86.0/24`.

### Services

#### S3 Backup System

| Schedule | Script | Target | Log |
|----------|--------|--------|-----|
| Daily 2 AM | `~/bin/sync-backup.sh` | `s3://farsika-sync-backup/current/` | `~/logs/sync-backup.log` |
| Weekly Sun 5 AM | `~/bin/weekly-snapshot.sh` | `s3://farsika-sync-backup/weekly/YYYY-WNN/` | `~/logs/weekly-snapshot.log` |
| Quarterly | `~/bin/quarterly-snapshot.sh` | `s3://farsika-sync-backup/YYYY-QN/` | `~/logs/quarterly-snapshot.log` |
| Yearly Mar 1 | `~/bin/yearly-archive.sh` | `the-vault` bucket (Glacier Deep Archive) | `~/logs/yearly-archive.log` |

**What's backed up:** `~/sync/` containing `org/` (personal notes), `Documents/` (archive), `digital-library/` (books)

> **Don't trust a red backup check — read the log.** These scripts must tell a real failure apart from `aws s3 sync`'s harmless exit-2-on-unreadable-files (`.sync/root_acl_entry` is skipped every run, that's normal). Until 2026-07-27 they grepped for `error|fatal|failed` anywhere in the output, so a *filename* could fake a failure — `…Individual Terrorism.org` contains "T**error**ism", and `.git/rebase-merge/no-reschedule-failed-exec` contains "failed". Three false DOWN alerts (07-20, 07-26, 07-27) with backups completing fine. Now matches aws's own error line prefixes after `tr '\r' '\n'` (the `tr` is required — progress output is `\r`-joined, so `^` won't anchor). Confirm any red check against an actual `fatal error:` / `upload failed:` line in `~/logs/sync-backup.log`.

#### Org Repo Git Sync (`ezmiller/org`)

- **Daily 3 AM** `~/bin/org-git-sync.sh` → log `~/logs/org-git-sync.log`. Commits whatever Resilio brought in from peers (`git add -A`), `pull --rebase origin main`, pushes.
- farsika is the **only peer with a `.git/`** — laptop and phone see the working tree only, and `.git` is in every peer's Resilio IgnoreList. Never `git init`/`clone` inside `~/org` on the laptop.
- Journals work as a textual kanban: each day's board is **carried forward** into the new journal and the old one is gutted to its 3-line header. A large uncommitted deletion in `techwork/journals/<yesterday>.org` is therefore *normal* — before "restoring" it, check whether the content landed in a later journal.

> **⚠️ Two writers on `main` (hit July 2026, still unresolved).** **KingKong Bot** (`kingkongbot@users.noreply.github.com`, the botserver agent) also commits and pushes to `origin/main` directly, at arbitrary times. On 2026-07-25 it pushed an edit to a Logseq page that farsika's 3am job had uncommitted local edits to; the rebase conflicted and left `~/sync/org/.git/rebase-merge/`. **A stuck rebase blocks every subsequent night** (`fatal: there is already a rebase-merge directory`) — the outage never self-heals, and the check stayed DOWN three days. The next night's run then did `git add -A && git commit` on the conflicted tree, committing **conflict markers** onto a detached HEAD, which Resilio propagated to the laptop and phone.
> **Diagnosing:** `cd ~/sync/org && git status` (look for "rebase in progress"), `test -d .git/rebase-merge`, `git log origin/main --format='%an %s'` for non-farsika authors near the failure, and `grep -rlE "^(<<<<<<<|>>>>>>>)" --include="*.org" .` (excluding `.sync/` and `logseq/bak/`) to see if markers reached peers.
> **Recovering:** back up any *intentionally* dirty files first (a hard reset resurrects carry-forward deletions — Resilio will then spread the resurrection), then `git rebase --abort`, reconcile with `origin/main`, restore the backed-up files, commit, push. Re-run `~/bin/org-git-sync.sh` by hand to clear the healthcheck rather than waiting for 3 AM.

#### Resilio Sync
- Runs as: `rslsync` user
- Sync directory: `/home/ezmiller/sync/`
- Status: `systemctl status resilio-sync`

#### AdGuard Home (LAN DNS) — critical, whole-house dependency
- **This is the DNS server for the apartment LAN.** If AdGuard is down, every device on the network loses DNS (symptom: things "work" only after switching a device to `1.1.1.1`).
- **How devices are pointed here:** popcorn hands out `192.168.86.26` directly as the DHCP DNS server — clients query AdGuard themselves rather than going through the gateway. The old Google Wifi router instead forwarded to AdGuard as its *upstream*; that arrangement is gone, and popcorn's own upstream is deliberately left unfiltered (its only DNS consumer is itself). Consequence: **AdGuard's protections do not depend on any router setting**, but they also do not cover a device that ignores DHCP and hardcodes its own DNS. Closing that would take a firewall rule on popcorn blocking outbound port 53 to anything but farsika; not done as of 2026-09-10.
- Service: `systemctl status AdGuardHome` (`/etc/systemd/system/AdGuardHome.service`, runs `/opt/AdGuardHome/AdGuardHome -s run`)
- Config: `/opt/AdGuardHome/AdGuardHome.yaml` (edit + `sudo systemctl restart AdGuardHome`)
- Listens on `:53` at **three** addresses (`dns.bind_hosts`): LAN `192.168.86.26`, Tailscale v4 `100.70.53.80`, Tailscale v6 `fd7a:115c:a1e0::7732:3550` — serves DNS to both LAN and tailnet.
- Web UI on port `6060`. Upstreams: Quad9 (`9.9.9.10` etc.).
- **`dns.rewrites` in that YAML is where the friendly links resolve** — `openclaw.dashboard`,
  `hermes.dashboard`, `family.board` all → `100.117.184.4` (botserver). Not in any git repo,
  so a farsika rebuild loses all of them at once. See botserver's "Friendly links" section.
- Check binds: `sudo ss -tulnp | grep ':53 '` — expect all three addresses, UDP+TCP.
- **Upgrades:** manual `/opt/AdGuardHome` install (raw binary + systemd unit), **not** managed by any repo (not `farsika-config`, not `botserver-nix`). Use the built-in updater: `sudo /opt/AdGuardHome/AdGuardHome --update` — auto-detects arch (arm64), backs up old binary + config to `/opt/AdGuardHome/agh-backup/`, and **auto-restarts the service** (~1–2s DNS blip). Take a manual `cp AdGuardHome.yaml AdGuardHome.yaml.bak-vNN` first. Verify: `--version`, `is-active`, all three `:53` binds, a test query. Note `dig`/`nslookup` are **not installed** — test resolution with a raw-socket python one-liner. Last upgraded 2026-07-23: **v0.107.75 → v0.107.78** (security release; also cleared the .75 DNSSEC-cache bug). Caveat: .76 rewrites YAML duration values to `d` units, so a rollback to ≤.75 needs them converted back to hours — hence keep the pre-upgrade `.yaml.bak`.

> **⚠️ Tailscale ↔ AdGuard deadlock (hit June 2026).** Because `bind_hosts` includes the Tailscale IPs, if Tailscale drops (e.g. node-key expiry → node logged out), those addresses disappear and AdGuard **crash-loops** (`bind: cannot assign requested address`) → LAN DNS dies. Then tailscaled can't resolve `controlplane.tailscale.com` to log back in (its DNS *is* AdGuard) → deadlock. Note the LAN IP still binds fine, so this is **not** a switch-port/DHCP problem.
> **Mitigation already in place:** `/etc/sysctl.d/99-adguard-nonlocal-bind.conf` sets `net.ipv4.ip_nonlocal_bind=1` + `net.ipv6.ip_nonlocal_bind=1` so AdGuard can bind the Tailscale IPs even before `tailscale0` exists. Verify with `/usr/sbin/sysctl net.ipv4.ip_nonlocal_bind` (note: `sysctl` isn't in the non-login PATH; use the full path).
> **Manual recovery if it ever recurs:** (1) drop the two Tailscale lines from `bind_hosts`, restart AdGuard → LAN DNS back; (2) `sudo tailscale up --advertise-exit-node` and approve the login URL; (3) re-add the Tailscale lines, restart AdGuard.
- **Parental control is on** (`parental_enabled: true`) and blocks game/gacha domains house-wide, showing up as vague in-game errors. Blocks from it log as `"Reason":5` / `parental CATEGORY_BLACKLISTED` in `data/querylog.json` (search the log by client IP and filter on `IsFiltered`, not by guessed domain names; recent queries sit in memory until flushed). Fix = allow rule in `user_rules`. 2026-09-27: added `@@||qpyou.cn^` (Com2uS/MLB Rivals servers; the game showed "Failed to update GameGuard (380)").

#### Home Assistant (Docker)
- `homeassistant` + `matter-server` containers (`sudo docker ps`), images `ghcr.io/home-assistant/...:stable`.

#### Configuration
- **Config repo:** `~/.farsika-config` (remote: `git@github.com:ezmiller/farsika-config.git`)
- Scripts in `~/bin/` are symlinks to `~/.farsika-config/bin/`
- To update: `cd ~/.farsika-config && git pull && ./install.sh`

#### Health Check
```bash
ssh farsika << 'EOF'
systemctl status resilio-sync --no-pager
systemctl is-active AdGuardHome && sudo ss -tulnp | grep ':53 '
tailscale status
grep -E '^\[' ~/logs/sync-backup.log | tail -3
grep -E '^\[' ~/logs/org-git-sync.log | tail -3
cd ~/sync/org && git status --porcelain -uall && test -d .git/rebase-merge && echo 'WARNING: stuck rebase'
df -h /
uptime
EOF
```
