---
name: farsika
description: Manage, troubleshoot, and answer questions about the farsika personal server. Use when the user mentions farsika, server maintenance, backups, Resilio Sync, or asks about their S3 backup system.
user_invocable: true
---

# Farsika Server

You are helping the user manage and understand their personal server called **farsika**. When the user invokes this skill, they may ask a specific question, request a maintenance check, or ask you to perform an operation. Use the context below to know where to look and what to do.

## Connection

- **SSH:** `ssh farsika` (configured in ~/.ssh/config, user `ezmiller`)
- **Tailscale IP:** 100.70.53.80
- The server is accessed over Tailscale mesh VPN

## Services Running

### S3 Backup System
Syncs `~/sync/` (~12GB) to S3 with a tiered retention strategy:

| Schedule | Script | Target | Log | Healthcheck ID |
|---|---|---|---|---|
| Daily 2 AM | `~/bin/sync-backup.sh` | `s3://farsika-sync-backup/current/` | `~/logs/sync-backup.log` | `c7352d6d-ec31-4995-b330-d01fb76cad36` |
| Weekly Sun 5 AM | `~/bin/weekly-snapshot.sh` | `s3://farsika-sync-backup/weekly/YYYY-WNN/` | `~/logs/weekly-snapshot.log` | `abdbb7d5-a91a-46ae-87f2-db915db1d6c8` |
| Quarterly (Jan/Apr/Jul/Oct 1) | `~/bin/quarterly-snapshot.sh` | `s3://farsika-sync-backup/YYYY-QN/` | `~/logs/quarterly-snapshot.log` | `145d2323-f673-4436-b3cc-32945f9fefbf` |
| Yearly Mar 1 | `~/bin/yearly-archive.sh` | `the-vault` bucket (Glacier Deep Archive) | `~/logs/yearly-archive.log` | `27e1c53e-b080-434b-8a29-2c2e579b0942` |
| Yearly Apr 1 | `~/bin/yearly-cleanup.sh` | Removes old quarterly snapshots | `~/logs/yearly-archive.log` | — |

**What's backed up:** `~/sync/` containing `org/` (personal notes), `Documents/` (archive), `digital-library/` (books)

**Known issue:** AWS S3 sync shows harmless warnings about Resilio Sync metadata files (.sync/root_acl_entry) — exit code 2 from AWS CLI. The scripts handle this correctly.

### Resilio Sync
- Runs as: `rslsync` user
- Sync directory: `/home/ezmiller/sync/`
- Status: `systemctl status resilio-sync`
- Restart: `sudo systemctl restart resilio-sync`
- Logs: `sudo journalctl -u resilio-sync`

### Tailscale
- Status: `tailscale status`
- IP: `tailscale ip`

## Configuration Management

- **Config repo:** `~/.farsika-config` (remote: `git@github.com:ezmiller/farsika-config.git`)
- Scripts in `~/bin/` are **symlinks** to `~/.farsika-config/bin/`
- To update: `cd ~/.farsika-config && git pull && ./install.sh`
- Crontab is managed in the repo: `~/.farsika-config/crontab`
- Full docs: `~/.farsika-config/SERVICES.md` and `~/.farsika-config/README.md`

## How to Respond

When the user asks a question or requests maintenance:

1. **SSH into farsika** to get live data — don't guess from static docs alone.
2. **Check the relevant logs, service status, or system state** based on what they're asking about.
3. **Be specific** — show actual log output, disk numbers, service status, etc.
4. **For edits to scripts**, remember they live in `~/.farsika-config/bin/` and changes should be committed to the Git repo.

### Common maintenance checks

If the user asks for a general health check, run these via SSH:

```bash
# Service status
systemctl status resilio-sync --no-pager
tailscale status

# Recent backup logs (last run of each)
tail -20 ~/logs/sync-backup.log
tail -20 ~/logs/weekly-snapshot.log

# Disk usage
df -h /
du -sh ~/sync/

# Cron jobs are installed
crontab -l

# System uptime and load
uptime

# Pending security updates (if apt-based)
apt list --upgradable 2>/dev/null | head -20
```

### Answering questions

- **"Did last night's backup work?"** → Check `~/logs/sync-backup.log` tail
- **"How much space am I using?"** → `df -h /` and `du -sh ~/sync/`
- **"Is Resilio running?"** → `systemctl status resilio-sync`
- **"What's in the S3 bucket?"** → `aws s3 ls s3://farsika-sync-backup/`
- **"When did the last quarterly snapshot run?"** → Check `~/logs/quarterly-snapshot.log`
- **"What cron jobs are set up?"** → `crontab -l`
- **Script questions** → Read from `~/.farsika-config/bin/`
