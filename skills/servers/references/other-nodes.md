# Other nodes — moltbot-aws, songster, pixel

## moltbot-aws (LEGACY)

**EC2 instance — OpenClaw gateway disabled.** Kept for reference. Will be terminated.

- `ssh moltbot@moltbot-aws` (Tailscale)
- Infra repo: `~/Projects/moltbot-aws-terraform`
- OpenClaw gateway: **disabled** (`systemctl --user disable openclaw-gateway`)
- All agents migrated to botserver as of 2026-04-10
- Pre-migration backup: `~/.openclaw/openclaw.json.pre-kingkong-migration`

## songster

**Raspberry Pi at parents' house.** Runs Ubiquiti UniFi network controller.

- `ssh songster` (Tailscale IP: 100.84.34.106)
- Mostly dormant — used for managing parents' UniFi Wi-Fi setup

## pixel

**Ethan's Pixel phone.** Resilio Sync peer; occasional file-management target.

- `ssh pixel` (Termux sshd, port 8022, configured in ~/.ssh/config; Tailscale IP: 100.115.153.79)
- **No root** — `/data/data/...` (app configs incl. Resilio's) and `/sdcard/Android/data` are inaccessible; only shared storage is visible

### Resilio shares (`/storage/emulated/0/Sync/`)

| Share | Mac counterpart | Notes |
|-------|-----------------|-------|
| `Documents` | `~/Documents` | **Selective sync ON** — pixel `archive/` is intentionally sparse; `inbox/` stays current |
| `org` | `~/org` | Selective sync off |
| `.keepass` | — | `kp2.kdbx` password DB |
| `digital-library` | `~/digital-library` | Selective sync (partial on pixel) |
| `.eitanveleah`, `Fonts` | — | |

- Scans from **Files by Google** land in `/storage/emulated/0/Files by Google/Scanned` → move to `Sync/Documents/inbox/` for `/process-inbox`
- Cleaned June 2026: stale duplicate share copies (`/storage/emulated/0/Documents`, `Download/Sync/*`) verified against live copies and deleted — don't recreate
- May be offline for extended periods (last seen can be weeks)
