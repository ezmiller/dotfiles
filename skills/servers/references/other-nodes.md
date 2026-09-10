# Other nodes — moltbot-aws, pixel

## moltbot-aws — RETIRED / TERMINATED (2026-09-08)

**Gone. Do not try to SSH to it.** The EC2 instance was terminated on 2026-09-08
after sitting stopped for ~5 months. The `Host moltbot-aws` block in
`~/.ssh/config` is commented out; the Tailscale node (`100.97.168.36`) is stale
and should be deleted from the tailnet admin console if it still appears.

- Instance was `i-0bc24ceba3628caaf` (t3.medium, us-east-1), stopped since ~2026-04
- **Final disk snapshot: `snap-0794268763c822c34`** (30 GiB, tagged
  `moltbot-aws-final`) — the only surviving copy of that box. Restore it as a
  volume and attach it to a throwaway instance if anything is ever needed back.
  Delete the snapshot once you are sure nothing is.
- Infra repo (still on disk): `~/Projects/moltbot-aws-terraform` — its state no
  longer matches reality, since the instance was terminated by hand, not by
  `terraform destroy`.
- All agents had already migrated to botserver as of 2026-04-10
- The pre-migration backup that used to live at
  `~/.openclaw/openclaw.json.pre-kingkong-migration` now only exists inside the
  snapshot above.

## songster — moved to `references/songster.md`

It outgrew this file. It is now a ThinkCentre M920q on NixOS, and the old
Raspberry Pi still exists and is still serving the parents' network, so there are
**two** machines by that name. The old Tailscale IP (`100.84.34.106`) is dead and
that node was deleted; the new one is `100.64.228.27`.

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
