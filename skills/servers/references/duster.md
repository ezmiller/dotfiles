# ethan-duster

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
