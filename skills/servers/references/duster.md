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
- **Sunshine** (game streaming) — see `~/org/techwork/tracking/general/duster_sunshine_setup.org` for X11/NVIDIA setup notes
- **Transmission** (system service, `transmission` user) — Web UI http://192.168.86.216:9091

### Gotchas
- **SSH:** the Mac can't resolve `ethan-duster` (MagicDNS off) and known_hosts keys it by name → use `ssh -o HostKeyAlias=ethan-duster ethan@100.126.203.96` (or the `duster` alias in ~/.ssh/config).
- **Restarting Steam remotely:** relaunching from SSH with just `DISPLAY`/`XAUTHORITY` attaches Steam to the SSH session and the screen **flickers** in Big Picture. Relaunch with the KDE desktop's full environment instead (`startplasma-x11` session 1):
  `xargs -0 -a /proc/$(pgrep -x plasmashell)/environ sh -c 'exec env -i "$@" setsid nohup ~/.local/share/Steam/steam.sh steam://open/bigpicture >/dev/null 2>&1 &' sh`
- **Custom Proton (GE-Proton):** unpack into `~/.local/share/Steam/compatibilitytools.d/`; Steam only sees it after a full restart. GE-Proton10-34 installed 2026-09-26 for MLB Rivals. Its GameGuard error 380 was really AdGuard parental control blocking qpyou.cn (see farsika.md), not Proton.
