# songster — UniFi controller for the parents' network

**There are currently TWO machines called songster.** Getting them confused has
already wasted time. Read this section before touching either.

| | New songster | Old songster (the Pi) |
|---|---|---|
| Hardware | Lenovo ThinkCentre M920q | Raspberry Pi, Raspbian 10 |
| OS | NixOS 26.11 | Raspbian |
| UniFi | 10.5.67 | 6.5.55 |
| Tailnet | `songster` @ **100.64.228.27** | **not on the tailnet** |
| SSH | `ssh ethan@songster` | `ssh pi@<their-lan-ip>` |
| Where | red house, Vashon (staging) | parents' house, serving their network |
| Status | built and verified, awaiting cutover | still live and in production |

The Pi is **not** dead and must not be decommissioned until the new machine has
adopted everything — it holds the only working copy of the configuration. Its
Tailscale key expired 2026-02-26, which is the only reason it looked dead; that
node was deleted, so returning it to the tailnet needs someone at its keyboard.

## New songster

- Config repo: <https://github.com/ezmiller/songster-nix> (private)
- Deploy: `ssh ethan@songster`, then `git pull && sudo nixos-rebuild switch --flake .#songster`
- Validate before deploying, from the Mac: `nix eval --raw .#nixosConfigurations.songster.config.system.build.toplevel.drvPath`
- i5-8600T, 32GB, 256GB NVMe, single ext4 root via disko
- Admin UI: `https://songster:8443` (tailnet) — self-signed cert, warning expected

### Remote recovery — AMT

This machine has Intel vPro/AMT, which is why the M920q was chosen over the
cheaper M720q (AMT needs the Q370 chipset).

- Web UI `http://<lan-ip>:16992`, user `admin`, password in KeePass
- Remote power on/off/cycle — **tested working**
- Remote console including BIOS, and remote repair-image mounting — *untested*
- **The NixOS firewall cannot restrict AMT.** The Management Engine sees those
  packets before the OS does. The MEBx password is the only control.
- User consent is deliberately disabled, so no one needs to read a code off a
  monitor that isn't plugged in.
- Wired ethernet only. AMT is useless over Wi-Fi.

### Key expiry — the lesson of this whole project

Key expiry is **disabled** on the songster node, and must stay that way. An
expired key is what silently removed the Pi from the tailnet for 173 days. It
does not survive deleting and re-registering a node — re-check it after either.

## Parents' network

Gateway is the **Xfinity/Netgear box, not UniFi** — so the controller is out of
the data path. If songster is down, their Wi-Fi and wired traffic keep working;
only management and stats stop. An outage here is an inconvenience.

Four UniFi devices, all on 2022-era firmware:

| Model | Device | Firmware |
|---|---|---|
| `USL8LP` | USW-Lite-8-PoE switch | 7.4.1.16850 |
| `U7LR` | UAP-AC-LR | 6.8.2.15592 |
| `U7MSH` | UAP-AC-Mesh | 6.8.2.15592 |
| `UDMB` | BeaconHD (wall-plug extender) | 6.7.17.15512 |

⚠️ **The parents' LAN and the red house LAN are both `192.168.1.0/24`, and both
have a `.112`.** This has already caused a misidentification — the old Pi
answered when the new machine was expected. It also means a full-tunnel VPN into
one site cannot reach `192.168.1.x` at the other; the local subnet wins.

## Cutover plan

Reconfigure by hand; **do not restore the backup**. The Pi runs UniFi 6.5.55
against the new machine's 10.5.67 — four major versions, not a supported restore
path. Only four devices to redo.

Backups were pulled anyway and checksum-verified, in
`~/Downloads/songster-unifi-backup/` (6.5.55, Jul and Aug 2026).

Before the cutover, check the four devices against UniFi 10.5's supported-device
list — the AC-series APs and BeaconHD are legacy.

## Gotchas already paid for

- **`tailscale up` hanging with no login URL is not diagnostic.** The real error
  is in `journalctl -u tailscaled`, potentially dozens of lines below where the
  interesting-looking lines stop. A day went into blaming the version and the
  UDM when the control plane was returning 502s and truncated JSON. Retrying
  hours later just worked.
- **MongoDB is unfree**, so there is no cached binary and the NixOS default
  compiles it from source — hours on this CPU. The config uses `mongodb-ce`,
  which downloads MongoDB's own prebuilt release instead. The trade is that
  it is 8.2.x against Ubiquiti's tested 7.0/8.0; suspect it first if the
  controller misbehaves in database-shaped ways.
- **The NIC name `eno1` is load-bearing.** Firewall rules are scoped to it; if it
  ever changes, AP inform traffic on 8080 is silently dropped and adoption fails
  in a way that looks like a broken controller.
- **A rebooted nixos-anywhere installer takes a new DHCP lease** under the
  hostname `nixos-installer`, so it comes back on a different address than the
  machine had. The installer will sit there retrying the old one forever.
