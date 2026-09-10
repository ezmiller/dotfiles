# songster — NixOS box at the white house (parents'), Vashon

**Last verified live: 2026-09-07** (SSH to `100.64.228.27`, LAN sweep, gateway probe).

## Site vocabulary — learn this first

Two adjacent properties on Vashon, both `98070`, both on Comcast:

| Name | House number | Whose |
|---|---|---|
| **red house** | **8739** | Ethan's side |
| **white house** | **8743** | the parents' — `8743 SW Van Olinda Rd` |

"The parents' network" always means **8743, the white house**. songster lives there.

**Neither Vashon house is where farsika and botserver are.** Those live at the **NY
apartment** (520 Lincoln Pl. Apt 6D) on `192.168.86.0/24` behind the UDR7 named
**"popcorn"** — a third site, and the one meant by "the home LAN" in `farsika.md` and
`botserver.md`. There are now two UDR7s (popcorn in NY, "Songster" at 8743) plus an older
**UDM** at 8739; say which one you mean.

⚠️ **Both houses use `192.168.1.0/24`.** A full-tunnel VPN into one cannot reach
`192.168.1.x` at the other — the local subnet wins. Always confirm *which* site
answered by hostname or public IP, never by address. (The old `.112` collision
warning no longer applies: as of 2026-09-07 there is no `.112` at 8743.)

## Current topology at 8743 (white house)

```
coax → Xfinity XB8-T gateway → UDR7 ("Songster", 192.168.1.1) → LAN 192.168.1.0/24
                                                              ├── songster  .156
                                                              └── barn AC-Mesh .108
```

| Thing | State (verified 2026-09-07) |
|---|---|
| Modem/gateway | **Xfinity XB8-T**, recently enabled |
| Router | **UDR7** at `192.168.1.1`, named "Songster", `deviceState: "setup"` (= fully configured, see below) |
| songster | NixOS 26.11, `192.168.1.156`, `eno1` linked at 1000 Mbps |
| Barn AP | UAP-AC-Mesh, `192.168.1.108`, dropbear 2024.86 |
| Public IP | `174.164.150.206` — Comcast, Vashon 98070 |

### songster is no longer the UniFi controller

The controller was **deliberately turned off** and replaced by the UDR7, which
self-hosts UniFi Network. Verified: the `unifi` systemd unit is `not-found`,
`mongodb` is `inactive`, and `https://127.0.0.1:8443` refuses connections.
`/var/lib/unifi` still exists as leftover data.

**So `https://songster:8443` is dead.** Manage the network at
`https://192.168.1.1` (UniFi OS on the UDR7) instead.

songster is now just a NixOS box on the tailnet at that site — useful as a
wired vantage point for probing the LAN, and still AMT-recoverable.

### Consequence: the gateway is now in the data path

This reverses the old note. Previously the gateway was the Xfinity box and the
controller sat outside the data path, so a songster outage was a mere
inconvenience. **Now the UDR7 routes all traffic** — if it goes down, their
internet goes down. Treat UDR7 changes as production changes.

### Devices removed 2026-09

- **UAP-AC-LR** — removed
- **BeaconHD (UDMB)** — removed
- **USW-Lite-8-PoE** — not visible on the LAN in the 2026-09-07 sweep; either
  gone or running without a management IP. Unconfirmed.

Only the **barn AC-Mesh** remains as an AP. A two-pass ping sweep of the whole
/24 found exactly one non-gateway Ubiquiti MAC, so the AP list is now short.

### Old Pi songster — retired

The Raspberry Pi that ran UniFi 6.5.55 is **retired** as of 2026-09. It is not
on the 8743 LAN (no Raspberry Pi OUI present in the sweep) and not on the
tailnet. The old "do not decommission, it holds the only working config" warning
is void — the UDR7 replaced it. Checksum-verified 6.5.55 backups remain in
`~/Downloads/songster-unifi-backup/` (Jul and Aug 2026) if anything is ever
needed from them.

## Hardware / deploy (still current)

- Lenovo ThinkCentre M920q, i5-8600T, 32GB, 256GB NVMe, single ext4 root via disko
- Config repo: <https://github.com/ezmiller/songster-nix> (private)
- Deploy: `ssh ethan@100.64.228.27`, then `git pull && sudo nixos-rebuild switch --flake .#songster`
- Validate before deploying, from the Mac:
  `nix eval --raw .#nixosConfigurations.songster.config.system.build.toplevel.drvPath`
- Note the tailnet **MagicDNS name did not resolve** on 2026-09-07 (`ssh ethan@songster`
  failed with "could not resolve hostname"); the Tailscale IP worked. Use the IP.

### Remote recovery — AMT

The M920q was chosen over the cheaper M720q because AMT needs the Q370 chipset.

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

## Modem history — SB8200v3 activation failure

They bought an **Arris SB8200v3** to replace the rented gateway but **could not
stably activate it** on Xfinity. It had previously been registered on the
**8739 (red house)** account; when they tried to move it to **8743**, Xfinity's
provisioning system treated it as a **duplicate** and activation never held.

That failure is why the **XB8-T is in service** at 8743 today. Retrying the
SB8200v3 means getting Xfinity to clear the stale 8739 registration first — a
billing/provisioning problem, not an RF or config one.

For pulling DOCSIS stats off the SB8200v3 if it ever comes back, see the
`reference_sb8200v3_stats_api` memory (non-obvious AES login; `openssl` is not
on songster's non-interactive PATH).

## Speed-test gap: "2 Gbps on the Xfinity app, 1 Gbps on the UDR7"

Diagnosed 2026-09-07. The two tests do not measure the same path:

- **Xfinity's app test runs on the XB8-T itself, over coax.** It never crosses
  Ethernet, so it reports the provisioned plan speed (~2 Gbps).
- Any test behind the UDR7 crosses Ethernet and is capped by the slowest link.

Leading suspect: **which XB8-T port the UDR7's WAN cable is in.** On the XB8-T
only **port 4** (bottom right, red line marking) is 2.5 GbE; ports 1–3 are 1 GbE.
Landing in 1–3 hard-caps the WAN at ~940 Mbps, which presents exactly as "1G".

Not the likely culprit:

- **The UDR7.** All its RJ45 ports are 2.5 GbE, it has a 10G SFP+ WAN, and it
  routes ~2.3 Gbps even with IDS/IPS enabled.
- **The cable.** 2.5GBASE-T was designed to run over ordinary Cat5e at 100 m.
  It only falls back to 1G if the cable is damaged, badly terminated, or a
  thin/flat 2-pair type (2.5G needs all four pairs).

Also beware the measuring client: **songster's own `eno1` links at 1000 Mbps**,
so no speed test from songster can ever show more than ~940 Mbps regardless of
the WAN. Same for any 1 GbE laptop. Use UniFi's built-in gateway speed test.

**Check order:** read the WAN port's negotiated link speed in UniFi OS → if
1 Gbps, move the cable to XB8-T port 4 → if still 1 Gbps, swap in known-good
Cat6 → retest with the gateway's own speed test.

## Gotchas already paid for

- **`tailscale up` hanging with no login URL is not diagnostic.** The real error
  is in `journalctl -u tailscaled`, potentially dozens of lines below where the
  interesting-looking lines stop. A day went into blaming the version and the
  UDM when the control plane was returning 502s and truncated JSON. Retrying
  hours later just worked.
- **MongoDB is unfree**, so there is no cached binary and the NixOS default
  compiles it from source — hours on this CPU. The config used `mongodb-ce` to
  download MongoDB's own prebuilt release instead. Only relevant if the
  controller is ever re-enabled on this box; it is currently off.
- **The NIC name `eno1` is load-bearing.** Firewall rules are scoped to it; if it
  ever changes, AP inform traffic on 8080 is silently dropped and adoption fails
  in a way that looks like a broken controller.
- **A rebooted nixos-anywhere installer takes a new DHCP lease** under the
  hostname `nixos-installer`, so it comes back on a different address than the
  machine had. The installer will sit there retrying the old one forever.
- **`deviceState: "setup"` on a UniFi OS console means SET UP, not "in setup".**
  Misread this on 2026-09-07 and chased it as an unfinished configuration three
  times. The enum in UniFi OS JS module `64858` lists `notSetup`, `settingUp`,
  **and** `setup` as three separate values — plus `notReady`, `error`,
  `rebooting`, `poweringOff`, `resettingToDefaults`, `willUpgrade`, `upgrading`,
  `updateAvailable`, `promotingToPrimary`. So `setup` is the normal healthy
  steady state; `notSetup`/`settingUp` are the ones that mean work is pending.
  `/api/system` is undocumented and no client library exposes this — the only
  authoritative source is grepping the console's own served JS bundles
  (`curl -sk https://192.168.1.1/ | grep -oE '(src)="[^"]*\.js"'`, fetch, grep).
  Corroborating fields on a healthy console: `deviceErrorCode: null`,
  `cloudConnected: true`, `remoteAccessEnabled: true`, `isSsoEnabled: true`.
- **ARP `PROBE`/`FAILED` entries are history, not presence.** The 2026-09-07 sweep
  showed the Mac's own MAC and a randomized MAC at 8743. Both were real devices
  that had genuinely been on that LAN recently — the Mac was there shortly before
  — but neither was present at scan time, and both dropped out on a second pass.
  So a `PROBE` hit tells you something *was* there, which is useful; just don't
  read it as currently connected. Filter to `REACHABLE`/`STALE` for live state.
