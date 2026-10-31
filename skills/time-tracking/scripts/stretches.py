#!/usr/bin/env python3
"""Group timestamped messages into work stretches.

stdin: lines "ISO-8601 time<TAB>label<TAB>text" (extra columns ignored).
stdout: one line per stretch: date, start-end, minutes counted, message count, labels.
"""
import argparse
import sys
from datetime import datetime

p = argparse.ArgumentParser()
p.add_argument("--gap-minutes", type=float, default=45)
p.add_argument("--lone-minutes", type=float, default=5)
a = p.parse_args()

msgs = []
for line in sys.stdin:
    parts = line.rstrip("\n").split("\t")
    if parts and parts[0]:
        msgs.append((datetime.fromisoformat(parts[0]), parts[1] if len(parts) > 1 else ""))
msgs.sort()

stretches = []
for t, label in msgs:
    if stretches and (t - stretches[-1][-1][0]).total_seconds() <= a.gap_minutes * 60:
        stretches[-1].append((t, label))
    else:
        stretches.append([(t, label)])

total = 0.0
for s in stretches:
    start, end = s[0][0], s[-1][0]
    mins = (end - start).total_seconds() / 60 or a.lone_minutes
    total += mins
    labels = ",".join(sorted({l for _, l in s if l}))
    print(f"{start:%Y-%m-%d}\t{start:%H:%M}-{end:%H:%M}\t{mins:.0f} min\tn={len(s)}\t{labels}")
print(f"total\t{total / 60:.2f} h")
