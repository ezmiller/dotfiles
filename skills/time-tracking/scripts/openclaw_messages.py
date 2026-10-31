#!/usr/bin/env python3
"""Print the user's messages from an OpenClaw agent DB, read-only.

Output: "ISO-time<TAB>topic<TAB>first 120 chars", oldest first.
"""
import argparse
import sqlite3
from datetime import datetime, timedelta
from zoneinfo import ZoneInfo

p = argparse.ArgumentParser()
p.add_argument("--db", required=True)
p.add_argument("--from", dest="start", required=True, help="YYYY-MM-DD, inclusive")
p.add_argument("--to", dest="end", required=True, help="YYYY-MM-DD, inclusive")
p.add_argument("--tz", default="America/Los_Angeles")
p.add_argument("--keys", nargs="+", required=True, help="SQL LIKE patterns for session_key")
a = p.parse_args()

tz = ZoneInfo(a.tz)
ms = lambda d: datetime.fromisoformat(d).replace(tzinfo=tz).timestamp() * 1000
lo, hi = ms(a.start), ms(a.end) + 86_400_000

NOISE = ("Watch fired", "Pre-compaction", "[OpenClaw heartbeat", "[cron:")
db = sqlite3.connect(f"file:{a.db}?mode=ro", uri=True)
where = " or ".join("w.session_key like ?" for _ in a.keys)
rows = db.execute(
    f"""select f.timestamp, w.session_key, f.text
        from session_transcript_fts f join session_windows w on w.session_id = f.session_id
        where f.role = 'user' and f.timestamp >= ? and f.timestamp < ? and ({where})
        order by f.timestamp""",
    (lo, hi, *a.keys),
)
for ts, key, text in rows:
    text = " ".join(text.split())
    if text.startswith(NOISE):
        continue
    t = datetime.fromtimestamp(ts / 1000, tz)
    print(f"{t.isoformat(timespec='minutes')}\t{key.split(':')[-1]}\t{text[:120]}")
