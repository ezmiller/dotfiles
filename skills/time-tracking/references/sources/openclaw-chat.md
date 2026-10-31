# OpenClaw chat (timing)

Messages the user sent to an OpenClaw agent (e.g. Telegram topics). Lives in the agent's SQLite DB (`session_transcript_fts` joined to `session_windows` for `session_key`; `timestamp` is epoch ms).

Run `scripts/openclaw_messages.py` **on the host that has the DB**, read-only. Over SSH, pipe it in rather than copying it:

```sh
ssh <host> 'sudo -u <db-owner> python3 - --db <path> --from YYYY-MM-DD --to YYYY-MM-DD --tz <tz> --keys <session-key-patterns>' < scripts/openclaw_messages.py
```

It prints one line per user message: `ISO-time<TAB>topic<TAB>first 120 chars`, skipping system noise (watch notices, memory flushes, heartbeats). Pipe that to `scripts/stretches.py`.

Lines like "[review-page] … Ethan acted on this packet's review page" are written by the system but record the user's own actions: keep them.

The settings give the `--keys` patterns. Scope by topic first (the settings list which topics belong to the project). Then drop individual off-topic lines by judgment — an in-scope topic still gets stray messages, and a general topic gets some in-scope ones. Edit the script's output before piping it to `stretches.py`.
