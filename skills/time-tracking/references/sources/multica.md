# Multica tickets (labeling)

Ticket codes (e.g. `PER-72`) appear in chats and PRs. Look up each code's title once per run:

```sh
multica issue get <CODE> --output json   # use the .title field
```

Run as a user whose Multica CLI is configured for the right workspace (the settings say which). Titles turn codes into plain-language notes; they never add time.
