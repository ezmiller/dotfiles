# Jira Cross-Reference

Compare local journal state against live Jira ticket status. Useful for:
- Catching drift (you marked something DONE locally but forgot to update Jira, or vice versa)
- Sprint planning sanity checks
- Making sure your journal reflects reality before standup

**This is opt-in.** Don't automatically query Jira for this standalone check; the automatic
cross-system reconciliation lives in the morning briefing (`references/morning-briefing.md`,
Step 2.5). Run this recipe when Ethan explicitly asks to compare against Jira.

**Step 1: Extract ticket IDs from today's journal**

Scan today's journal for `EPD-XXXX` patterns. Collect unique ticket IDs from:
- Task headlines: `* TODO EPD-2466: Convert collectionQuery to GraphQL`
- Org tags: `:epd_2466:`
- Inline mentions in body text
- Links: `https://primary.atlassian.net/browse/EPD-2466`

**Step 2: Query Jira for each ticket**

Use the `mcp-atlassian__jira_get_issue` tool to fetch current status:
```
jira_get_issue(issue_key="EPD-2466", fields="summary,status,assignee")
```

For multiple tickets, batch them if possible or query in parallel.

**Step 3: Compare and report divergence**

Map org states to Jira statuses:
| Org state | Expected Jira statuses |
|-----------|------------------------|
| TODO | To Do, Backlog, Open |
| STARTED | In Progress, In Review |
| REVIEW | In Review, Code Review |
| DONE | Done, Closed, Resolved |
| BLOCKED | Blocked (if your Jira has this) |

**Output format:**

Pair every ticket ID with its summary — never show one bare (see "Always pair a ticket ID
with a short description" in `SKILL.md`). Pull the summary from the Jira `fields` you already
fetched.

```
## Jira Sync Check (5 tickets)

✅ In sync (3)
- EPD-2466 (Convert collectionQuery to GraphQL): STARTED locally, "In Progress" in Jira
- EPD-2481 (Pre-header logo, 1-column layout): STARTED locally, "In Progress" in Jira
- EPD-2482 (Full-width tablet/mobile nav): TODO locally, "To Do" in Jira

⚠️ Diverged (2)
- EPD-2378 (Discount code error messaging): DONE locally but "In Progress" in Jira
  → Maybe update Jira? Or reopen locally?
- EPD-2400 (Add INP metric): TODO locally but "Done" in Jira
  → Maybe mark DONE locally?
```

**What to do with divergence:**
- **Local ahead of Jira** — remind Ethan to update Jira (or offer to add a comment)
- **Jira ahead of local** (e.g. Jira shows `Done`) — **don't take that at face value.**
  Jira statuses are unpredictable (automations, bulk edits, stale syncs can flip a status
  without real work happening). Before suggesting Ethan mark the local task DONE, check
  git (`gh search prs`/`gh pr view`) for a merged PR that backs it up. If git doesn't
  confirm it, surface it as "Jira says Done but I can't find a merged PR — worth
  double-checking" rather than as settled fact.
- **Ambiguous** — just surface it, let Ethan decide

**Don't auto-fix.** Always surface the divergence and let Ethan choose what to do. Jira
is the system of record for the team, but its status field alone is not reliable enough to
assert as fact — git is the tiebreaker. Local journal is personal working memory. They
serve different purposes and occasional drift is normal.

**Bonus: tickets in Jira but not in journal**

If Ethan asks "what am I missing", query his assigned tickets in the current sprint:
```
jira_search(jql="assignee = currentUser() AND sprint in openSprints()")
```

Compare against tickets mentioned in recent journals. Surface any that are assigned to him
but have no local journal entry — these might need attention.
