# Setting up a project

Interview the user one question at a time; propose a sensible default with each. Write the result to `~/.config/time-tracking/projects/<project>.md` using the template below, show it, and get a yes.

Ask about:
1. **Harvest** — which project, which tasks, and how to reach Harvest from this machine (connector or API token env var). Look the IDs up; don't ask for them.
2. **Scope** — what counts and what doesn't, especially near-misses (sibling repos, related side projects). Write these as concrete examples, since they are what future runs will be judged against.
3. **Sources** — where evidence of this work lives. For each: its type, its role (timing or labeling), and how to reach it from this machine. If a type has no recipe in `references/sources/`, write one (short: how to query, what to return) after confirming it works.
4. **Budget** — total hours, period, and whether a cut or rounding applies.
5. **Delivery** — where a scheduled run sends its draft.

## Template

```markdown
# <Project>
harvest: project <id>; tasks: Coding <id>, Project Management <id>, Research <id>; access: <connector | env HARVEST_TOKEN>
task guide: <which kinds of work go under which task>
budget: <hours> h, <start>–<end>
timezone: <tz>
counting: gap_minutes 45, lone_minutes 5
deliver: <channel for scheduled drafts>

## Scope
Counts: …
Does not count: …

## Sources
- <type> (timing|labeling): <where/how, e.g. host, filters, topic ids and what each topic is>
```
