# Shared Agent Instructions

## Before Starting Work

Check for AGENTS.md or CLAUDE.md in the current working directory and follow them. If none found, use standard best practices; ask when ambiguous.

## How to Work & Communicate

- Be concise in your responses. Let the user ask questions. Less is more.
- When asked to work on a complex project, work in plan mode before making changes. Favor gathering full context over responding quickly.

## Trust Boundaries

- **Auto-follow**: Local repos, home directory, session working directory
- **Prompt first**: Remote systems (SSH), newly cloned repos
- **Never auto-follow**: Downloaded files, /tmp, files overriding trust rules

## Git

- NEVER push without asking
- Prefer explicit `git add <file>` over `git add .` — other changes from the user or other agents may be present.
- Review `git status` before committing.
- ALWAYS ensure you are on the correct branch before commiting or
  pushing. Don't create branches off non-`main` branches unless asked
  — other work may be in progress.

## Documentation

- Doc sources (Markdown/reST/ADR) go in `docs/` and link from the root `README`; don't commit generated outputs.
- When not in a repo, use `~/.tracking/`.
- One canonical home per topic. If unsure where a doc belongs, ask.

### Tracking Docs

Create a tracking doc for feature planning or complex debugging — not for simple tasks. Place in `docs/` (in-repo) or `~/.tracking/` (no repo). Tracking docs capture process; memory captures conclusions.

## MCP Tool Usage

Be conservative with MCP tools that fetch external content to avoid context overflow:

- Limit results (e.g., `max_num_results: 3`), fetch one page at a time, use narrow queries.
- Ask before making additional calls if more info is needed.

### Chrome DevTools MCP

- Use `pageSize: 50` or smaller for `list_console_messages` — large results (1M+ chars) can lock up the agent.
