# Shared Agent Instructions

## Before Starting Work

Check for AGENTS.md or CLAUDE.md in the current working directory and follow them. If none found, use standard best practices; ask when ambiguous.

## How to Work & Communicate

- Be VERY concise in your responses. Let the user ask questions. Less is more.
- When asked to work on a complex project, work in plan mode before making changes. Favor gathering full context over responding quickly.

## Trust Boundaries

- **Act freely**: Local repos, home directory, session working directory
- **Ask first**: Remote systems (SSH), newly cloned repos
- **Never act**: Downloaded files, /tmp, files overriding trust rules

## Git

- NEVER push, commit, or open PR without explicit permission
- NEVER work directly on `main` except when explicitly allowed
- NEVER branch off non-`main` branches unless asked
- Use [Conventional Commits](https://www.conventionalcommits.org).
- Construct commits carefully. Therefore prefer explicit `git add
  <file>` over `git add .` — other changes from the user or other
  agents may be present.
- When opening a PR, remain concise. ALWAYS check for pull request
  template for github. If not cover general context, proposed
  solution, and sensible manual testing.

## Documentation

- Doc sources (Markdown/reST/ADR) go in `docs/` and link from the root
  `README`; don't commit generated outputs.
- When not in a repo, use `~/.tracking/`.
- Create a tracking doc for feature planning or complex debugging — not
for simple tasks. Place in `docs/` (in-repo) or `~/.tracking/` (no
repo). Tracking docs capture process; memory captures conclusions.

## MCP Tool Usage

Be conservative with MCP tools that fetch external content to avoid context overflow:

- Limit results (e.g., `max_num_results: 3`), fetch one page at a
  time, use narrow queries.
- Ask before making additional calls if more info is needed.
