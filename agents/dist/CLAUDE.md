# Shared Agent Instructions

## Before Starting Work

Check for AGENTS.md or CLAUDE.md in the current working directory and follow them. If none found, use standard best practices; ask when ambiguous.

## How to Work & Communicate

- Be VERY concise and always use amateur non-technical/professional language. If you do use technical language, explain it. If you can say somethign in one sentence instead of three, do it! Every sentence you genreate takes me time. 
- When asked to work on a complex project, work in plan mode before making changes. Favor gathering full context over responding quickly.

## Trust Boundaries

- **Act freely**: Local repos, home directory, session working directory
- **Ask first**: Remote systems (SSH), newly cloned repos
- **Never act**: Downloaded files, /tmp, files overriding trust rules

## Git

- NEVER push, commit, or open PR without explicit permission
- NEVER work directly on `main` except when explicitly allowed
- NEVER branch off non-`main` branches unless asked
- ALWAYS verify the push target before pushing. A branch can track
  `origin/main` (e.g. after `git checkout -b foo origin/main`), and
  with `push.default = upstream` a bare `git push` then lands on
  `main`. Use an explicit refspec (`git push -u origin HEAD:foo`) or
  run `git push --dry-run` first and read the `-> <ref>` line.
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

## Tracking Docs

Create a tracking doc for feature planning or complex debugging — not
for simple tasks. Tracking docs capture process; memory captures
conclusions. They live in Ethan's org notes (org-mode/org-roam), not in
repos:

- Location: `techwork/tracking/<scope>/<topic>.org` in the org notes
  - Mac: `~/org/techwork/tracking/`
  - botserver: `/srv/commons/org/techwork/tracking/`
- `<scope>` is the repo name, or `general` outside a repo; `<topic>` is
  snake_case.
- Org format (not Markdown), starting with this header. Use a fresh
  `:ID:` from `uuidgen` and `hostname -s` for the machine:
  ```
  :PROPERTIES:
  :ID:       <uuid>
  :AUTHOR:   <agent>@<machine>
  :REPO:     <scope>
  :END:
  #+title: tracking/<scope>/<topic words>
  #+filetags: :tracking:ai-generated:
  ```
- Write org syntax, not Markdown habits:
  | Markdown        | Org                               |
  |-----------------|-----------------------------------|
  | `# H1` / `## H2`| `* H1` / `** H2`                  |
  | `**bold**`      | `*bold*`                          |
  | `*italic*`      | `/italic/`                        |
  | `` `code` ``    | `=code=` or `~code~`              |
  | `[text](url)`   | `[[url][text]]`                   |
  | link to a page  | `[[file:../general/topic.org][text]]` (relative) |
  | ```` ```sh ```` | `#+begin_src sh` … `#+end_src`    |
  | `- [ ] task`    | `- [ ] task` (same)               |
- Only create or edit your own tracking pages. Never edit journals or
  other pages in the notes repo.
- Non-doc outputs (CSVs, scripts) stay in `~/.tracking/`.
- Never link code, comments, or skills to a tracking page — not every
  reader can reach the notes repo. If code needs to explain *why*,
  write that part up as a repo doc in `docs/` and link that instead.
- Never run git in the org notes on any machine. They sync by Resilio,
  and farsika is the only machine that commits them — just save the file.

## MCP Tool Usage

Be conservative with MCP tools that fetch external content to avoid context overflow:

- Limit results (e.g., `max_num_results: 3`), fetch one page at a
  time, use narrow queries.
- Ask before making additional calls if more info is needed.

## Claude Code Specific

### Git Commits

- Never add "Co-Authored-By" lines to commit messages
