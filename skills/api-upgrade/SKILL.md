---
name: api-upgrade
description: >
  Provides systematic playbook for upgrading Shopify API versions (Admin or Storefront)
  between supported releases. Guides through changelog analysis, type regeneration,
  testing, and validation. Use when upgrading Shopify API versions, performing version
  bumps, or when user mentions Shopify API migration or version updates.
metadata:
  short-description: Shopify API version upgrade playbook
---

# Shopify API Upgrade Skill

## Overview

This skill describes a general process for upgrading Shopify versions
within Primary kids repos. This skill is designed to be
repository-agnostic. All repo-specific details (file locations,
commands, scripts, quirks) are describedin API_UPGRADE_CONTEXT.md
file(s). In the case of a regular repo, there will be a single
API_UPGRADE_CONTEXT.md file at the repo root; for monorepos there will
usually be one at the root and in individual packages.

## Collaboration Model

**User maintains control. Agent analyzes and presents, then pauses for direction.**

**Operating principles:**
- **Follow every step:**: Follow each step of the process without exception
- **Maintain ToDo list:** Display the full API upgrade checklist with checkboxes at every
- **Analyze, don't auto-fix:** Provide condensed analysis (counts, categories, examples) then ask
  major pause or checkpoint, and whenever asked.
- **Pause at decisions:** Before changes, when tests fail, when multiple paths exist
- **Offer options:** Present 2-3 choices including "I'll handle manually"
- **Condense information:** Use templates to highlight critical vs informational.
- **No commits w/out permission**
- **No file creation w/out permission**
- **No deployment w/out permission** 

See [`COMMUNICATION_GUIDE.md`](references/COMMUNICATION_GUIDE.md) for concrete
examples of effective communication patterns.

### Progress Tracking

Copy this checklist and check off items as you complete them:

```
API Upgrade Progress:
- [ ] Pre-flight: Verify repository readiness
- [ ] Step 0: Determine FROM and TO versions
- [ ] Step 1: Read changelog and assess changes
- [ ] Step 2: Survey query coverage gaps
- [ ] Step 3: Establish baseline on current version
- [ ] Step 4: Bump API version in all locations
- [ ] Step 5: Regenerate types and review diffs
- [ ] Step 6: Run integration tests and review snapshot diffs
- [ ] Step 7: Typecheck codebase and categorize errors
- [ ] Step 8: Manual and E2E validation
- [ ] Step 9: Summarize and communicate results
```

Mark items as you progress to keep the user informed.

---

## Pre-flight Checklist

Before beginning, verify the repository is ready:

### Repository Context
- [ ] **API_UPGRADE_CONTEXT.md exists**:
  - Check `<REPO_ROOT>/API_UPGRADE_CONTEXT.md`.
  - If monorepo mode, check only listed/affected package dirs for `API_UPGRADE_CONTEXT.md`, recurse as needed.
  - Load context files progressively (on use, not all at once).
  - If missing, create using [API_UPGRADE_CONTEXT_TEMPLATE.md](references/API_UPGRADE_CONTEXT_TEMPLATE.md) or ask user.
  - Examples:
    ```
    /repo-root/
      API_UPGRADE_CONTEXT.md # Describes monorepo mode, common tools, impacted packages.
      packages/
        worker/
          API_UPGRADE_CONTEXT.md
    ```

### Package Focus

**Each package = full process.** In monorepos, execute Steps 1-7 independently for each affected package/extension. State current package at each step. Scope commands to that package. Confirm before switching packages.

### Git Repository State
- [ ] **Clean working directory**: Run `git status` - commit or stash changes
- [ ] **Check for existing upgrade work**: Examine branch name and recent commits for API upgrade indicators (version patterns, "regenerate types", changes to `.env.api`). If found, analyze state and offer to resume or start fresh.

### Required Tools
- [ ] **jq**: Test with `jq --version` (install: `brew install jq`)
- [ ] **curl**: Test with `curl --version`
- [ ] **Node.js & yarn/npm**: Verify versions meet project requirements
- [ ] **TypeScript**: Test with `npx tsc --version`

### Script Usage

**Available Scripts:**

#### `get-shopify-api-versions.sh`

Queries Shopify API to list available versions.

**Usage:**
```bash
./scripts/get-shopify-api-versions.sh <env-file> <api-type>
```

**Arguments:**
- `env-file` - Path to environment file
- `api-type` - Either `admin` or `storefront`

**Example:**
```bash
./scripts/get-shopify-api-versions.sh .env.api admin
```

**For Admin API** (`api-type=admin`), env file must contain:
- `SHOPIFY_ADMIN_API_TOKEN` - Admin API access token
- `SHOPIFY_SHOP_NAME` - Shop domain (e.g., `mystore.myshopify.com`)
- `SHOPIFY_API_VERSION` - Current version (e.g., `2025-10`)

**For Storefront API** (`api-type=storefront`), env file must contain:
- `STOREFRONT_ACCESS_TOKEN` - Storefront API access token
- `GRAPHQL_URL` - Full GraphQL URL (e.g., `https://mystore.myshopify.com/api/2025-04/graphql.json`)

**Output:**
- Lists all versions with support status
- Shows current version, latest stable, and recommended target (one behind latest)

**Note:** Repo-specific scripts may be described in `API_UPGRADE_CONTEXT.md` and should be preferred when available.

### Environment & Dependencies
- [ ] **Environment files**: `.env.*` files exist with required API credentials (see `API_UPGRADE_CONTEXT.md` for specifics)
- [ ] **Dependencies installed**: Run `yarn install`
- [ ] **Unit tests are passing**: Run unit test command from [`API_UPGRADE_CONTEXT.md`](references/API_UPGRADE_CONTEXT.md)

---

**Resolve any failures before proceeding.** A clean starting state is critical for
tracking upgrade-specific changes. Once verified, proceed to Step 0.

---

## 0. Determine FROM and TO Versions

See [`API_UPGRADE_CONTEXT.md`](references/API_UPGRADE_CONTEXT.md) for version locations and scripts.

1. **Detect current (FROM) version** from env files/configs
2. **Confirm target (TO) version** with user if not specified
3. **Query available versions** using repo script or Shopify changelog
4. **⚠️ STOP - User Approval Required**
   - Present FROM and TO versions
   - If multiple versions behind, offer: stepwise, direct to latest-minus-one, other
   - Do NOT proceed without explicit confirmation

---

## 1. Read the Changelog and Version Policy

1. **Always**: Query Shopify changelog (see `API_UPGRADE_CONTEXT.md` for API type)
   - Admin API: [changelog](https://shopify.dev/changelog?filter=api&api_type=admin-graphql)
   - Storefront API: [changelog](https://shopify.dev/changelog?filter=api&api_type=storefront-graphql)
2. Filter with `action_required=true` for breaking changes
3. **If available** also query the Shopify MCP server. Merge results to create fuller picture.
4. Search for repo-specific terms from [`API_UPGRADE_CONTEXT.md`](references/API_UPGRADE_CONTEXT.md)
   (e.g., cart, checkout, metafield, product)
5. Summarize using [`changelog-summary.md`](assets/changelog-summary.md) template

**⚠️ PAUSE - Review Changelog Findings**
- Present summary, highlight action-required and breaking changes
- Assess risk (Low/Medium/High)
- Await user decision

---

## 2. Survey Query Coverage Gaps

Compare tested queries (see [`API_UPGRADE_CONTEXT.md`](references/API_UPGRADE_CONTEXT.md)) to at-risk queries
from changelog. Check one-off/hardcoded queries too.

**⚠️ PAUSE - If Significant Gaps Found**
- List untested at-risk queries
- Recommend: add tests, accept risk, or manual validation
- Get user decision

---

## 3. Establish a Baseline on the CURRENT Version

- Verify version strings point to current (FROM) version (see [`API_UPGRADE_CONTEXT.md`](references/API_UPGRADE_CONTEXT.md))
- Baseline is NOT covered by preflight alone. You must:
  - **Update integration test snapshots on current version FIRST**
	- Snapshots drift between upgrades due to code changes. Updating now isolates
	  API changes from code drift.
  - Regenerate types on current version
  - Run unit tests and type checks to confirm clean baseline
  - For all commands see [`API_UPGRADE_CONTEXT.md`](references/API_UPGRADE_CONTEXT.md)

---

## 4. Bump the API Version

Update ALL version references per [`API_UPGRADE_CONTEXT.md`](references/API_UPGRADE_CONTEXT.md)
(env files, configs, deployment scripts, hardcoded endpoints).

---

## 5. Regenerate Types and Inspect Diffs

- Run type generation (command in [`API_UPGRADE_CONTEXT.md`](references/API_UPGRADE_CONTEXT.md))
- Analyze diffs, summarize using [`type-diff-analysis.md`](assets/type-diff-analysis.md)

**⚠️ PAUSE - Review Type Changes**
- Present analysis, highlight breaking changes
- Await user decision: proceed, adjust, or manual

---

## 6. Run Integration Tests and Review Snapshot Diffs

- Run tests (command in [`API_UPGRADE_CONTEXT.md`](references/API_UPGRADE_CONTEXT.md))
- Review snapshot diffs showing API response changes
- Flag problems: missing fields, unexpected nulls, structural changes

**⚠️ PAUSE - Review Snapshot Diffs**
- Present API response changes summary
- Highlight concerning changes
- User assesses: acceptable, needs investigation, or problematic
- If acceptable, update snapshots; if problematic, investigate first

---

## 7. Typecheck and Categorize Errors

- Run type checker (command in [`API_UPGRADE_CONTEXT.md`](references/API_UPGRADE_CONTEXT.md))
- Compare to baseline, categorize using [`error-categorization.md`](assets/error-categorization.md)

**⚠️ PAUSE - If Type Errors Exist**
- Present categorized errors with priorities
- Offer: automated fixes, guided, or manual
- Do NOT proceed until critical errors resolved or user approves risk

---

## 8. Manual and E2E Validation

- Run dev server, test key workflows per [`API_UPGRADE_CONTEXT.md`](references/API_UPGRADE_CONTEXT.md)
- Check field/type-dependent features and custom scripts

**⚠️ PAUSE - If Issues Found**
- Document issues (console errors, GraphQL failures, UI breaks)
- Assess severity, present findings
- Get user decision on remediation

---

## 9. Summarize and Communicate

Compile summary using [`upgrade-summary.md`](assets/upgrade-summary.md):
- Versions (FROM → TO)
- Key changes (breaking/additive/deprecation)
- Type diffs, test results
- Open items and gaps

---

## Multi-Session Tracking

Create `/tmp/<repo>-api-upgrade-<from>-to-<to>/UPGRADE_STATUS.md` to track progress across packages and sessions. Update after each package completes.

---

## Rollback and Recovery

If the upgrade needs to be abandoned:

### Full Rollback
```bash
git checkout main
git branch -D api-upgrade-branch-name  # Delete upgrade branch
```

### Partial Rollback (Save Progress)
```bash
git add .
git commit -m "WIP: API upgrade to [TO_VERSION] - paused"
git push origin api-upgrade-branch-name
git checkout main
```

### Fix Corrupted Artifacts
- **Types**: Delete `src/types/gql/`, verify `.env.*` version, regenerate
- **Snapshots**: Delete snapshots, run `yarn test:integration -u` on correct version

---

## Behavioral Guardrails

- **Target "latest stable minus one"** - Shopify's "stable" isn't always stable; one-version buffer provides safety
- **No unrelated refactoring** - Only API bump and required fixes
- **Update all version references** - Per [`API_UPGRADE_CONTEXT.md`](references/API_UPGRADE_CONTEXT.md)
- **Pause for user approval** - At all decision points, before changes, when issues arise
- **API_UPGRADE_CONTEXT.md is authoritative** - If conflicts arise, follow repo-specific docs
