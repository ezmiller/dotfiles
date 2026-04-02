# Bootstrap: Setting Up API Upgrade Skill for a New Repository

**Use this guide when**: You've copied the api-upgrade skill to a new repository and need to create its `REPO_CONTEXT.md` file.

## Contents
- Overview
- Step 1: Verify Need for Bootstrap
- Step 2: Repository Reconnaissance (detect API type, version locations, tests, type generation, dev server, special scripts)
- Step 3: Generate REPO_CONTEXT.md
- Step 4: Review and Confirm
- Step 5: Write File
- Notes

---

## Overview

The api-upgrade skill requires a `references/REPO_CONTEXT.md` file containing repository-specific information. This guide helps you create it through automated discovery and user input.

---

## Step 1: Verify Need for Bootstrap

Check if `references/REPO_CONTEXT.md` exists:
- **If exists**: You don't need this guide - proceed to normal Pre-flight Checklist
- **If missing**: Continue with this bootstrap process

---

## Step 2: Repository Reconnaissance

Auto-detect repository structure and conventions:

### 2.1 Detect API Type
```bash
# Search for Shopify API references
grep -r "storefront" --include="*.ts" --include="*.js" --include="*.json" | head -5
grep -r "admin.*api" --include="*.ts" --include="*.js" --include="*.json" | head -5
```

**Ask user**: "Based on the search, this appears to use [Storefront/Admin/Both] API. Is this correct?"

### 2.2 Find Version Locations
```bash
# Look for env files
find . -name ".env*" -type f | grep -v node_modules

# Search for version strings in env files
grep -h "GRAPHQL_URL\|API_VERSION\|shopify.*api" .env* 2>/dev/null
```

**Ask user**:
- "Found these env files: [list]. Are there others?"
- "Version appears to be set via [variable name]. Correct?"
- "Are there any other files with hardcoded API versions (scripts, configs, etc.)?"

### 2.3 Identify Integration Tests
```bash
# Search for test files
find . -name "*test*" -o -name "*spec*" | grep -i integration

# Check package.json for test commands
cat package.json | jq '.scripts | to_entries[] | select(.key | contains("test"))'
```

**Ask user**:
- "Found integration tests at: [paths]. Are these snapshot-based?"
- "Command to run integration tests: [detected command]. Correct?"
- "Command to update snapshots: [detected command -u]. Correct?"

### 2.4 Find Type Generation
```bash
# Look for GraphQL codegen
cat package.json | jq '.scripts | to_entries[] | select(.key | contains("codegen") or .key | contains("generate") or .key | contains("gql") or .key | contains("graphql"))'

# Check for codegen config
find . -name "codegen.yml" -o -name "codegen.yaml" -o -name "graphql.config.*"

# Find type output directory
find . -type d -name "gql" -o -name "graphql" -o -name "generated" | grep -v node_modules | head -5
```

**Ask user**:
- "Command to generate types: [detected]. Correct?"
- "Types output to: [detected path]. Correct?"

### 2.5 Identify Dev Server & Manual Testing
```bash
# Find dev server command
cat package.json | jq '.scripts | to_entries[] | select(.key | contains("dev") or .key | contains("start") or .key | contains("serve"))'
```

**Ask user**:
- "Dev server command: [detected]. Correct?"
- "What are the critical user flows to manually test after upgrade? (e.g., checkout, product pages, cart)"

### 2.6 Discover Special Scripts
**Ask user**:
- "Are there any custom scripts or pages with hardcoded Shopify API versions?"
- "Any deployment scripts, CI/CD configs, or Docker files that reference API versions?"
- "Any external integrations or proxy scripts we should know about?"

---

## Step 3: Generate REPO_CONTEXT.md

Using gathered information, create `references/REPO_CONTEXT.md` with this structure:

```markdown
# Repository-Specific Context

This document contains all [REPO_NAME] specific context needed to
perform a Shopify API upgrade. For general process, see SKILL.md.

## API Type
This repository uses the **Shopify [Storefront/Admin] API**.

## Version Location
- API versions are set in `.env.*` files using the variable `[VARIABLE_NAME]`
- [List any other locations with hardcoded versions]

## Version Query Script
[If the get-shopify-api-versions.sh script is relevant, include usage. Otherwise note "N/A - query versions manually via Shopify docs"]

## Integration Tests
- Integration test file(s): [paths]
- Run all integration tests: `[command]`
- Update integration test snapshots: `[command]`
- Integration tests are [snapshot-based/assertion-based] and validate [what they test]

## Type Generation / Codegen
- GraphQL types are generated via: `[command]`
- Type files are output to: `[path]`
- Type generation scripts and CI workflows: [locations]

## Manual QA and Developer Testing
- After passing integration tests, run the dev server: `[command]`
- Manually test:
  - [Critical flow 1]
  - [Critical flow 2]
  - [Critical flow 3]
- Check the browser console and network tab for GraphQL errors

## Special Scripts and Locations
[Document any custom scripts, hardcoded endpoints, or special files that need version updates]

## Common Gotchas
[Empty placeholder - to be populated during/after first upgrade]

### Past Upgrade Issues
<!-- Document problems encountered in previous upgrades and their solutions -->

### Environment-Specific Concerns
<!-- Note any differences between dev/integration/production environments -->

### Frequently Missed Locations
<!-- Track version references that are easy to overlook -->

### Type Generation Quirks
<!-- Document any oddities with the codegen process -->

## Other Notes
- [Any additional repo conventions relevant to this process]
```

---

## Step 4: Review and Confirm

**⚠️ PAUSE - User Review Required**

Present the generated `REPO_CONTEXT.md` to the user:
- Show the complete generated file
- Highlight sections where information was auto-detected vs user-provided
- Note any gaps or uncertainties

**Ask user**:
1. "Review the generated REPO_CONTEXT.md above. Does this look accurate?"
2. "Are there any missing details or corrections needed?"
3. "Should I write this file, or would you like to edit it manually first?"

---

## Step 5: Write File

Once approved, write `references/REPO_CONTEXT.md` with the generated content.

**Confirm**: "REPO_CONTEXT.md created. You can now proceed with the api-upgrade skill's Pre-flight Checklist."

---

## Notes

- **This is a one-time process** per repository
- **Update REPO_CONTEXT.md** as you learn more during upgrades (add to Common Gotchas, etc.)
- **Keep it accurate** - the upgrade skill relies on this information being correct
