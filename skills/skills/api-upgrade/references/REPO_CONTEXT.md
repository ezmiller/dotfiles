# Repository-Specific Context

This document contains all pk-shopify-theme specific context needed to
perform a Shopify API upgrade. For general process, see SKILL.md.

## Contents
- API Type
- Version Location
- Version Query Script
- Integration Tests
- Type Generation / Codegen
- Manual QA and Developer Testing
- Bazaarvoice Proxy Script
- Common Gotchas (Past Upgrade Issues, Environment-Specific Concerns, Frequently Missed Locations, Type Generation Quirks)
- Other Notes

---

## API Type
This repository uses the **Shopify Storefront API** (not Admin).

## Version Location
- API versions are set in `.env.*` files using the variable
  `GRAPHQL_URL` (e.g., `/api/2025-04/graphql.json`)
- Search for occurrences of `GRAPHQL_URL` and other Shopify version
  references in env files and scripts.
- Sometimes, version strings may also appear in deployment or script
  files—double-check if any build or deploy processes set the API
  version.

## Version Query Script
- This repo provides [`scripts/get-shopify-api-versions.sh`](../scripts/get-shopify-api-versions.sh),
  a Bash script specialized for querying **Shopify Storefront API** versions using the
  `/api/publicApiVersions` endpoint.
- Usage: `./scripts/get-shopify-api-versions.sh [env_file]`
- The script reads API credentials from an env file (default: `.env.integration`).
- It outputs all available versions and highlights current, stable, and recommended targets
  following team policy.
- **If your repo uses the Admin API or another Shopify GraphQL API,** this script may need adaptation.
  See comments inside the script or Shopify documentation for endpoint/credentials changes.

## Integration Tests

- Integration test file(s):
  `src/__integrationTests__/shopifyAPI.test.integration.ts`
- Run all integration tests: `yarn test:integration`
- Update integration test snapshots: `yarn test:integration -u`
- Integration tests are snapshot-based and validate a set of critical
  Storefront API queries. They are the *primary* automated check
  post-upgrade.

## Type Generation / Codegen
- GraphQL types are generated via: `yarn update-gql-types`
- Type files are output to: `src/types/gql/`
- Type generation scripts and CI workflows:
  `.github/workflows/codegen.yml` (and check for any scripts in the
  `scripts/` directory)

## Manual QA and Developer Testing
- After passing integration tests, run the dev server: `yarn serve` or the repo’s blessed dev command
- Manually test:
  - Home page
  - Collection/product pages
  - Cart & mini-cart
  - Discount code flow
  - Any custom store flows (bundles, subscriptions, etc)
- Check the browser console and network tab for GraphQL errors

## Bazaarvoice Proxy Script
- The file [`shopify_scripts/bazaarvoiceproxy.html`](../shopify_scripts/bazaarvoiceproxy.html) is a hidden Shopify page that fetches all product IDs using a **hardcoded Storefront API version string** (e.g., `/api/2025-04/graphql.json`).
- **Any time you change the Storefront API version, you must manually update this file.**
- How it works:
    - The page (must be hosted on the primary.com domain) renders a div for each product, which the Bazaarvoice JS widget (`bv.js`) uses to display ratings.
    - An external AWS Lambda (see [`pk-workers-monorepo`](https://github.com/PrimaryKids/pk-workers-monorepo/tree/bazaar-voice-proxy)) loads and scrapes this page into S3 roughly every three hours, and also serves ratings data to the frontend app.
    - See architecture details: [BazaarVoice Proxy RFC (Confluence)](https://primary.atlassian.net/wiki/spaces/EN/pages/3581870085/BazaarVoice+Proxy+RFC).
- **This script currently depends on the following GraphQL query:**
    - `products { edges { node { id handle } } pageInfo { hasNextPage endCursor } }`
- **You should always check this script for what queries/fields it uses before upgrading, and scan for breaking changes in the API changelog that might affect this data.**

## Common Gotchas

This section documents known issues, quirks, and lessons learned from previous
API upgrades. **Populate this section during and after each upgrade** to build
institutional knowledge.

### Past Upgrade Issues
<!-- Document problems encountered in previous upgrades and their solutions -->
<!-- Example:
- **2024-07 → 2024-10**: Cart.discountCodes migration required updating 15+ files
  due to string → array change. Automated find/replace worked well.
-->

### Environment-Specific Concerns
<!-- Note any differences between dev/integration/production environments -->
<!-- Example:
- Integration environment uses cached GraphQL responses - may need cache clear
  after version bump to see real API changes
-->

### Frequently Missed Locations
<!-- Track version references that are easy to overlook -->
<!-- Example:
- Bazaarvoice script - always forgotten on first pass
- Docker compose files in `/deploy` folder also have hardcoded versions
-->

### Type Generation Quirks
<!-- Document any oddities with the codegen process -->
<!-- Example:
- Type generation sometimes fails if node_modules/.cache exists - delete before running
-->

## Other Notes
- If you find additional "repo conventions" relevant to this process,
  add another section here.
