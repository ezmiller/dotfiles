# API_UPGRADE_CONTEXT Template

<!--
AUTHORING GUIDANCE (do not include in actual files):

Context files share the context window. Challenge each piece: Does the model need this?
Can it be grepped? Does it justify its token cost?

Include: Version locations, commands, API type, changelog terms, non-obvious gotchas.
Exclude: Explanations, greppable patterns, test counts, framework details, one-time issues.
Format: Tables and code blocks over prose. One line per fact.
-->

# API Upgrade Context - [package-name]

For monorepo context: `<REPO_ROOT>/API_UPGRADE_CONTEXT.md`

## API Type & Changelog Terms

- **API**: [Admin API | Storefront API | REST API | Functions API]
- **Search terms**: [relevant Shopify terms for this package]

## Version Locations

- `path/to/file.ts:NN`
- `wrangler.toml` - Lines NN, NN, NN (prod, qa, dev)

**Post-update** (if any):
```bash
# Commands after version bump
```

## Type Generation

[Package-specific details, or "See monorepo root."]

## Commands

| Command | Purpose |
|---------|---------|
| `yarn test` | Unit tests |
| `yarn test:api` | API tests |
| `yarn typecheck` | Type checking |
| `yarn deploy-dev` | Deploy to dev |

## GraphQL Queries

- `src/graphql/*.graphql`

## Gotchas

[Non-obvious issues only. Delete if none.]

- **Issue**: How to avoid

## Past Upgrade Issues

[Reusable lessons only. Delete if none.]

### [Version] → [Version]

- **Issue**: Resolution
