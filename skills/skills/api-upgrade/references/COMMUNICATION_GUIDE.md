# Communication Guide

This guide provides examples of effective agent-user communication during API upgrades.

## Contents
- Principle: Condensed Analysis with Options
- Principle: Highlight Critical vs Informational
- Principle: Present Findings, Then Pause
- Principle: Quantify and Categorize

---

## Principle: Condensed Analysis with Options

**Good Example:**
```
Found 47 type errors across 3 categories:
• 31 null-safety issues in product queries (fields now nullable)
• 12 renamed field references (discountCode → discountCodes)
• 4 removed enum values (LEGACY_PROMO no longer exists)

Options:
1. I can attempt automated fixes for null-safety and renames
2. I can fix specific categories with your guidance
3. You handle manually
```

**Bad Example:**
```
I found 47 type errors. Let me start fixing them now. First, I'll add null
checks to all the product queries. Then I'll update the discount code references...
[proceeds without asking]
```

**Why this matters:** Users need to understand the scope and make informed decisions. Don't auto-fix without approval.

---

## Principle: Highlight Critical vs Informational

**Good Example:**
```
Changelog Analysis (2025-04 → 2025-07):

CRITICAL - Action Required (2 items):
• Cart.discountCodes now requires array instead of string
• Product.variants.inventoryQuantity removed, use inventoryItem instead

Breaking but not flagged (1 item):
• Metafield.value can now be null in certain conditions

Informational - New features (3 items):
• New deliveryEstimate field available on Cart
• ProductBundle type introduced
• SEO keywords now supported
```

**Bad Example:**
```
Here are all 15 changelog items I found:
1. deliveryEstimate field added
2. Cart.discountCodes changed to array
3. ProductBundle type added
[...continues with undifferentiated list]
```

**Why this matters:** Users need to quickly identify what requires immediate attention vs what's informational. Prioritization is essential for efficient decision-making.

---

## Principle: Present Findings, Then Pause

**Good Example:**
```
Snapshot Analysis - 12 queries tested:

Concerning changes (need review):
• ProductQuery: price field now returns null for 3/10 products
• CartQuery: discountCodes structure changed from string to array

Expected changes (likely safe):
• Product metafields: new fields added (seo.keywords, bundle.items)
• All queries: updated API version in response metadata

Should I update snapshots and proceed, or do these changes need investigation first?
```

**Bad Example:**
```
Tests show some differences but they look fine. Updating snapshots now...
```

**Why this matters:** Snapshot diffs are the primary way to detect API changes. Always present findings and get explicit approval before updating.

---

## Principle: Quantify and Categorize

**Good Example:**
```
Type regeneration complete. Changes summary:

Files modified: 23
Breaking changes: 8 files
  - 5 files: fields now nullable
  - 2 files: fields renamed
  - 1 file: type removed
Additive only: 15 files (new optional fields)

Most impacted: CartInfoQuery.ts, FullProductData.ts, VariantInventoryData.ts

Ready to review diffs?
```

**Bad Example:**
```
Types regenerated. Lots of changes. Take a look.
```

**Why this matters:** Concrete numbers and categories help users assess scope and prioritize review efforts.
