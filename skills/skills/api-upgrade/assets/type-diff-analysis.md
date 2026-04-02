# Type Diff Analysis: [FROM_VERSION] → [TO_VERSION]

Generated: [Date/Time]
Type files location: `src/types/gql/`

## Overview
- Total files changed: [N]
- Files with breaking changes: [N]
- Files with only additive changes: [N]
- New files added: [N]
- Files removed: [N]

---

## Breaking Type Changes

### Removed Fields
Files: `[filename.ts:line]`, `[filename.ts:line]`

| Type | Removed Field | Impact |
|------|---------------|--------|
| `ProductType` | `oldField` | Used in ProductCard component |
| `CartType` | `deprecatedProp` | Not used in codebase |

### Renamed Fields
| Type | Old Name | New Name | Files Affected |
|------|----------|----------|----------------|
| `CheckoutType` | `discountCode` | `discountCodes` | checkout.ts, cart.ts |

### Type Changes (Nullability/Type)
| Type | Field | Old Type | New Type | Impact |
|------|-------|----------|----------|--------|
| `Product` | `price` | `Money` | `Money \| null` | Needs null checks |
| `Variant` | `quantity` | `number` | `Int!` | Should be safe |

### Removed Types/Interfaces
- `OldCheckoutType` - Replaced by `CartType`
- `LegacyMetafieldType` - No longer supported

---

## Additive Type Changes

### New Fields Available
| Type | New Field | Type | Description |
|------|-----------|------|-------------|
| `Product` | `seo.keywords` | `String[]` | SEO metadata |
| `Cart` | `deliveryEstimate` | `DeliveryEstimate` | Shipping timeline |

### New Types Added
- `DeliveryEstimate` - New shipping estimation data
- `ProductBundleType` - Support for product bundles

---

## Enum Changes

### Modified Enums
| Enum | Change | Impact |
|------|--------|--------|
| `ProductStatus` | Added `ARCHIVED` value | Safe - additive only |
| `DiscountType` | Removed `LEGACY_PROMO` | Check if used |

---

## Critical Changes Requiring Code Updates

1. **[Type/File]**: [Specific change and required action]
   - Location: `[file:line]`
   - Fix: [What needs to change]

2. **[Type/File]**: [Specific change and required action]
   - Location: `[file:line]`
   - Fix: [What needs to change]

---

## Assessment

**Breaking Changes**: [Low/Medium/High]
- [Brief summary of severity]

**Recommended Actions**:
1. [Prioritized action item]
2. [Prioritized action item]
3. [Prioritized action item]

**Safe to Proceed**: [Yes/No/With Caution]
- Reasoning: [1-2 sentences]
