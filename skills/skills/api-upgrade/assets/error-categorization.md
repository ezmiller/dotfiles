# Type Error Categorization

Version: [FROM_VERSION] → [TO_VERSION]
Generated: [Date/Time]
Command used: `[typecheck command from REPO_CONTEXT.md]`

## Summary
- **Total Errors**: [N]
- **New Errors** (vs baseline): [N]
- **Production Code Errors**: [N]
- **Test Code Errors**: [N]
- **Type Definition Errors**: [N]

---

## Error Categories

### 1. Null Safety Issues
**Count**: [N] errors
**Severity**: [High/Medium/Low]

Example errors:
```
src/components/Product.tsx:42:15 - error TS2531: Object is possibly 'null'.
src/utils/pricing.ts:89:22 - error TS2531: Object is possibly 'null'.
```

**Affected Files**:
- `[file:line]` - [Brief description]
- `[file:line]` - [Brief description]

**Root Cause**: [Field X is now nullable in API response]

**Fix Strategy**: [Add null checks / Use optional chaining / Provide defaults]

---

### 2. Renamed/Removed Fields
**Count**: [N] errors
**Severity**: [High/Medium/Low]

Example errors:
```
src/components/Cart.tsx:56:10 - error TS2339: Property 'discountCode' does not exist on type 'Cart'.
```

**Field Mappings**:
| Old Field | New Field | Occurrences |
|-----------|-----------|-------------|
| `discountCode` | `discountCodes` | 5 files |
| `removedField` | N/A (removed) | 2 files |

**Fix Strategy**: [Update field references / Remove usage / Use alternative]

---

### 3. Type Mismatches
**Count**: [N] errors
**Severity**: [High/Medium/Low]

Example errors:
```
src/hooks/useCheckout.ts:34:5 - error TS2322: Type 'string' is not assignable to type 'string[]'.
```

**Issues**:
- `[Field]`: Changed from `[OldType]` to `[NewType]` ([N] occurrences)
- `[Field]`: Changed from `[OldType]` to `[NewType]` ([N] occurrences)

**Fix Strategy**: [Update type annotations / Transform data / Adjust logic]

---

### 4. Removed Types/Interfaces
**Count**: [N] errors
**Severity**: [High/Medium/Low]

**Missing Types**:
- `[TypeName]` - Used in [N] files
  - Replacement: `[NewType]` or [migration strategy]
- `[TypeName]` - Used in [N] files
  - Replacement: `[NewType]` or [migration strategy]

**Fix Strategy**: [Import new types / Refactor to new structure]

---

### 5. Test-Only Errors
**Count**: [N] errors
**Severity**: [Low - does not affect production]

**Files**:
- `[test-file:line]` - [Issue]
- `[test-file:line]` - [Issue]

**Fix Strategy**: [Update mocks / Fix snapshots / Adjust test data]

---

### 6. Other/Uncategorized
**Count**: [N] errors

[List any errors that don't fit above categories]

---

## Recommended Approach

**Priority 1 - Must Fix** ([N] errors):
- [Category]: [Brief action plan]
- [Category]: [Brief action plan]

**Priority 2 - Should Fix** ([N] errors):
- [Category]: [Brief action plan]

**Priority 3 - Can Defer** ([N] errors):
- [Category]: [Brief action plan or reason to defer]

---

## Options for User

1. **Automated Fix Attempt**
   - I can attempt to fix categories: [list safe categories]
   - Estimated changes: [N] files
   - Risks: [Any concerns]

2. **Guided Fix**
   - I provide specific fix instructions for each error
   - You review and approve before each change
   - More time consuming but safer

3. **Manual Fix**
   - You handle all fixes manually
   - I can provide this analysis as reference

**Recommendation**: [Option X - with reasoning]
