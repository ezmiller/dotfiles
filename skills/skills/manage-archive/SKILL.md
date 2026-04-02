---
name: manage-archive
description: >
  Search, analyze, rename, and maintain files in ~/Documents/archive/.
  Use when user wants to find documents, summarize groups of records,
  fix naming/tags on archived files, or audit the archive for consistency.
metadata:
  short-description: Search, analyze, and maintain document archive
---

# Manage Archive Skill

## Overview

This skill works with files already in `~/Documents/archive/`. It supports four core operations: searching/retrieving documents, analyzing groups of documents, renaming/retagging files, and auditing the archive for consistency.

All files in the archive follow the denote naming convention:
```
YYYYMMDDThhmmss--description-with-dashes__tags_with_underscores.extension
```

See [TAGS.md](references/TAGS.md) for the complete tag taxonomy.

---

## 1. Search & Retrieve

Find and read documents by date, tags, description keywords, or content.

### By Tags

```bash
ls ~/Documents/archive/ | grep '_medical'
ls ~/Documents/archive/ | grep '_ethan_fidelity'
```

### By Date Range

```bash
# All documents from January 2026
ls ~/Documents/archive/ | grep '^202601'

# Documents from Q1 2026
ls ~/Documents/archive/ | grep -E '^2026(01|02|03)'
```

### By Description Keywords

```bash
ls ~/Documents/archive/ | grep 'cholesterol'
ls ~/Documents/archive/ | grep 'rent-invoice'
```

### By Content (full-text search in PDFs)

```bash
# Search inside PDFs for specific text
find ~/Documents/archive/ -name '*.pdf' -newer /tmp/start_date -exec sh -c 'pdftotext "$1" - 2>/dev/null | grep -l "search term" && echo "$1"' _ {} \;
```

### Reading Documents

- Use the Read tool for PDFs, images, markdown, and CSV files
- For large PDFs, read specific pages: `pages: "1-3"`
- For text extraction: `pdftotext "file.pdf" - | head -30`

### Presenting Results

When returning search results, present them in a table:

| Date | Description | Tags | File |
|------|-------------|------|------|
| 2026-01-17 | NYU ER lab troponin | ethan, medical | `20260117T...pdf` |

---

## 2. Analyze & Report

Summarize groups of documents to answer questions about history, trends, or totals.

### Common Analysis Patterns

**Medical history:** Find all `_medical` documents for a person, read key ones, and summarize chronologically.

**Financial overview:** Find all `_investments_statement` or `_retirement_statement` documents for a period and summarize holdings/changes.

**Tax preparation:** Find all tax-relevant documents for a year (`_w2`, `_taxes`, `_charitable`, `_medical` receipts) and create a checklist.

**Spending by category:** Find all `_invoice` or `_receipt` documents in a date range and summarize by category.

### Process

1. Search the archive for relevant files using tag and date filters
2. Read documents (use the Read tool — for large PDFs, read page 1 first)
3. Extract key data points (dates, amounts, descriptions)
4. Present a structured summary with source references

### Output Format

Always cite specific filenames when referencing data. Use tables or chronological lists as appropriate for the question.

---

## 3. Rename & Retag

Fix or update filenames, descriptions, or tags on existing archived files.

### Process

1. **Identify the file(s)** to rename — search or let user specify
2. **Show current name** parsed into components:
   - Timestamp: `20260117T122400`
   - Description: `nyu-er-lab-troponin-i-high-sensitivity`
   - Tags: `ethan_medical`
3. **Propose new name** with changes highlighted
4. **Confirm with user** before renaming
5. **Rename:**
   ```bash
   mv ~/Documents/archive/old-name ~/Documents/archive/new-name
   ```
6. **Verify** the rename succeeded

### Bulk Rename

When renaming multiple files:
1. Present a table of all proposed changes
2. Get user confirmation for the batch
3. Process and report results

### Tag Reference

When retagging, consult [TAGS.md](references/TAGS.md) for:
- Valid tags and their meanings
- Correct tag order (person → organization → category → type)
- Tags to avoid (consolidated/deprecated tags)

---

## 4. Maintain & Audit

Find and fix inconsistencies in the archive.

### Naming Convention Check

Find files that don't match the expected pattern:
```bash
ls ~/Documents/archive/ | grep -v -E '^[0-9]{8}T[0-9]{6}--[a-z0-9-]+__[a-z0-9_]+\.[a-z]+$'
```

### Tag Consistency Audit

Extract and count all tags in use:
```bash
ls ~/Documents/archive/ | sed 's/.*__//; s/\..*//' | tr '_' '\n' | sort | uniq -c | sort -rn
```

Compare against the approved tag list in [TAGS.md](references/TAGS.md). Flag:
- Tags not in the approved list (potential typos or deprecated tags)
- Deprecated tags that should be updated (e.g., `finance` → specific category)

### Duplicate Detection

Find potential duplicates by similar descriptions or identical timestamps:
```bash
# Files with identical timestamps
ls ~/Documents/archive/ | cut -c1-15 | sort | uniq -d
```

### Missing Tag Audit

Find files that may be missing expected tags:
```bash
# Files without a person tag
ls ~/Documents/archive/ | grep -v -E '_(ethan|leah|cy|alma)[_.]'

# Files without a category tag
ls ~/Documents/archive/ | grep -v -E '_(medical|investments|retirement|credit|banking|taxes|education|housing|utilities|employment|insurance|charitable|travel|transportation|childcare|legal|hsa)[_.]'
```

### Reporting

Present audit findings as actionable items:
1. **Issues found** — list each problem with the affected file
2. **Suggested fix** — proposed rename or retag
3. **Batch fix option** — offer to fix all issues after confirmation

---

## Reference

- Tag taxonomy: [TAGS.md](references/TAGS.md)
- Full archiving instructions: `~/Documents/archive-instructions.md`
- Inbox processing (for new files): use the `process-inbox` skill instead
