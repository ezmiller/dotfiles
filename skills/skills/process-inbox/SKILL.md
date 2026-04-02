---
name: process-inbox
description: >
  Process and archive files from ~/Documents/inbox/ (or specified directory) into
  ~/Documents/archive/ using standardized naming conventions. Reads file contents to
  determine appropriate timestamp, description, and tags. Use when user wants to file,
  archive, or organize documents.
metadata:
  short-description: Process inbox files into archive
---

# Process Inbox Skill

## Overview

This skill processes files from `~/Documents/inbox/` (or a specified source) and archives them to `~/Documents/archive/` using a standardized naming convention that works with the denote library (https://github.com/protesilaos/denote).

## Archive Filename Format

```
YYYYMMDDThhmmss--description-with-dashes__tags_with_underscores.extension
```

### Components

1. **Timestamp**: ISO 8601 format (YYYYMMDDThhmmss)
   - Extract from document metadata, content, or use current date
   - Default time: T120000 (noon) when exact time unavailable

2. **Description**: Lowercase with hyphens
   - Descriptive, human-readable name (3-8 words)
   - Use hyphens to separate words

3. **Tags**: Underscore-separated categories
   - Order: person → organization → category → type
   - Multiple tags separated by underscores

## Process

### Step 1: List Files to Process

List files in `~/Documents/inbox/` (or specified directory):
```bash
ls -la ~/Documents/inbox/
```

Present the list and ask user which files to process, or process all. 

### Step 2: For Each File

#### 2a. Read the File
- Use the Read tool for PDFs, images, and text files
- For large PDFs, extract first page:
  ```bash
  pdftotext -f 1 -l 1 "filename.pdf" - | head -30
  ```

#### 2b. Determine Timestamp
- Look for dates in document content
- Check file metadata if needed
- Use document creation/transaction date
- Default to T120000 (noon) when time unknown

#### 2c. Create Description
- Write a clear, descriptive name (3-8 words)
- Use lowercase letters with hyphens
- Focus on what the document IS

#### 2d. Select Tags

Apply tags in order: **person → organization → category → type**

See [TAGS.md](references/TAGS.md) for the complete tag taxonomy.

#### 2e. Confirm with User

Present the proposed archive name:
```
Original: Gmail - NYC DOF Payment Receipt# MOB260168202.pdf
Proposed: 20260120T101500--nyc-dof-property-tax-payment-receipt__housing_receipt.pdf
```

Ask user to confirm or modify.

#### 2f. Move File

```bash
mv "original-filename" ~/Documents/archive/YYYYMMDDThhmmss--description__tags.ext
```

### Step 3: Verify

After moving, verify the file exists in the archive:
```bash
ls -la ~/Documents/archive/ | grep "description-fragment"
```

## Examples

**Investment/Trading:**
```
20251024T120000--fidelity-sold-mu-nvda-strl__ethan_fidelity_investments_trade.pdf
```

**Medical:**
```
20250103T033300--ethan-cholesterol-panel-lab-results__ethan_medical.pdf
```

**Education:**
```
20250915T104500--cbe-yachad-tuition-payment-receipt__cy_education_cbe_receipt.pdf
```

**Tax Documents:**
```
20260125T120000--leah-w2-2025__leah_taxes_w2.pdf
```

## Batch Processing

When processing multiple files:
1. Read all files first to understand contents
2. Present a table of proposed renames
3. Get user confirmation for the batch
4. Process confirmed files
5. Report success/failure for each

## Reference

For complete archiving instructions including PDF stamping, see:
`~/Documents/archive-instructions.md`
