# Financial Aid Application - Analysis Scripts

This directory contains Python scripts developed during the 2025-2026 financial aid application process for extracting and categorizing medical expenses from YNAB budget data.

## Scripts

### 1. `analyze_ynab_health_wellness.py`

**Purpose:** Initial analysis to categorize Health & Wellness transactions into medical vs. fitness/wellness expenses.

**Usage:**
```bash
# Extract Health & Wellness transactions from YNAB export
grep "Health & Wellness" ynab_full_export.csv > /tmp/health_wellness.csv

# Add header row
echo "Account,Flag,Date,Payee,Category Group/Category,Category Group,Category,Memo,Outflow,Inflow,Cleared" > /tmp/health_wellness_with_header.csv
cat /tmp/health_wellness.csv >> /tmp/health_wellness_with_header.csv

# Run analysis
python3 analyze_ynab_health_wellness.py /tmp/health_wellness_with_header.csv
```

**Output:**
- Clear medical expenses (hospitals, doctors, pharmacies)
- Fitness/wellness expenses to exclude (gyms, apps, equipment)
- Unclear items requiring manual review

### 2. `calculate_medical_expenses.py`

**Purpose:** Calculate final medical expense totals after manual classification of unclear items.

**Usage:**
```bash
# First, update MEDICAL_PAYEES in the script based on user confirmation
# Then run the calculation
python3 calculate_medical_expenses.py /tmp/health_wellness_with_header.csv > /tmp/medical_expenses_report.txt
```

**Output:**
- Total medical expenses unpaid by insurance
- Breakdown by category (therapy, doctors, dental, pharmacy, labs, collections)
- Detailed list by payee with transaction counts and sample memos

## Workflow

1. **Export YNAB data** for the relevant tax year
2. **Run `analyze_ynab_health_wellness.py`** to get initial categorization
3. **Review unclear items** with user to classify as medical or non-medical
4. **Update `calculate_medical_expenses.py`** with confirmed medical payees
5. **Run `calculate_medical_expenses.py`** to generate final totals
6. **Save results** to `/tmp/medical_expenses_unpaid_YYYY.txt`

## Notes

- These scripts assume YNAB CSV format with specific column names
- The `MEDICAL_PAYEES` set in `calculate_medical_expenses.py` is year-specific and should be updated based on manual review
- Fitness/wellness keywords may need updating for different users
- High-deductible health plans result in most medical expenses being out-of-pocket

## Development Context

These scripts were created during Session 2 (2026-01-10) of the financial aid application process, documented in `../references/DEVELOPMENT_TRACKER.md`.

Key learnings:
- YNAB "Health & Wellness" category mixes medical and fitness expenses
- Memos are critical for identifying true medical expenses
- Some payees (e.g., Amazon, ATM withdrawals) require manual review
- Therapy/mental health often represents significant portion of expenses
