#!/usr/bin/env python3
"""
Analyze YNAB Health & Wellness Category Transactions

Purpose: Parse YNAB CSV export to categorize health & wellness transactions
into medical expenses vs fitness/wellness expenses.

Usage:
    python3 analyze_ynab_health_wellness.py <ynab_csv_file>

Input: YNAB CSV export filtered to Health & Wellness category
Output: Categorized report showing:
    - Clear medical expenses (hospitals, doctors, pharmacies, etc.)
    - Fitness/wellness expenses (gyms, apps, equipment)
    - Unclear items requiring manual review

This script was developed during the 2025-2026 financial aid application
process to identify out-of-pocket medical expenses from YNAB budget data.
"""

import csv
import sys
from collections import defaultdict


def is_medical(payee):
    """Determine if a payee is a medical expense."""
    medical_keywords = [
        'hospital', 'clinic', 'doctor', 'medical', 'health care', 'pharmacy',
        'pediatric', 'mount sinai', 'columbia faculty', 'quest', 'laboratory',
        'patient', 'dental', 'cvs', 'duane reade', 'pure health', 'vashon pharmacy',
        'institute for family', 'nyu grossman', 'seattle children'
    ]
    payee_lower = payee.lower()
    return any(kw in payee_lower for kw in medical_keywords)


def is_fitness(payee):
    """Determine if a payee is a fitness/wellness expense (not medical)."""
    fitness_keywords = [
        'zwift', 'orangetheory', 'urban asanas', 'fitify', 'garmin', 'strava',
        'rise science', 'gym', 'fitness'
    ]
    payee_lower = payee.lower()
    return any(kw in payee_lower for kw in fitness_keywords)


def calculate_total(transactions):
    """Calculate total outflow from a list of transactions."""
    total = 0.0
    for t in transactions:
        outflow = t['Outflow'].replace('$', '').replace(',', '')
        if outflow:
            total += float(outflow)
    return total


def analyze_health_wellness(csv_file):
    """Analyze Health & Wellness transactions from YNAB CSV."""

    # Read the CSV
    transactions = []
    with open(csv_file, 'r') as f:
        reader = csv.DictReader(f)
        for row in reader:
            transactions.append(row)

    # Group by payee
    payee_groups = defaultdict(list)
    for t in transactions:
        payee = t['Payee'].strip('"')
        payee_groups[payee].append(t)

    # Sort payees by category
    medical_payees = []
    fitness_payees = []
    unclear_payees = []

    for payee in sorted(payee_groups.keys()):
        if is_medical(payee):
            medical_payees.append(payee)
        elif is_fitness(payee):
            fitness_payees.append(payee)
        else:
            unclear_payees.append(payee)

    # Print report
    print("=" * 80)
    print("CLEAR MEDICAL EXPENSES")
    print("=" * 80)
    medical_total = 0.0
    for payee in medical_payees:
        total = calculate_total(payee_groups[payee])
        medical_total += total
        count = len(payee_groups[payee])
        print(f"{payee}: ${total:,.2f} ({count} transactions)")
        # Show a sample transaction with memo if available
        sample = payee_groups[payee][0]
        memo = sample['Memo'].strip('"')
        if memo:
            print(f"  Sample memo: {memo}")

    print(f"\nTOTAL CLEAR MEDICAL: ${medical_total:,.2f}")

    print("\n" + "=" * 80)
    print("FITNESS/WELLNESS (NOT Medical Expenses)")
    print("=" * 80)
    fitness_total = 0.0
    for payee in fitness_payees:
        total = calculate_total(payee_groups[payee])
        fitness_total += total
        count = len(payee_groups[payee])
        print(f"{payee}: ${total:,.2f} ({count} transactions)")

    print(f"\nTOTAL FITNESS/WELLNESS: ${fitness_total:,.2f}")

    print("\n" + "=" * 80)
    print("UNCLEAR - NEED TO REVIEW")
    print("=" * 80)
    unclear_total = 0.0
    for payee in unclear_payees:
        total = calculate_total(payee_groups[payee])
        unclear_total += total
        count = len(payee_groups[payee])
        print(f"{payee}: ${total:,.2f} ({count} transactions)")
        # Show memos for unclear items
        memos = set()
        for t in payee_groups[payee]:
            memo = t['Memo'].strip('"')
            if memo:
                memos.add(memo)
        if memos:
            print(f"  Memos: {', '.join(sorted(memos))}")

    print(f"\nTOTAL UNCLEAR: ${unclear_total:,.2f}")

    print("\n" + "=" * 80)
    print("SUMMARY")
    print("=" * 80)
    print(f"Clear Medical Expenses: ${medical_total:,.2f}")
    print(f"Fitness/Wellness (exclude): ${fitness_total:,.2f}")
    print(f"Unclear (need review): ${unclear_total:,.2f}")
    print(f"Total Health & Wellness category: ${medical_total + fitness_total + unclear_total:,.2f}")


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python3 analyze_ynab_health_wellness.py <ynab_health_wellness.csv>")
        print("\nFirst, extract Health & Wellness transactions from YNAB:")
        print('  grep "Health & Wellness" ynab_export.csv > health_wellness.csv')
        print('  echo "Account,Flag,Date,Payee,Category Group/Category,Category Group,Category,Memo,Outflow,Inflow,Cleared" > temp.csv')
        print('  cat health_wellness.csv >> temp.csv')
        print('  mv temp.csv health_wellness.csv')
        sys.exit(1)

    analyze_health_wellness(sys.argv[1])
