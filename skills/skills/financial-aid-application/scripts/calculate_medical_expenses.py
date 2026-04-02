#!/usr/bin/env python3
"""
Calculate Medical Expenses from YNAB Health & Wellness Transactions

Purpose: Calculate total medical expenses after manual classification of payees.
This script uses a predefined list of medical payees that have been confirmed
as legitimate medical expenses (excluding fitness/wellness items).

Usage:
    python3 calculate_medical_expenses.py <ynab_health_wellness.csv>

Input: YNAB CSV export filtered to Health & Wellness category
Output: Detailed breakdown of medical expenses by payee and category

This script was developed during the 2025-2026 financial aid application
process after running analyze_ynab_health_wellness.py and manually reviewing
unclear items with the user.
"""

import csv
import sys
from collections import defaultdict


# Medical payees confirmed through manual review
# Update this list based on your specific payees and user confirmation
MEDICAL_PAYEES = {
    # Clear medical providers
    'Amazon Pharmacy',
    'CVS',
    'CVS Pharmacy',
    'Columbia Faculty Practice',
    'Duane Reade',
    'Institute for Family Health',
    'Laboratory Corporation',
    'Mount Sinai',
    'Mount Sinai Doctors',
    'NYU Grossman School of Medicine',
    'Park Slope Dental Arts',
    'PatientPayment.com',
    'Pediatric Associates',
    'Pure Health Pharmacy',
    'Quest Diagnostics',
    'Seattle Children\'s Hospital',
    'United Health Care',
    'Vashon Pharmacy',
    # User-confirmed medical expenses (2025)
    'Zelle Transfer To Capers Kuhnert',  # Talk therapy
    'Zelle: Capers Kuhnert',  # Talk therapy
    'Frost-Arnett',  # Medical debt collector
}


def calculate_medical_expenses(csv_file):
    """Calculate total medical expenses from YNAB CSV."""

    # Read the CSV
    transactions = []
    with open(csv_file, 'r') as f:
        reader = csv.DictReader(f)
        for row in reader:
            transactions.append(row)

    # Collect medical expenses
    medical_expenses = []
    for t in transactions:
        payee = t['Payee'].strip('"')
        if payee in MEDICAL_PAYEES:
            medical_expenses.append(t)

    # Sort by date
    medical_expenses.sort(key=lambda x: x['Date'])

    # Calculate total and create detailed breakdown
    total = 0.0
    payee_totals = {}

    for t in medical_expenses:
        payee = t['Payee'].strip('"')
        outflow = t['Outflow'].replace('$', '').replace(',', '')
        if outflow:
            amount = float(outflow)
            total += amount
            if payee not in payee_totals:
                payee_totals[payee] = {'total': 0.0, 'count': 0, 'transactions': []}
            payee_totals[payee]['total'] += amount
            payee_totals[payee]['count'] += 1
            payee_totals[payee]['transactions'].append(t)

    # Print detailed report
    print("MEDICAL EXPENSES UNPAID BY INSURANCE")
    print("=" * 80)
    print("Source: YNAB Health & Wellness Category")
    print("=" * 80)
    print()

    # Sort by total amount descending
    for payee in sorted(payee_totals.keys(), key=lambda x: payee_totals[x]['total'], reverse=True):
        info = payee_totals[payee]
        print(f"{payee}: ${info['total']:,.2f} ({info['count']} transactions)")

        # Show sample memos
        memos = set()
        for t in info['transactions']:
            memo = t['Memo'].strip('"')
            if memo:
                memos.add(memo)
        if memos:
            for memo in sorted(memos)[:3]:  # Show up to 3 memos
                print(f"  - {memo}")
        print()

    print("=" * 80)
    print(f"TOTAL MEDICAL EXPENSES UNPAID BY INSURANCE: ${total:,.2f}")
    print("=" * 80)
    print()
    print("BREAKDOWN BY CATEGORY:")
    print("-" * 80)

    # Categorize expenses
    therapy = sum([
        payee_totals.get('Zelle Transfer To Capers Kuhnert', {'total': 0.0})['total'],
        payee_totals.get('Zelle: Capers Kuhnert', {'total': 0.0})['total'],
    ])

    dental = payee_totals.get('Park Slope Dental Arts', {'total': 0.0})['total']

    doctors = sum([
        payee_totals.get('Columbia Faculty Practice', {'total': 0.0})['total'],
        payee_totals.get('Mount Sinai', {'total': 0.0})['total'],
        payee_totals.get('Mount Sinai Doctors', {'total': 0.0})['total'],
        payee_totals.get('Pediatric Associates', {'total': 0.0})['total'],
        payee_totals.get('NYU Grossman School of Medicine', {'total': 0.0})['total'],
        payee_totals.get('Seattle Children\'s Hospital', {'total': 0.0})['total'],
        payee_totals.get('Institute for Family Health', {'total': 0.0})['total'],
        payee_totals.get('PatientPayment.com', {'total': 0.0})['total'],
        payee_totals.get('United Health Care', {'total': 0.0})['total'],
    ])

    pharmacy = sum([
        payee_totals.get('Amazon Pharmacy', {'total': 0.0})['total'],
        payee_totals.get('CVS', {'total': 0.0})['total'],
        payee_totals.get('CVS Pharmacy', {'total': 0.0})['total'],
        payee_totals.get('Duane Reade', {'total': 0.0})['total'],
        payee_totals.get('Pure Health Pharmacy', {'total': 0.0})['total'],
        payee_totals.get('Vashon Pharmacy', {'total': 0.0})['total'],
    ])

    labs = sum([
        payee_totals.get('Laboratory Corporation', {'total': 0.0})['total'],
        payee_totals.get('Quest Diagnostics', {'total': 0.0})['total'],
    ])

    collections = payee_totals.get('Frost-Arnett', {'total': 0.0})['total']

    print(f"Therapy/Mental Health: ${therapy:,.2f}")
    print(f"Doctors/Specialists/Copays: ${doctors:,.2f}")
    print(f"Dental: ${dental:,.2f}")
    print(f"Pharmacy/Prescriptions: ${pharmacy:,.2f}")
    print(f"Lab Work: ${labs:,.2f}")
    print(f"Medical Collections: ${collections:,.2f}")
    print("-" * 80)
    print(f"TOTAL: ${therapy + doctors + dental + pharmacy + labs + collections:,.2f}")


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python3 calculate_medical_expenses.py <ynab_health_wellness.csv>")
        print("\nFirst, extract Health & Wellness transactions from YNAB:")
        print('  grep "Health & Wellness" ynab_export.csv > health_wellness.csv')
        print('  echo "Account,Flag,Date,Payee,Category Group/Category,Category Group,Category,Memo,Outflow,Inflow,Cleared" > temp.csv')
        print('  cat health_wellness.csv >> temp.csv')
        print('  mv temp.csv health_wellness.csv')
        print("\nNote: Update MEDICAL_PAYEES in this script based on your confirmed medical providers.")
        sys.exit(1)

    calculate_medical_expenses(sys.argv[1])
