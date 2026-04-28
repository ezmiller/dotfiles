# Archive Tags Reference

This document contains the complete tag taxonomy for archiving files.

## Tag Order

Always apply tags in this order: **person → organization → category → type**

## People

| Tag | Person |
|-----|--------|
| `ethan` | Ethan |
| `leah` | Leah |
| `cy` | Cy |
| `alma` | Alma |

## Organizations

| Tag | Organization |
|-----|--------------|
| `fidelity` | Fidelity Investments |
| `primarykids` | Primary Kids |
| `cbe` | Congregation Beth Elohim |
| `crcs` | Community Roots Charter School |
| `nationalgrid` | National Grid |
| `capitalone` | Capital One |
| `aetna` | Aetna Health Insurance |
| `voya` | Voya Retirement Plans |
| `llc` | Ethan Miller Consulting Services LLC |

## Core Categories

### Financial Categories

| Tag | Use For |
|-----|---------|
| `investments` | Brokerage accounts, stock/fund trading, dividends, Fidelity taxable accounts |
| `retirement` | Retirement accounts (401k, IRA, Roth IRA, SEP-IRA), retirement statements |
| `credit` | Credit cards, credit accounts, credit card statements |
| `banking` | Bank accounts, checking, savings, bank statements |
| `charitable` | Charitable contributions, donor-advised funds, donation receipts |

### Other Core Categories

| Tag | Use For |
|-----|---------|
| `medical` | All health-related (medical, dental, vision), doctor visits, lab results, prescriptions |
| `insurance` | Insurance plans, insurance cards, coverage documents, plan summaries |
| `taxes` | Tax documents, W-2s, 1099s, tax returns |
| `education` | School, tuition, educational programs |
| `housing` | Rent, lease, apartment/home-related |
| `utilities` | Gas, electric, water, internet bills |
| `employment` | Employment-related, paystubs, job documents |

### Specialized Categories

| Tag | Use For |
|-----|---------|
| `travel` | Travel documents, tickets, itineraries |
| `transportation` | Car, DMV, parking, transit |
| `childcare` | Childcare, after-school programs |
| `legal` | Legal documents, contracts |
| `hsa` | Health Savings Account (hybrid finance/medical) |
| `psychotherapy` | Psychotherapy sessions, invoices, and payments |

## Document Types

| Tag | Use For |
|-----|---------|
| `statement` | Account statements (financial, utility, etc.) |
| `receipt` | Payment receipts, purchase confirmations |
| `invoice` | Bills, invoices |
| `trade` | Investment trade confirmations |
| `paystub` | Salary/wage paystubs |
| `contract` | Contracts, agreements, leases |
| `confirmation` | Confirmation documents |
| `w2` | W-2 tax forms |
| `return` | Complete filed tax returns |
| `important` | Critical documents (use sparingly) |

## Tags to AVOID

These tags have been consolidated - do NOT use them:

| Don't Use | Use Instead |
|-----------|-------------|
| `finance` | Specific tag: `investments`, `retirement`, `credit`, `banking`, `charitable` |
| `401k` | `retirement` |
| `apartment`, `rent` | `housing` |
| `dental`, `vision`, `benefits` | `medical` or `insurance` |
| `career` | `employment` |
| `bill` | `invoice` |
| Stock tickers (nvda, fxaix) | Put in description, not tags |
| Year tags (2024, 2025) | Already in timestamp |
| `fund`, `stock`, `equity` | `investments` |

## Examples

```
20251024T120000--fidelity-sold-mu-nvda-strl__ethan_fidelity_investments_trade.pdf
20250103T033300--ethan-cholesterol-panel-lab-results__ethan_medical.pdf
20250915T104500--cbe-yachad-tuition-payment-receipt__cy_education_cbe_receipt.pdf
20260125T120000--leah-w2-2025__leah_taxes_w2.pdf
20231220T000000--520-lincoln-place-rent-invoice-january-2024__housing_ethan_leah_invoice.pdf
```
