---
name: update-ynab
description: >
  Bring YNAB up to date — the recurring household budgeting workflow Ethan runs
  3-4 times a month. Covers picking which budget to work on, categorizing and
  approving uncategorized/unapproved transactions, and reconciling individual
  accounts against bank balances. Trigger this whenever the user says things
  like "update YNAB", "do YNAB", "categorize transactions", "approve
  transactions", "reconcile [account]", "YNAB cleanup", "let's do the budget",
  or any other request to work on YNAB. Also trigger on general YNAB questions
  ("what's unapproved in my main budget?", "how much did I spend on groceries?")
  since this skill is the entry point for the YNAB MCP tools.
user_invocable: true
metadata:
  short-description: Categorize and reconcile YNAB budget
---

# Update YNAB

You are helping Ethan bring his YNAB budget up to date. This is a recurring
household chore (3-4 times a month) with three predictable phases:

1. **Pick a budget** to work on
2. **Categorize + approve** the unapproved/uncategorized transactions
3. **Reconcile** individual accounts against bank balances

Each phase is independent — the user may want to stop after Phase 2, or jump
straight to Phase 3. Confirm before moving between phases.

All YNAB operations go through the `mcp__ynab__*` tools. Never invent endpoints
or scrape the web UI; if a tool doesn't exist for what's needed, say so.

---

## Phase 1: Pick the budget

YNAB MCP defaults to `YNAB_BUDGET_ID` when no `budgetId` is passed, but Ethan
maintains multiple budgets. Surface the choice rather than silently defaulting.

1. Call `ynab_list_budgets` to enumerate budgets.
2. For each budget, call `ynab_get_unapproved_transactions` and count
   results. (This call is cheap — it uses server knowledge.)
3. Present a short table:

   | Budget | Unapproved txns | Last modified |
   |---|---|---|
   | Household | 12 | 2026-05-15 |
   | Solo | 3  | 2026-05-10 |

4. Ask which to work on, or — if only one budget has pending work — propose it
   and continue on confirmation.

Capture the chosen `budgetId` and pass it explicitly to every subsequent call,
so the user can see in the transcript which budget is being touched.

---

## Phase 2: Categorize + approve

The goal is to leave the budget with **zero uncategorized and zero unapproved
transactions**. YNAB treats "uncategorized" and "unapproved" as separate flags
that often overlap — a freshly imported transaction is usually both.

### 2a. Pull the queue

```
ynab_get_transactions(budgetId, type='unapproved')
```

`unapproved` is a superset of `uncategorized` in practice (most uncategorized
txns are also unapproved), so a single fetch is usually enough. If after
processing there are still uncategorized-but-approved transactions, pull
`type='uncategorized'` separately.

### 2b. Build a payee → category map from history

For each unique payee in the queue, look up prior transactions to find the
category Ethan has used most often. This is what makes categorization fast:
recurring payees (Spotify, Con Edison, Whole Foods) get auto-suggested with
high confidence; one-off payees fall through to manual review.

```
ynab_get_transactions(budgetId, payeeId=<payee>, limit=20)
```

For each payee, classify as:

- **Confident**: ≥3 prior transactions, all in the same category → propose that
  category, batch with other confidents
- **Likely**: most-common category covers ≥60% of history → propose with the
  alternative noted
- **Unknown**: no useful history → ask the user

Cache the per-payee history lookups within a session — don't refetch.

### 2c. Present categorizations in a batch

Show a table the user can scan in one pass:

| # | Date | Payee | Amount | Proposed category | Confidence |
|---|---|---|---|---|---|
| 1 | 05-12 | Spotify | -$11.99 | Subscriptions | confident |
| 2 | 05-13 | Whole Foods | -$84.21 | Groceries | confident |
| 3 | 05-14 | Etsy | -$22.00 | ? | unknown |

Ask for any corrections in a single round-trip ("change #2 to Household, leave
the rest"), then apply.

### 2d. Apply changes

For each transaction, call `ynab_update_transaction` with `categoryId` and
`approved: true` in the same call. There's no separate "categorize then
approve" round trip needed.

If the user just wants to approve already-categorized transactions, use
`ynab_bulk_approve_transactions` with the list of IDs — it's one call.

### 2e. Wrap up Phase 2

Confirm count of transactions touched, and ask whether to move on to
reconciliation.

### Notes on category lookup

Call `ynab_list_categories(budgetId)` once at the start of Phase 2 and hold
the result for the duration. Categories are grouped — when matching by name,
prefer exact match within the most likely group (e.g. "Groceries" in
"Immediate Obligations", not in some old hidden group).

Hidden / deleted categories show up in the list but should never be proposed.
Filter them out.

---

## Phase 3: Reconcile accounts

YNAB reconciliation = "I've checked the bank statement and YNAB's cleared
balance for this account matches it, so lock in everything that's currently
cleared." The MCP has no dedicated reconcile endpoint, but the workflow is
straightforward.

### 3a. List accounts

```
ynab_list_accounts(budgetId)
```

Show open accounts with both balances:

| Account | YNAB cleared balance | Working balance |
|---|---|---|
| Chase Checking | $4,231.10 | $4,180.55 |
| Amex Gold      | -$842.00 | -$1,103.42 |

Ask which to reconcile. Reconcile one account at a time — never batch this.

### 3b. Compare to bank

Ask the user for the **current cleared balance from the bank** (whatever the
bank's site or statement says is the actual money in the account, or actual
balance owed for a credit card).

Compare to YNAB's cleared balance.

**If they match exactly:** proceed to 3c.

**If they differ:** do NOT mark anything reconciled. Help the user investigate:

- Pull cleared transactions in the last ~30 days for the account:
  `ynab_get_transactions(budgetId, accountId, sinceDate)`
- Common causes:
  - A bank transaction hasn't imported yet (add it manually, then re-check)
  - A YNAB transaction was entered but the bank hasn't processed it yet
    (leave it uncleared until it posts)
  - Wrong amount on a transaction (correct it)
- After fixes, re-ask for the bank balance and re-compare.

Do not "force" reconciliation by creating an adjustment unless the user
explicitly asks for one. Even then, surface the amount clearly and confirm.

### 3c. Mark cleared transactions as reconciled

Once balances match:

1. Pull all transactions for the account where `cleared == 'cleared'`. The
   transactions endpoint doesn't filter by cleared status directly, so fetch
   recent transactions and filter client-side.
2. For each, call `ynab_update_transaction` with `cleared: 'reconciled'`.

This can be many transactions. If there are more than ~20, pause to confirm
the count with the user before sending the updates.

### 3d. Move to next account

After each account, ask whether to continue with the next one or stop.

---

## General guidance

- **Surface budget and account context** in every reply. "Updating Household
  budget" beats a silent operation the user can't audit.
- **Batch user prompts.** A single table with 10 categorizations is faster
  than 10 individual confirmations.
- **Never auto-approve without categorization.** An approved-but-uncategorized
  transaction is worse than an unapproved one — it stops appearing in the
  "needs attention" queue.
- **Money values are in dollars** in this MCP (not milliunits). `-10.99` is a
  $10.99 outflow.
- **Dates are ISO** (`2026-05-16`).

## What this skill is NOT for

- Creating new transactions from receipts → that's a different workflow
- Building category targets / assigning money → use the YNAB UI; the MCP has
  no goal/target tools exposed
- Pulling spending reports → `ynab_budget_summary` works for quick "what's
  overspent this month" but for serious analysis use the YNAB website

If the user asks for one of these, do the best you can with the available
tools and flag the limitation.
