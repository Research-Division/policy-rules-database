# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is
The Policy Rules Database (PRD) from the Federal Reserve Bank of Atlanta is written in R. It encodes eligibility and benefit rules for federal and state public assistance programs, taxes, and tax credits. Program details and algorithms are in `PRD Technical Manual - 07-12-2024.pdf`. Planned parameter refresh dates (FPL, SNAP COLA/SUA, SMI, taxes, etc.) are in `PRD_Update_Schedule.md`.

## Running
There is no build or lint step. The working directory must be the repo root, because paths are built from `getwd()`.

    Rscript applyBenefitsCalculator.R        # local R is at C:\Program Files\R\R-4.2.2\bin\x64\Rscript.exe

- Inputs come from `projects/<PROJECT>.yml`. `PROJECT` is set in `applyBenefitsCalculator.R` and defaults to `"TEST"`. For your own scenario, copy `TEST.yml` and change `PROJECT`.
- The final `write.csv(... output/results_<PROJECT>.csv)` line is commented out. Uncomment it, or inspect `data2` interactively.
- `libraries.R` installs any missing packages and loads many of them, including shiny and plotly. `pivottabler`, `ggiraph`, and `here` are loaded but not auto-installed. It deliberately loads `plyr` **before** `dplyr`, so dplyr verbs win. Keep that order.

Tests (testthat, run locally from the repo root):

    Rscript tests/run_snap_tests.R                                    # all tests in tests/testthat
    Rscript -e "testthat::test_file('tests/testthat/test-snap-parameters.R')"   # single file

- `tests/testthat/test-snap-parameters.R` sanity-checks `snapData` for the latest ruleYear: structure, monotonic in famsize, state-level fields constant across famsize, federal amounts uniform across the 48 states + DC, year-over-year bounds. Decreases from the prior year are reported as warnings, not failures.
- `tests/testthat/test-snap-benefit.R` runs `function.snapBenefit` on synthetic households built by `make_household()` in `helper-snap.R`. It also pins the published federal COLA figures, so update that table with each October SNAP update.
- Set the `PRD_BENEFIT_PARAMS` env var to test a different `benefit.parameters.rdata`, e.g. a previous version extracted with `git show <rev>:prd_parameters/benefit.parameters.rdata > old.rdata`.
- `.github/workflows/test.yml` is stale. It still expects the deleted `tests/BenefitsCalculator_Unittest.R` and `tests/run_tests.R`, and it does not run these tests.

## Architecture
**Parameters are data, not code.** Rules live in binary `.rdata` files in `prd_parameters/`. They are loaded into the global env as data.frames:
- `benefit.parameters.rdata` has one object per program: `snapData`, `tanfData`, `ssiData`, `ssdiData`, `wicData`, `section8Data`, `medicaidData`, `acaData`, `headstartData`, `preKData`, `schoolmealData`, `fed/state{inctax,eitc,ctc,cdctc}Data`, `ficataxData`, and per-state `ccdfData_XX`.
- `expenses.rdata` holds ALICE / cost-of-living expense tables (`exp.*Data*`).
- `tables.rdata` holds crosswalks: `table.countypop`, `table.msamap`, `table.fpl`, `table.smi`, etc.
- `parameters.defaults.rdata` holds `parameters.defaults`.

The source spreadsheets and scripts that generate these files are outside this repo (see `dir.benefitsexpenses` inside them). Parameter-only updates, such as the SNAP COLA, TN SNAP, or IA tax fixes, are committed as a replaced `.rdata` with no code diff. To inspect or edit one, `load()` it, change the data.frame, then `save()` **every** object that was in the file. The root-level `benefit.parameters.rdata` is a stale copy (Feb 2026), and the calculator only reads `prd_parameters/`.

**Rule-year pattern.** Program tables are keyed by `ruleYear` + `stateFIPS` (+ `famsize`, etc.). Each `function.<program>Benefit(data)` starts by copying the latest available `ruleYear` forward to any future years requested. It then `left_join`s the table onto `data` and computes the benefit with vectorized base-R / matrixStats (`rowMaxs`, `rowMins`). State-specific exceptions are hard-coded by `stateFIPS` inside the function. One example is the NY/NH SNAP gross-income rules in `function.snapBenefit`. New rules are typically added as rows for a new `ruleYear` in the `.rdata`. Change code only when the rule's *structure* changes.

**Pipeline** (`applyBenefitsCalculator.R`):
1. `function.createData(inputs)` (BenefitsCalculator_functions.R) uses `expand.grid` to build one row per income step × location × household combination. It then calls `function.InitialTransformations`, which joins county/MSA data and derives `famsize`, `numkids`, `income1..6`, and `totalassets`.
2. `BenefitsCalculator.ALICEExpenses` attaches default expenses from expense_functions.R.
3. Program blocks run **in this order, because later ones consume earlier outputs**: `OtherBenefits` (SSDI → TANF → SSI) → `Childcare` (Head Start, PreK, CCDF) → `Healthcare` (Medicaid, ACA) → `FoodandHousing` (Section 8/RAP/FRSP, SNAP, school meals, WIC) → `TaxesandTaxCredits`. Example: SNAP counts `value.tanf`, `value.ssi`, and `value.ssdi` as income and uses `netexp.childcare` and `netexp.rentormortgage` set by earlier blocks.
4. `function.createVars` computes totals such as `NetResources` and `AfterTaxIncome`.

Each block takes `APPLY_*` switches. When a switch is FALSE, the block sets that program's `value.*` (and related `netexp.*`) to 0 rather than skipping it.

**Where code lives:**
- `functions/benefits_functions.R` has the per-program calculators (`function.snapBenefit`, `function.fedctc`, `function.stateinctax`, …).
- `functions/TANF.R` and `functions/CCDF.R` hold very large state-by-state implementations.
- `functions/BenefitsCalculator_functions.R` holds the block orchestration plus take-up and "net expense" logic.

**Output conventions:** `value.<program>` is the annual benefit, `exp.*` is the gross expense, and `netexp.*` is the expense after benefits. Amounts are annual. Parameter tables often store monthly values, which is why the code multiplies by 12.

## Gotchas
- `BenefitsCalculator.OtherBenefits(data, APPLY_TANF, APPLY_SSDI, APPLY_SSI)` takes its switches in a different order from the call in `applyBenefitsCalculator.R`, which passes `(…, APPLY_TANF, APPLY_SSI, APPLY_SSDI)` positionally. Use named arguments when calling it.
- `applyBenefitsCalculator.R` begins with `rm(list=ls())`.
- Downstream CLIFF tools and the PRD Dashboard reuse these functions (hence `function.createVars.CLIFF` and `CareerMAP` args). Keep function signatures and column names stable.
