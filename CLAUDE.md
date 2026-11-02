# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is
<<<<<<< HEAD

The Policy Rules Database (PRD) is a batch microsimulation engine written in R (no package structure — plain scripts sourced in order). Given a household specification, geography, policy year, income range, and program switches, it builds household scenarios and computes expenses, public benefits, taxes, tax credits, after-tax income, and net resources for the major U.S. federal/state safety-net programs.

A detailed, code-verified map of the architecture, data flow, column contracts, and known inconsistencies lives in `ARCHITECTURE.md` at the repo root — read it before making non-trivial changes. It is a local companion doc (not auto-maintained), so treat it as a snapshot and verify against the code when precision matters; update it when you change call order, YAML wiring, parameter bundle schemas, or output columns.

## Running the model

There is no build system, package manager, or test runner configured (the CI workflow at `.github/workflows/test.yml` references `tests/BenefitsCalculator_Unittest.R` and `tests/run_tests.R`, but no `tests/` directory is tracked in this checkout — do not assume these tests exist or run them).

To run a simulation:
1. Open R/RStudio with working directory at the repo root (`applyBenefitsCalculator.R` builds paths off `getwd()`).
2. Edit or copy `projects/TEST.yml` to specify household/geography/policy-year/income inputs and `APPLY_*` program switches.
3. In `applyBenefitsCalculator.R`, set `PROJECT <- "YOUR_NAME"` to point at that YAML file.
4. Run the script top to bottom. It calls `rm(list=ls())` first, so it destroys any other interactive state in the session.
5. Results end up in the in-memory `data2` data frame. The `write.csv(...)` line that would save `output/results_<PROJECT>.csv` is currently commented out — uncomment it if you need the file written.

`libraries.R` installs missing packages as a side effect of sourcing it — expect first-run installs. Package load order matters: `plyr` is loaded after `dplyr` deliberately to control function masking.

## Architecture essentials

- **Composition root**: `applyBenefitsCalculator.R` loads four `.rdata` parameter bundles from `prd_parameters/`, sources the five function files in `functions/`, reads the project YAML, and calls orchestrator blocks in a fixed order. This order encodes real policy dependencies (e.g. SSDI must precede TANF because TANF treats SSDI as unearned income; child care must precede taxes because CDCTC uses `netexp.childcare`). See "Ordering invariants" in `ARCHITECTURE.md` before reordering blocks.
- **Core data contract**: everything communicates through one wide data frame (`data` / `data2`), one row per scenario (household × location × income × policy year × any other vector-valued YAML input), built via `expand.grid()` — multi-valued YAML fields are crossed, not paired. Modules add/overwrite columns on this frame rather than passing domain objects.
- **Function files** (`functions/`):
  - `BenefitsCalculator_functions.R` — scenario construction (`function.createData`, `function.InitialTransformations`) and the top-level orchestrator functions (`BenefitsCalculator.OtherBenefits`, `.Childcare`, `.Healthcare`, `.FoodandHousing`, `.TaxesandTaxCredits`), plus `function.createVars` for summary measures.
  - `benefits_functions.R` — individual program rule functions (SSDI, SSI, SNAP, WIC, Section 8/RAP/FRSP, school meals, Head Start/Pre-K, Medicaid/ACA, federal/state income tax, FICA, EITC, CTC, CDCTC).
  - `expense_functions.R` — ALICE baseline expense assignment (child care, food, housing, healthcare, transportation, tech, misc) driven by the `budget.ALICE` setting.
  - `TANF.R`, `CCDF.R` — state-by-state rule engines, split out because their branching dwarfs the other programs; still exposed as single top-level functions (`function.tanfBenefit`, `function.CCDFcopay`).
- **Parameters** (`prd_parameters/*.rdata`): `benefit.parameters.rdata` (eligibility/benefit/tax rules, one `ccdfData_*` per state), `expenses.rdata` (ALICE cost estimates), `tables.rdata` (geography/FPL/SMI crosswalks), `parameters.defaults.rdata` (shared constants). Whole bundles are loaded; functions refer to named objects in the global environment, so object names/schemas are an implicit API. Note there's a second, non-identical `benefit.parameters.rdata` at the repo root that is *not* used by the script — don't confuse the two.
- **Common join keys** across parameter lookups: time (`ruleYear`/`Year`), geography (`stateFIPS`, `stateName`, `stateAbbrev`, `stcountyfips2010`, `countyortownName`, `AKorHI`), household (`famsize`, `numadults`, `numkids`, `numkidsinCare`), and tax status (`FilingStatus`, qualifying-child count). Changing any of these ripples repository-wide.
- **Household schema**: up to 12 members via repeated numbered columns (`agePerson1`...`agePerson12`, etc.), not a person table. Ages 19+ count as adults, 18 and under as children. Earned income is split equally across adults for individual disability calcs.

## Known sharp edges (verified in code, not fixed — be deliberate if touching)

- `BenefitsCalculator.OtherBenefits` is declared `(data, TANF, SSDI, SSI)` but called as `(data, APPLY_TANF, APPLY_SSI, APPLY_SSDI)` — SSI/SSDI args are swapped at the call site.
- `APPLY_TAXES` is read from YAML but never passed into `BenefitsCalculator.TaxesandTaxCredits` (which defaults to `TRUE`).
- Several child-care YAML switches (continuous eligibility, FT/PT) are parsed but not passed into `BenefitsCalculator.Childcare`, so function defaults apply instead; baseline child-care expenses hard-code summer care to `"PT"`.
- `Year` in YAML is unused by scenario construction — `function.createData()` always sets `Year = ruleYear`.
- Rule functions are inconsistent about using `0` vs `NA` for "ineligible"/"not applicable" — check the specific function rather than assuming.

Full details, plus a change-impact table (what to inspect/validate when touching YAML fields, benefit rules, geography keys, household schema, block order, taxes, or a new program) are in `ARCHITECTURE.md`.
=======
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
>>>>>>> 44ea78fa84115f2cc8ac1879cddad692b8edd404
