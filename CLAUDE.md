# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

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
