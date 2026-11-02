# Sanity checks on snapData for the latest ruleYear in prd_parameters/benefit.parameters.rdata

calc_cols <- c("stateFIPS", "famsize", "ruleYear", "FPL", "GrossIncomeEligibility", "GrossIncomeEligibilityFPL",
               "NetIncomeEligibilityFPL", "NetIncomeEligibility_nonelddis", "NetIncomeEligibility_Elder_Dis",
               "AssetTestFed_nonelddis", "AssetTestFed_Elder_Dis", "AssetTest_nonelddis",
               "AssetTest_Elder_Dis_over200FPL", "AssetTest_Elder_Dis_under200FPL",
               "MedicalExpenseDeductionFloor", "StandardDeduction", "MaxShelterDeduction",
               "MaxBenefit", "MinBenefit", "HCSUAValue", "BasicLimitedUtilityAllowance")
flag_cols <- c("HCSUA", "HeatandEatState", "BBCE_State")

test_that("snapData has the expected structure", {
  expect_true(all(c(calc_cols, flag_cols) %in% names(snapData)),
              info = paste("missing:", paste(setdiff(c(calc_cols, flag_cols), names(snapData)), collapse = ", ")))
  for (v in calc_cols) expect_true(is.numeric(snap_latest[[v]]), info = v)

  expect_equal(length(snap_states), 51)
  expect_equal(sort(unique(snap_latest$famsize)), 1:12)
  expect_equal(nrow(snap_latest), 51 * 12)
  dups <- snap_latest[duplicated(snap_latest[, c("stateFIPS", "famsize")]), ]
  expect_equal(nrow(dups), 0, info = show_rows(dups, c("stateFIPS", "famsize")))

  na_cols <- names(which(colSums(is.na(snap_latest[, c(calc_cols, flag_cols)])) > 0))
  expect_equal(na_cols, character(0))
})

test_that("snapData values are in valid domains", {
  expect_true(all(snap_latest$HCSUA %in% c("Mandatory", "Optional")))
  expect_true(all(snap_latest$HeatandEatState %in% c("Yes", "No")))
  expect_true(all(snap_latest$BBCE_State %in% c("Yes", "No")))
  for (v in setdiff(calc_cols, c("stateFIPS", "famsize", "ruleYear"))) {
    expect_true(all(snap_latest[[v]] >= 0), info = v)
  }
  expect_true(all(snap_latest$MaxBenefit > 0))
  expect_true(all(snap_latest$FPL > 0))
  expect_true(all(snap_latest$MinBenefit[snap_latest$famsize <= 2] > 0))
})

test_that("amounts increase with family size within each state", {
  steps <- snap_latest %>%
    arrange(stateFIPS, famsize) %>%
    group_by(stateFIPS) %>%
    mutate(across(c(MaxBenefit, FPL, GrossIncomeEligibility, StandardDeduction), ~ .x - lag(.x), .names = "d_{.col}")) %>%
    ungroup() %>%
    filter(famsize > 1)

  for (v in c("MaxBenefit", "FPL", "GrossIncomeEligibility")) {
    bad <- steps[steps[[paste0("d_", v)]] <= 0, ]
    expect_equal(nrow(bad), 0, label = paste(v, "non-increasing rows"),
                 info = show_rows(bad, c("stateFIPS", "famsize", v)))
  }
  bad <- steps[steps$d_StandardDeduction < 0, ]
  expect_equal(nrow(bad), 0, label = "StandardDeduction decreasing rows",
               info = show_rows(bad, c("stateFIPS", "famsize", "StandardDeduction")))
})

test_that("state-level rules are the same for every family size", {
  # HCSUAValue and BasicLimitedUtilityAllowance can legitimately vary by household size (e.g. NC, TN, AZ, VA)
  state_level <- c("GrossIncomeEligibilityFPL", "NetIncomeEligibilityFPL", "MaxShelterDeduction",
                   "AssetTestFed_nonelddis", "AssetTestFed_Elder_Dis", "AssetTest_nonelddis",
                   "AssetTest_Elder_Dis_over200FPL", "AssetTest_Elder_Dis_under200FPL",
                   "MedicalExpenseDeductionFloor", flag_cols)
  for (v in state_level) {
    # Rows that differ from the state's most common value
    bad <- snap_latest %>%
      group_by(stateFIPS) %>%
      mutate(mode = names(which.max(table(.data[[v]])))) %>%
      ungroup() %>%
      filter(as.character(.data[[v]]) != mode)
    expect_equal(nrow(bad), 0, label = paste(v, "outlier rows"),
                 info = show_rows(bad, c("stateFIPS", "stateName", "famsize", v, "mode")))
  }
})

test_that("federal amounts are identical across the 48 contiguous states and DC", {
  federal <- c("MaxBenefit", "StandardDeduction", "MaxShelterDeduction", "MinBenefit", "FPL",
               "AssetTestFed_nonelddis", "AssetTestFed_Elder_Dis")
  contiguous <- snap_latest[!snap_latest$stateFIPS %in% noncontiguous_fips, ]
  for (v in federal) {
    n_values <- tapply(contiguous[[v]], contiguous$famsize, function(x) length(unique(x)))
    expect_true(all(n_values == 1), label = paste(v, "uniform by famsize"),
                info = paste("famsizes with >1 value:", paste(names(n_values)[n_values > 1], collapse = ", ")))
  }

  base <- contiguous %>% distinct(famsize, base_max = MaxBenefit)
  ak_hi <- snap_latest %>% filter(stateFIPS %in% noncontiguous_fips) %>% left_join(base, by = "famsize")
  bad <- ak_hi[ak_hi$MaxBenefit < ak_hi$base_max, ]
  expect_equal(nrow(bad), 0, info = show_rows(bad, c("stateFIPS", "famsize", "MaxBenefit", "base_max")))
})

test_that("derived thresholds are consistent with FPL", {
  bad <- snap_latest[abs(snap_latest$GrossIncomeEligibility - snap_latest$FPL * snap_latest$GrossIncomeEligibilityFPL) > 1, ]
  expect_equal(nrow(bad), 0, info = show_rows(bad, c("stateFIPS", "famsize", "FPL", "GrossIncomeEligibilityFPL", "GrossIncomeEligibility")))

  waived <- 999999
  for (v in c("NetIncomeEligibility_nonelddis", "NetIncomeEligibility_Elder_Dis")) {
    ok <- snap_latest[[v]] >= waived | abs(snap_latest[[v]] - snap_latest$FPL * snap_latest$NetIncomeEligibilityFPL) <= 1
    expect_true(all(ok), label = v, info = show_rows(snap_latest[!ok, ], c("stateFIPS", "famsize", "FPL", "NetIncomeEligibilityFPL", v)))
  }

  bad <- snap_latest[snap_latest$HCSUAValue < snap_latest$BasicLimitedUtilityAllowance, ]
  expect_equal(nrow(bad), 0, info = show_rows(bad, c("stateFIPS", "famsize", "HCSUAValue", "BasicLimitedUtilityAllowance")))
})

test_that("year-over-year changes are within normal COLA bounds", {
  skip_if(nrow(snap_prior) == 0, "no prior ruleYear to compare against")
  lower <- 0.90
  upper <- 1.15
  prior <- snap_prior %>% distinct(stateFIPS, famsize, .keep_all = TRUE)
  both <- inner_join(snap_latest, prior, by = c("stateFIPS", "famsize"), suffix = c("", ".prior"))

  for (v in c("MaxBenefit", "StandardDeduction", "MaxShelterDeduction", "FPL", "HCSUAValue")) {
    pv <- paste0(v, ".prior")
    cmp <- both[both[[v]] > 0 & both[[pv]] > 0, ]
    cmp$ratio <- round(cmp[[v]] / cmp[[pv]], 3)

    bad <- cmp[cmp$ratio < lower | cmp$ratio > upper, ]
    expect_equal(nrow(bad), 0, label = paste(v, "rows outside", lower, "-", upper),
                 info = show_rows(bad, c("stateFIPS", "stateName", "famsize", pv, v, "ratio")))

    decreases <- cmp[cmp$ratio < 1, ]
    if (nrow(decreases) > 0) {
      warning(v, " decreased from ruleYear ", snap_year - 1, " in ", nrow(decreases), " rows (states: ",
              paste(unique(decreases$stateName), collapse = ", "), ") - confirm this is expected", call. = FALSE)
    }
  }
})
