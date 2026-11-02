# Behavior checks on function.snapBenefit using the latest SNAP parameters

annual_max <- function(df) {
  left_join(df[, c("stateFIPS", "famsize")], snap_latest[, c("stateFIPS", "famsize", "MaxBenefit", "MinBenefit")],
            by = c("stateFIPS", "famsize"))
}

test_that("calculator runs for every state and family size", {
  grid <- expand.grid(stateFIPS = snap_states, famsize = 1:8, income = seq(0, 100000, by = 5000))
  hh <- make_household(grid, rent = 12000, utilities = 2400)
  value <- function.snapBenefit(hh)

  expect_length(value, nrow(hh))
  expect_true(is.numeric(value))
  expect_false(any(is.na(value)), info = show_rows(hh[is.na(value), ], c("stateFIPS", "famsize", "income")))
  expect_true(all(value >= 0))
})

test_that("a household with no income or shelter costs gets the maximum allotment", {
  grid <- expand.grid(stateFIPS = snap_states, famsize = 1:8, income = 0)
  hh <- make_household(grid)
  hh$snap <- function.snapBenefit(hh)
  hh$MaxBenefit <- annual_max(hh)$MaxBenefit

  bad <- hh[hh$snap != 12 * hh$MaxBenefit, ]
  expect_equal(nrow(bad), 0, info = show_rows(bad, c("stateFIPS", "famsize", "snap", "MaxBenefit")))
})

test_that("high-income households get no benefit", {
  grid <- expand.grid(stateFIPS = snap_states, famsize = 1:8, income = 250000)
  value <- function.snapBenefit(make_household(grid, rent = 12000, utilities = 2400))
  expect_true(all(value == 0))
})

test_that("benefit never increases as earnings rise", {
  grid <- expand.grid(stateFIPS = snap_states, famsize = 1:4, income = seq(0, 80000, by = 1000))
  hh <- make_household(grid, rent = 12000, utilities = 2400)
  hh$snap <- function.snapBenefit(hh)

  bad <- hh %>%
    arrange(stateFIPS, famsize, income) %>%
    group_by(stateFIPS, famsize) %>%
    mutate(change = snap - lag(snap)) %>%
    ungroup() %>%
    filter(change > 0)
  expect_equal(nrow(bad), 0, info = show_rows(bad, c("stateFIPS", "famsize", "income", "snap", "change")))
})

test_that("eligible one- and two-person households get at least the minimum benefit", {
  grid <- expand.grid(stateFIPS = snap_states, famsize = 1:2, income = seq(0, 40000, by = 1000))
  hh <- make_household(grid)
  hh$snap <- function.snapBenefit(hh)
  hh$MinBenefit <- annual_max(hh)$MinBenefit

  bad <- hh[hh$snap > 0 & hh$snap < 12 * hh$MinBenefit, ]
  expect_equal(nrow(bad), 0, info = show_rows(bad, c("stateFIPS", "famsize", "income", "snap", "MinBenefit")))
})

test_that("calculator matches a hand-computed Georgia family of three", {
  p <- snap_latest[snap_latest$stateFIPS == 13 & snap_latest$famsize == 3, ]
  earnings <- 18000
  rent <- 9600
  utilities <- 2400

  utility_deduction <- if (p$HCSUA == "Mandatory") 12 * p$HCSUAValue else max(12 * p$HCSUAValue, utilities)
  adjusted <- max(earnings - 0.2 * earnings - 12 * p$StandardDeduction, 0)
  excess_shelter <- min(max(rent + utility_deduction - 0.5 * adjusted, 0), 12 * p$MaxShelterDeduction)
  net <- max(adjusted - excess_shelter, 0)
  expect_lte(earnings, p$GrossIncomeEligibility)
  expect_lte(net, p$NetIncomeEligibility_nonelddis)
  expected <- round(min(max(12 * p$MaxBenefit - 0.3 * net, 12 * p$MinBenefit), 12 * p$MaxBenefit))

  hh <- make_household(data.frame(stateFIPS = 13, famsize = 3, income = earnings), rent = rent, utilities = utilities)
  expect_equal(function.snapBenefit(hh), expected)
})

test_that("federal amounts match the published FY2027 COLA figures", {
  # 48 contiguous states + DC, monthly amounts. Source: USDA FNS SNAP FY2027 Cost-of-Living Adjustments memo
  published <- data.frame(
    famsize = 1:4,
    MaxBenefit = c(306, 562, 808, 1023),
    StandardDeduction = c(217, 217, 217, 229),
    MaxShelterDeduction = 769,
    MinBenefit = c(25, 25, 0, 0)
  )
  actual <- snap_latest %>%
    filter(!stateFIPS %in% noncontiguous_fips, famsize <= 4) %>%
    distinct(famsize, MaxBenefit, StandardDeduction, MaxShelterDeduction, MinBenefit) %>%
    arrange(famsize)
  expect_equal(as.data.frame(actual), published)
})
