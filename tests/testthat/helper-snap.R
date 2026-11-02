# Shared setup for the SNAP tests: load parameters, source the SNAP calculator, build test households

suppressPackageStartupMessages({
  library(dplyr)
  library(matrixStats)
})

params_path <- Sys.getenv("PRD_BENEFIT_PARAMS", here::here("prd_parameters", "benefit.parameters.rdata"))
load(params_path, envir = globalenv())
source(here::here("functions", "benefits_functions.R"), local = globalenv())

snap_year <- max(snapData$ruleYear)
snap_latest <- snapData[snapData$ruleYear == snap_year, ]
snap_prior <- snapData[snapData$ruleYear == snap_year - 1, ]
snap_states <- sort(unique(snap_latest$stateFIPS))
noncontiguous_fips <- c(2, 15) # AK, HI have their own federal SNAP amounts

# Format offending rows for test failure messages
show_rows <- function(df, cols) {
  if (nrow(df) == 0) return("")
  paste(capture.output(print(as.data.frame(df[, cols]), row.names = FALSE)), collapse = "\n")
}

# Build one row per (stateFIPS, famsize, income) with every column function.snapBenefit reads.
# Person 1 is a working-age adult, everyone else is a child, nobody is elderly or disabled.
make_household <- function(grid, rent = 0, utilities = 0, childcare = 0, assets = 0) {
  data <- as.data.frame(grid)
  n <- nrow(data)
  data$ruleYear <- snap_year
  data$income.gift <- 0
  data$income.child_support <- 0
  data$income.investment <- 0
  data$value.tanf <- 0
  data$value.ssi <- 0
  data$value.ssdi <- 0
  for (i in 1:12) {
    data[[paste0("agePerson", i)]] <- ifelse(i == 1, 30, ifelse(i <= data$famsize, 10, NA))
    data[[paste0("disability", i)]] <- 0
  }
  for (i in 1:6) {
    data[[paste0("value.ssiAdlt", i)]] <- 0
    data[[paste0("value.ssiChild", i)]] <- 0
    data[[paste0("ssdiPIA", i)]] <- 0
  }
  data$oop.add_for_elderlyordisabled <- 0
  data$netexp.childcare <- childcare
  data$netexp.rentormortgage <- rent
  data$netexp.utilities <- utilities
  data$totalassets <- assets
  data
}
