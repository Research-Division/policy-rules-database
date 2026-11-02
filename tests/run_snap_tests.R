# Run the SNAP parameter and calculator tests
# Usage (from the repo root): Rscript tests/run_snap_tests.R
# To test a different parameters file: set PRD_BENEFIT_PARAMS=/path/to/benefit.parameters.rdata

library(testthat)
library(here)

test_dir(here::here("tests", "testthat"), reporter = "summary", stop_on_failure = TRUE)
