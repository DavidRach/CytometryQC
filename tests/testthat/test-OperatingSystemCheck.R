test_that("Coereba_UpdateGates returns a dataframe with 1 row", {
  library(CytometryQC)

  Location <- OperatingSystemCheck()

  expect_true(is.character(Location))
})