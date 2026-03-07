test_that("Operating System Check is functional", {
  library(CytometryQC)

  Location <- OperatingSystemCheck()

  expect_true(is.character(Location))
})