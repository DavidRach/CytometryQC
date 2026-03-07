test_that("FolderSetup Creates Required Infrastructure", {
  tmp <- withr::local_tempdir(pattern = "CytometryQC")
  withr::local_dir(tmp)

  FolderSetup(
  SetUpGit = FALSE,
  organization_name = "UMGCC FCSS",
  organization_website = "https://www.medschool.umaryland.edu/cibr/core/umgccc_flow/",
  githubusername = "umgccfcss",
  institution_name = "University of Maryland, Baltimore",
  AlternateDirectory = tmp
  )

  Folder <- list.files(tmp, pattern="InstrumentQC", full.names=TRUE)
  Files <- list.files(Folder)

  expect_true("_quarto.yml" %in% Files)
  expect_true("404.qmd" %in% Files)
  expect_true("data" %in% Files)
  expect_true("Data.qmd" %in% Files)
  expect_true("docs" %in% Files)
  expect_true("Gates.csv" %in% Files)
  expect_true("help.qmd" %in% Files)
  expect_true("Historical.qmd" %in% Files)
  expect_true("images" %in% Files)
  expect_true("index.qmd" %in% Files)
  expect_true("Instrument.qmd" %in% Files)
  expect_true("LICENSE.md" %in% Files)
  expect_true("Maintenance.csv" %in% Files)
  expect_true("Miscellaneous.qmd" %in% Files)
  expect_true("R" %in% Files)
  expect_true("README.md" %in% Files)
  expect_true("styles.scss" %in% Files) 
})