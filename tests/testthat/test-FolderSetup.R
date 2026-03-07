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

  # Main Folder was created
  expect_true(file.exists(Folder))

  Files <- list.files(Folder)

  # Subfolders were created
  expect_true("data" %in% Files)
  expect_true("docs" %in% Files)
  expect_true("images" %in% Files)
  expect_true("R" %in% Files)

  # CytometryQC extdata files were copied over
  expect_true("LICENSE.md" %in% Files)
  expect_true("styles.scss" %in% Files) 
  expect_true("Maintenance.csv" %in% Files)

  # QMD files were created
  expect_true("404.qmd" %in% Files)
  expect_true("help.qmd" %in% Files)
  expect_true("Miscellaneous.qmd" %in% Files)
  expect_true("Instrument.qmd" %in% Files)
  expect_true("Historical.qmd" %in% Files)
  expect_true("index.qmd" %in% Files)
  expect_true("Data.qmd" %in% Files)
  expect_true("README.md" %in% Files)
  expect_true("_quarto.yml" %in% Files)

  # openCyto Gating .csv
  expect_true("Gates.csv" %in% Files)

})