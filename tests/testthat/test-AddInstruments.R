test_that("Additional Instrument Pages Get Created", {
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

  AddInstruments(
  name="The5Laser",
  manufacturer = "Cytek",
  uv = 16,
  violet = 16,
  blue = 14,
  yellowgreen = 10,
  red = 8,
  TheFCSFolderPath = NULL,
  CytekbioExportFolderPath = NULL,
  AlternateDirectory = tmp
  )

  AdditionalFiles <- list.files(Folder, pattern="The5Laser")
  expect_true("Historical_The5Laser.qmd" %in% AdditionalFiles)
  expect_true("InitialData_The5Laser.R" %in% AdditionalFiles)
  expect_true("The5Laser.qmd" %in% AdditionalFiles)
  expect_true("TheScript_The5Laser.R" %in% AdditionalFiles)
  
})