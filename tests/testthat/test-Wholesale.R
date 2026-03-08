test_that("Additional Instrument Pages Get Created", {
  tmp <- withr::local_tempdir(pattern = "CytometryQC")
  withr::local_dir(tmp)

  FolderSetup(
  SetUpGit = FALSE,
  organization_name = "UMGCCC FCSR",
  organization_website = "https://www.medschool.umaryland.edu/cibr/core/umgccc_flow/",
  githubusername = "umgcccfcsr",
  institution_name = "University of Maryland, Baltimore",
  AlternateDirectory = tmp
  )

  Folder <- list.files(tmp, pattern="InstrumentQC", full.names=TRUE)

  AddInstruments(
  name="AuroraCS",
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

  AddInstruments(
  name="Aurora5",
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

  AddInstruments(
  name="Aurora4",
  manufacturer = "Cytek",
  uv = 16,
  violet = 16,
  blue = 14,
  yellowgreen = 0,
  red = 8,
  TheFCSFolderPath = NULL,
  CytekbioExportFolderPath = NULL,
  AlternateDirectory = tmp
  )

  AddInstruments(
  name="Aurora3",
  manufacturer = "Cytek",
  uv = 0,
  violet = 16,
  blue = 14,
  yellowgreen = 0,
  red = 8,
  TheFCSFolderPath = NULL,
  CytekbioExportFolderPath = NULL,
  AlternateDirectory = tmp
  )

  DataFolder <- file.path(Folder, "data")
  Directories <- list.dirs(DataFolder, recursive=TRUE)
  Directories <- Directories[stringr::str_detect(Directories, "Archive")]
  ExistingData <- list.files("inst/extdata/Extant", full.names=TRUE)

  ids <- sub(".*BeadData(.*)\\.csv", "\\1", ExistingData)
  ids <- sub("L$", "", ids)

  dir_map <- setNames(Directories, sub(".*Aurora(.*)/Archive", "\\1", Directories))

  for(i in seq_along(ExistingData)) {
    dest <- file.path(dir_map[ids[i]], basename(ExistingData[i]))
    ok <- file.copy(ExistingData[i], dest, overwrite = TRUE)
  }

  #withr::with_dir(Folder, {system("quarto render")})
  HistoricalPages(githubusername="umgcccfcsr", Archive="Bead")

})


