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

  setwd(Folder)

  RenderWebsite(AlternateRender = "Yes")

  pages <- c(
    "Data.html",
    "help.html",
    "historical.html",
    "Instrumental.html",
    "Miscellaneous.html"
  )

  for (p in pages) {

    html <- file.path("docs", p)
    expect_true(file.exists(html))

    png <- tempfile(fileext = ".png")

    webshot2::webshot(
      url = paste0("file:///", normalizePath(html)),
      file = png,
      vwidth = 1400,
      vheight = 1000,
      delay = 0.5
    )

    testthat::expect_snapshot_file(png)
  }

})