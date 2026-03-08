test_that("Render Website results in consistent webpage", {

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

  Folder <- list.files(tmp, pattern = "InstrumentQC", full.names = TRUE)

  withr::with_dir(Folder, {system("quarto render")})

  DocsFolder <- file.path(Folder, "docs")

  files <- list.files(DocsFolder, full.names=TRUE)

  pages <- c(
    "Data.html",
    "help.html",
    "Historical.html",
    "Instrument.html",
    "Miscellaneous.html"
  )

  for (p in pages) {
    html <- file.path(DocsFolder, p)
    snapshot_name <- paste0(tools::file_path_sans_ext(p), ".png")
    png <- file.path(tmp, snapshot_name)

    webshot2::webshot(
      url    = paste0("file:///", normalizePath(html)),
      file   = png,
      vwidth = 1400,
      vheight = 1000,
      delay  = 0.5
    )
  }

  PNGs <- list.files(tmp, ".png", full.names=TRUE)

  for (png_file in PNGs) {

  name <- tools::file_path_sans_ext(basename(png_file))

  img <- png::readPNG(png_file)

  fig <- function() {
    grid::grid.raster(img)
  }

  vdiffr::expect_doppelganger(
    title = paste("website-", name),
    fig = fig
  )
}

})