

#' Creates generic Miscellaneous.qmd file
#'
#' @param outpath Location to save the file, default is the InstrumentQC folder
#' 
#' @return A Miscellaneous.qmd file placeholder
#' 
#' @noRd
QMD_Miscellaneous <- function(outpath){
  
  StorageLocation <- file.path(outpath, "Miscellaneous.qmd")

  content <- '---
project:
  output-dir: docs/
toc: true
---

This is a placeholder
'
  cat(content, file = StorageLocation)
}
