
#' Creates generic Instrument.qmd file
#'
#' @param outpath Location to save file to, default is InstrumentQC folder
#' 
#' @return A instrument.qmd file placeholder
#' 
#' @noRd
QMD_Instrument <- function(outpath){
  StorageLocation <- file.path(outpath, "Instrument.qmd")

  content <- '---
format:
  dashboard:
    orientation: columns
    scrolling: true
---

This is a placeholder
'
    
  cat(content, file = StorageLocation)
}