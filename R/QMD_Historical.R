
#' Creates generic Historical.qmd file
#'
#' @param outpath The location to save the file, default is InstrumentQC folder
#' 
#' @return A Historical.qmd file placeholder
#' 
#' @noRd
QMD_Historical <- function(outpath){
  
  StorageLocation <- file.path(outpath, "Historical.qmd")

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