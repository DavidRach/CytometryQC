#' Internal, writes a Gates.csv for beads
#' 
#' @param outpath Location to store file
#' @param name Name for file, default is "Gates"
#' 
#' @noRd
CSV_Gates <- function(outpath, name="Gates"){
  Data <- data.frame(
    alias=c("nonDebris"),
    pop=c("+"),
    parent=c("root"),
    dims=c("FSC-A"),
    gating_method=c("gate_mindensity"),
    gating_args=c("gate_range=c(3e4, 5e4)"),
    collapseDataForGating=c("FALSE"),
    groupBy=c("NA"),
    preprocessing_method=c("NA"),
    preprocessing_args=c("")
  )

  FileName <- paste0(name, ".csv")
  StorageLocation <- file.path(outpath, FileName)
  write.csv(Data, StorageLocation, row.names=FALSE)
}
  