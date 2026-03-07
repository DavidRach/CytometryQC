#' Internal, adds an Instrument.R script for regular data processing
#' 
#' @param name See \code{\link{AddInstruments}}
#' @param outpath Location to save file, default is InstrumentQC folder
#' @param manufacturer See \code{\link{AddInstruments}}
#' @param TheFCSFolderPath See \code{\link{AddInstruments}}
#' @param CytekbioExportFolderPath See \code{\link{AddInstruments}}
#' @param timepointType Whether QC .fcs files are "single" or "double" (ie, before and after)
#' @param FolderName Default is InstrumentQC2
#' 
#' @return An instrument.R file
#' 
#' @noRd
AddInstrumentScript <- function(name, outpath, manufacturer, 
  TheFCSFolderPath, CytekbioExportFolderPath, timepointType,
  FolderName){

filename <- paste0("TheScript_", name, ".R")
StorageLocation <- file.path(outpath, filename)
  
if (manufacturer == "Cytek"){

  CytekScript(name=name, TheFCSFolderPath=TheFCSFolderPath,
    CytekbioExportFolderPath=CytekbioExportFolderPath,
    timepointType=timepointType, StorageLocation=StorageLocation,
    FolderName=FolderName
  )

} else if (manufacturer != "Cytek"){

  OtherScript(name=name, outpath=outpath, manufacturer=manufacturer,
  TheFCSFolderPath=TheFCSFolderPath)
} 
  
}
