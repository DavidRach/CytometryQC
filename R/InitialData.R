#' Personalized Script that runs the first processing run of the data
#' 
#' @param name Desired name for the instrument
#' @param outpath internal
#' @param manufacturer Instrument Manufacturer
#' @param TheFCSFolderPath internal
#' @param CytekbioExportFolderPath internal
#' 
#' @return Archive.csv files appear in their folder
#' 
#' @noRd
InitialData <- function(name, outpath, manufacturer, 
  TheFCSFolderPath, CytekbioExportFolderPath){
  
  filename <- paste0("InitialData_", name, ".R")
  StorageLocation <- file.path(outpath, filename)
  
  if (manufacturer=="Cytek"){
    TheSetup <- file.path(CytekbioExportFolderPath, "Setup")
  } else {TheSetup <- CytekbioExportFolderPath}

  FirstChunk <- sprintf('library(purrr)
  name <- "%s"
  Computer <- getwd()
  MainFolder <- file.path(Computer, "data", name)
  WorkingFolder <- file.path(Computer, "data")
  Archive <- file.path(MainFolder, "Archive")
  TheProcessed <- list.files(Archive)

  if(!any(stringr::str_detect(TheProcessed, "Application"))){
    SetupFolder <- "%s"
    TheSetupFiles <- list.files(SetupFolder, pattern="Application", full.names=TRUE)
    AppMatches <- TheSetupFiles
    file.copy(AppMatches, MainFolder)
    walk(.x=name, .f=Luciernaga:::AppQCParse, MainFolder=WorkingFolder)
  }

  if(!any(stringr::str_detect(TheProcessed, "Archived"))){
    SetupFolder <- "%s"
    TheSetupFiles <- list.files(SetupFolder, pattern="DailyQCR", full.names=TRUE)
    GainMatches <- TheSetupFiles
    if (!length(GainMatches) == 0){
      file.copy(GainMatches, MainFolder)
      walk(.x=name, .f=Luciernaga:::DailyQCParse, MainFolder=WorkingFolder)
    }
  } 

  if(!any(stringr::str_detect(TheProcessed, "Bead"))){
    FCSFolder <-  "%s"
    TheFCSFiles <- list.files(FCSFolder, pattern="fcs", full.names=TRUE)
    file.copy(TheFCSFiles, MainFolder)
    walk(.x=name, .f=Luciernaga:::QCBeadParse, MainFolder=WorkingFolder)
  }', name, TheSetup, TheSetup, TheFCSFolderPath)

  cat(FirstChunk, file = StorageLocation)
}