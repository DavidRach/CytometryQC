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
  TheFCSFolderPath, CytekbioExportFolderPath, timepointType){
  
  filename <- paste0("InitialData_", name, ".R")
  StorageLocation <- file.path(outpath, filename)
  
  FirstChunk <- sprintf('library(purrr)
  name <- "%s"
  Computer <- getwd()
  MainFolder <- file.path(Computer, "data", name)
  WorkingFolder <- file.path(Computer, "data")
  Archive <- file.path(MainFolder, "Archive")
  TheProcessed <- list.files(Archive)
  ', name)
  
  if (manufacturer=="Cytek"){
    TheSetup <- file.path(CytekbioExportFolderPath, "Setup")

SecondChunk <- sprintf('
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

', CytekbioExportFolderPath, TheSetup)
    
  } else {
SecondChunk <- '

'
}

if (timepointType=="single"){

ThirdChunk <- sprintf('
if(!any(stringr::str_detect(TheProcessed, "Holistic"))){
    FCSFolder <-  "%s"
    TheFCSFiles <- list.files(FCSFolder, pattern="fcs", full.names=TRUE)
    file.copy(TheFCSFiles, MainFolder)
    walk(.x=name, .f=Luciernaga:::HolisticQCParse, MainFolder=WorkingFolder,
    Template=CSTGates, subsets="Staining")
}  
  
', TheFCSFolderPath)
  
} else {
ThirdChunk <- sprintf('
 
if(!any(stringr::str_detect(TheProcessed, "Bead"))){
    FCSFolder <-  "%s"
    TheFCSFiles <- list.files(FCSFolder, pattern="fcs", full.names=TRUE)
    file.copy(TheFCSFiles, MainFolder)
    walk(.x=name, .f=Luciernaga:::QCBeadParse, MainFolder=WorkingFolder)
}
    
', TheFCSFolderPath)
}

  cat(FirstChunk, SecondChunk, ThirdChunk, file = StorageLocation)
}