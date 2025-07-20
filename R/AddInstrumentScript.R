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
    FolderName=FolderName, timepointType=timepointType
  )

} else if (manufacturer != "Cytek"){

  OtherScript(name=name, outpath=outpath, manufacturer=manufacturer,
  TheFCSFolderPath=TheFCSFolderPath)
} 
  
}

#' Internal, adds a BD-specific Instrument.R script for regular data processing
#' 
#' @param name See \code{\link{AddInstruments}}
#' @param outpath Location to save file, default is InstrumentQC folder
#' @param manufacturer See \code{\link{AddInstruments}}
#' @param TheFCSFolderPath See \code{\link{AddInstruments}}
#' @param CytekbioExportFolderPath See \code{\link{AddInstruments}}
#' 
#' @return An instrument.R file
#' 
#' @noRd
OtherScript <- function(name, outpath, manufacturer, 
  TheFCSFolderPath, CytekbioExportFolderPath){
  
  print("Upps")
  
}


#' Internal, adds a Cytek-specific Instrument.R script for regular data processing
#' 
#' @param name See \code{\link{AddInstruments}}
#' @param TheFCSFolderPath See \code{\link{AddInstruments}}
#' @param CytekbioExportFolderPath See \code{\link{AddInstruments}}
#' @param StorageLocation File.path of final storage
#' @param FolderName Default is InstrumentQC2
#' @param timepointType Whether QC .fcs files are "single" or "double" (ie, before and after)
#' 
#' @return An instrument.R file
#' 
#' @noRd
CytekScript <- function(name, TheFCSFolderPath, CytekbioExportFolderPath,
   FolderName, timepointType){

TheSetupFolder <- file.path(CytekbioExportFolderPath, "Setup")


FirstChunk <- sprintf('
library(stringr)
library(purrr)

# Find out current date
Today <- Sys.Date()
Today <- as.Date(Today)
Tomorrow <- Today+1

WorkingDirectory <- file.path(CytometryQC::OperatingSystemCheck(), "%s")
', FolderName)
  
SecondChunk <- sprintf('
setwd(WorkingDirectory)

GitPresent <- list.files(WorkingDirectory, all.files=TRUE, pattern="\\.git$")

if (length(GitPresent) == 1){
TheRepo <- git2r::repository(WorkingDirectory)
git2r::pull(TheRepo)
}

# Locating Archive Folder
Instrument <- "%s"

MainFolder <- file.path(WorkingDirectory, "data")
WorkingFolder <- file.path(WorkingDirectory, "data", Instrument)
StorageFolder <- file.path(WorkingFolder, "Archive")
', name)
  
ThirdChunk <- '
# Gains
Gains <- list.files(StorageFolder, pattern="Archived", full.names=TRUE)
Gains <- read.csv(Gains[1], check.names = FALSE)
# if (length(Gains) == 0){source("InitialData.R")}
LastGainItem <- Gains |> dplyr::slice(1) |> dplyr::pull(DateTime)
LastGainItem <- lubridate::ymd_hms(LastGainItem)
LastGainItem <- as.Date(LastGainItem)
PotentialGainDays <- seq.Date(from = LastGainItem, to = Today, by = "day")
GainRemoveIndex <- which(PotentialGainDays == LastGainItem)
PotentialGainDays <- PotentialGainDays #[-GainRemoveIndex]

# MFIs
MFIs <- list.files(StorageFolder, pattern="Bead", full.names=TRUE)
MFIs <- read.csv(MFIs[1], check.names=FALSE)
# if (length(MFIs) == 0){source("InitialData.R")}
LastMFIItem <- MFIs |> dplyr::slice(1) |> dplyr::pull(DateTime)
LastMFIItem <- lubridate::ymd_hms(LastMFIItem)
LastMFIItem <- as.Date(LastMFIItem)
PotentialMFIDays <- seq.Date(from = LastMFIItem, to = Today, by = "day")
MFIRemoveIndex <- which(PotentialMFIDays == LastMFIItem)
PotentialMFIDays <- PotentialMFIDays #[-MFIRemoveIndex]

# Holistic # Spell out option here
  
# Usage
Apps <- list.files(StorageFolder, pattern="Application", full.names=TRUE)
Apps <- read.csv(Apps[1], check.names=FALSE)
# if (length(Apps) == 0){source("InitialData.R")}
LastAppsItem <- Apps |> dplyr::slice(1) |> dplyr::pull(DateTime)
LastAppsItem <- lubridate::ymd_hms(LastAppsItem)
LastAppsItem <- as.Date(LastAppsItem)
PotentialAppsDays <- seq.Date(from = LastAppsItem, to = Today, by = "day")
AppsRemoveIndex <- which(PotentialAppsDays == LastAppsItem)
PotentialAppsDays <- PotentialAppsDays #[-AppsRemoveIndex]
'
  
FourthChunk <- sprintf('
if (!length(PotentialGainDays) == 0){

SetupFolder <- "%s"
TheSetupFiles <- list.files(SetupFolder, pattern="DailyQC", full.names=TRUE)
Dates <- as.character(PotentialGainDays)
Dates <- gsub("-", "", Dates)
GainMatches <- TheSetupFiles[str_detect(TheSetupFiles, str_c(Dates, collapse = "|"))]

if (!length(GainMatches) == 0){
file.copy(GainMatches, WorkingFolder)
walk(.x=Instrument, .f=Luciernaga:::DailyQCParse, MainFolder=MainFolder)
}
} else {message("QC data has already been transferred")
  GainMatches <- NULL
}

', TheSetupFolder)
  
if (timepointType == "single"){
FifthChunk <- sprintf('
if (!length(PotentialMFIDays) == 0){
FCSFolder <-  "%s"
TheFCSFiles <- list.files(FCSFolder, pattern="fcs", full.names=TRUE, recursive=TRUE)
days <- format(PotentialMFIDays, "%%d")
MFIMatches <- TheFCSFiles[str_detect(basename(TheFCSFiles), str_c(days, collapse = "|"))]

if (!length(MFIMatches) == 0){
  file.copy(MFIMatches, WorkingFolder)
  walk(.x=Instrument, .f=Luciernaga:::QCBeadParse, MainFolder=MainFolder)
  }
  } else {message("QC data has already been transferred")
    MFIMatches <- NULL
  }
', TheFCSFolderPath)

} else {
FifthChunk <- sprintf('
if (!length(PotentialMFIDays) == 0){
FCSFolder <-  "%s"
MonthStyle <- format(Today, "%%Y-%%m")
MonthFolder <- paste0("QC ", MonthStyle)
MonthFolder <- file.path(FCSFolder, MonthFolder)
TheFCSFiles <- list.files(MonthFolder, pattern="fcs", full.names=TRUE, recursive=TRUE)
days <- format(PotentialMFIDays, "%%d")
MFIMatches <- TheFCSFiles[str_detect(basename(TheFCSFiles), str_c(days, collapse = "|"))]

if (!length(MFIMatches) == 0){
file.copy(MFIMatches, WorkingFolder)
walk(.x=Instrument, .f=Luciernaga:::QCBeadParse, MainFolder=MainFolder)
}
} else {message("QC data has already been transferred")
  MFIMatches <- NULL
}
', TheFCSFolderPath)
}

SixChunk <- sprintf('
if (!length(PotentialAppsDays) == 0){
    SetupFolder <- "%s"
    TheSetupFiles <- list.files(SetupFolder, pattern="Application", full.names=TRUE)
    MonthStyle <- format(Today, "%%Y-%%m")
    MonthStyle <- sub("([0-9]{4})-([0-9]{2})", "\\2-\\1", MonthStyle)
    MonthStyle <- gsub("-", " ", MonthStyle)
    MonthStyle <- paste0(MonthStyle, ".txt")
  
    AppMatches <- TheSetupFiles[str_detect(TheSetupFiles, str_c(MonthStyle, collapse = "|"))]
    
    if (!length(AppMatches) == 0){

      if (any(length(GainMatches)|length(MFIMatches) > 0)){
      file.copy(AppMatches, WorkingFolder)
      walk(.x=Instrument, .f=Luciernaga:::AppQCParse, MainFolder=MainFolder)
      }
      }
} else {message("QC data has already been transferred")
    AppMatches <- NULL
}
', TheCytekbioExportPath)

SeventhChunk <- '
if (any(length(PotentialGainDays)|length(PotentialMFIDays)|length(PotentialAppsDays) > 0)){
  
  if (any(length(GainMatches)|length(MFIMatches)|length(AppMatches) > 0)){

  if (length(GitPresent) == 1){

    # Stage to Git
    git2r::add(TheRepo, "*")
    TheCommitMessage <- paste0("Update for ", Instrument, " on ", Today)
    git2r::commit(TheRepo, message = TheCommitMessage)
    cred <- git2r::cred_token(token = "GITHUB_PAT")
    git2r::push(TheRepo, credentials = cred)
    message("Done ", Today)
  } else {message("No Git Repository found, Data still Processed")}
  } else {message("No files to process ", Today)}
} else {message("No files to process ", Today)}
} else {message("Automation Skipped ", Today)}
 '
 cat(FirstChunk, SecondChunk, ThirdChunk, FourthChunk, FifthChunk, SixChunk,
  SeventhChunk, file = StorageLocation)
}
  