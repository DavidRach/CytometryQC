#' Checks data folder, determines instruments, creates GitHub project for each,
#'  checks for historical data and creates a year page for each, wraps up by updating
#'  the main webpage links
#' 
#' @importFrom purrr walk
#' 
#' @export
#' 
#' @return Assembled Historical Websites
#' 
#' @examples
#' 
#' A <- 2 + 2
HistoricalPages <- function(githubusername="umgccfcss"){
  DocumentsPath <- OperatingSystemCheck()
  InstrumentQC <- list.files(DocumentsPath, pattern="^InstrumentQC2$",
   full.names=TRUE)
  DataFolder <- file.path(InstrumentQC, "data")

  TheInstruments <- list.dirs(DataFolder, full.names=FALSE, recursive=FALSE)

  # x <- TheInstruments[3]
  walk(.x=TheInstruments, .f=InstrumentHistory, githubusername=githubusername)
}

#' Internal use Historical Pages, takes iterated instrument and creates website
#' 
#' @noRd
InstrumentHistory <- function(x, githubusername){
  DocumentsPath <- OperatingSystemCheck()
  InstrumentQC <- list.files(DocumentsPath, pattern="^InstrumentQC2$",
   full.names=TRUE)
  InstrumentQCPath <- file.path(DocumentsPath, "InstrumentQC2")
  TheseFolders <- list.dirs(DocumentsPath, full.names=FALSE, recursive=FALSE)
  HistoryPresent <- x %in% TheseFolders

  if (HistoryPresent == FALSE){
    dir.create(file.path(DocumentsPath, x), showWarnings = FALSE)
    NewFolder <- file.path(DocumentsPath, x)
    dir.create(file.path(NewFolder, "data"), showWarnings = FALSE)
    dir.create(file.path(NewFolder, "docs"), showWarnings = FALSE)
    dir.create(file.path(NewFolder, "images"), showWarnings = FALSE)
  } else { # Only creates a website folder once per instrument. 
    Status <- NULL
    return(Status)
  }

  NewFolder <- file.path(DocumentsPath, x)

  PackageLocation <- system.file(package = "CytometryQC")

  # License
  LicenseLocation <- file.path(PackageLocation, "extdata")
  License <- list.files(LicenseLocation, pattern="LICENSE", full.names=TRUE)
  Report <- file.copy(from=License, to=NewFolder, recursive=FALSE)

  # Styles
  StylesLocation <- file.path(PackageLocation, "extdata")
  Styles <- list.files(StylesLocation, pattern="styles", full.names=TRUE)
  Report <- file.copy(from=Styles, to=NewFolder, recursive=FALSE)

  # Images
  ImagesLocation <- file.path(PackageLocation, "extdata", "images")
  ImageMoveLocation <- file.path(NewFolder, "images")
  Images <- list.files(ImagesLocation, pattern="hex.svg", full.names=TRUE)
  Report <- file.copy(from=Images, to=ImageMoveLocation, recursive=FALSE)

  # Copying over new Index Page
  TheIndexDraft <- paste0("Historical_", x, ".qmd")
  TheIndex <- list.files(InstrumentQCPath, pattern=TheIndexDraft,
   full.names=TRUE)
  Report <- file.copy(from=TheIndex, to=NewFolder, recursive=FALSE)
  Old <- file.path(NewFolder, TheIndexDraft)
  New <- file.path(NewFolder, "index.qmd")
  file.rename(from = Old, to = New)

  # Copying over existing quarto.yml
  Yaml <- list.files(InstrumentQCPath, pattern="_quarto.yml",
    full.names=TRUE)
  Report <- file.copy(from=Yaml, to=NewFolder, recursive=FALSE)
  HistoricalYAML(InstrumentFolder=NewFolder, githubusername = githubusername)

  # Copying over equivalent instrument.qmd
  pattern <- paste0("^", x, ".qmd")
  InstrumentQMD <- list.files(InstrumentQCPath, pattern=pattern,
    full.names=TRUE)
  Report <- file.copy(from=InstrumentQMD, to=NewFolder, recursive=FALSE)
  pattern <- gsub("^", "", fixed=TRUE, pattern)
  Old <- file.path(NewFolder, pattern)
  New <- file.path(NewFolder, "Year.qmd")
  file.rename(from = Old, to = New)

}

HistoricalYAML <- function(InstrumentFolder, githubusername){
  # githubusername <- "umgccfcss"
  TheURL <- paste0("https://", githubusername, ".github.io/InstrumentQC2/")

  Index <- list.files(InstrumentFolder, pattern="index.qmd", full.names=TRUE)
  if (!length(Index) == 1){stop("No Index File Found")}

  Yaml <- list.files(InstrumentFolder, pattern="_quarto.yml", full.names=TRUE)
  if (!length(Yaml) == 1){stop("No YML File Found")}
  Data <- readLines(Yaml)
  Pattern <- '        href:'
  Matches <- Data[which(str_detect(Data, Pattern))]

  Modified <- sub(pattern = "href: (.*?)\\.qmd",
  replacement = paste0("href: ", TheURL, "\\1"),
  x = Matches
  )

  Data[which(str_detect(Data, Pattern))] <- Modified

  Pattern <- 'Historical_'
  Matches <- Data[which(str_detect(Data, Pattern))]
  Updated <- sub("InstrumentQC2/Historical_", "", Matches)

  Data[which(str_detect(Data, Pattern))] <- Updated

  Pattern <- '      href: index.qmd'
  Matches <- Data[which(str_detect(Data, Pattern))]
  Updated <- sub("index.qmd", TheURL, Matches)
  Data[which(str_detect(Data, Pattern))] <- Updated

  Pattern <- '      href: help.qmd'
  Matches <- Data[which(str_detect(Data, Pattern))]
  Updated <- sub("help.qmd", paste0(TheURL, "help"), Matches)
  Data[which(str_detect(Data, Pattern))] <- Updated

  Pattern <- '      href: Data.qmd'
  Matches <- Data[which(str_detect(Data, Pattern))]
  Updated <- sub("Data.qmd", paste0(TheURL, "Data"), Matches)
  Data[which(str_detect(Data, Pattern))] <- Updated

  writeLines(Data, Yaml)
}

#' Internal
#' 
#' @param InstrumentFolder Location of the Historical Archive Repo for given instrument
#' @param githubusername User name all lower case no spaces
#' 
#' @importFrom stringr str_detect
#' 
#' @return Updated Year.qmd file ready for use. 
#' 
#' @noRd
GeneralizingYear <- function(InstrumentFolder, githubusername){
  Instrument <- basename(InstrumentFolder)
  #githubusername <- "UMGCCFCSS"

  Year <- list.files(InstrumentFolder, pattern="Year.qmd", full.names=TRUE)
  if (!length(Year) == 1){stop("No Year File Found")}
  Data <- readLines(Year)
  Pattern <- 'x <- MFI'
  Matches <- which(str_detect(Data, Pattern))-3
  Data <- Data[-(7:Matches)]

  String1 <- sprintf('
```{r}
#| message: FALSE
library(dplyr)
library(purrr)
library(stringr)
library(plotly)
library(Luciernaga)
library(lubridate)

CSV <- data_path <-                                                
    paste(                                                  
      "https://raw.githubusercontent.com",                  
      "%s", "InstrumentQC",                   
      "main", "data", "Hmm", "Archive", "BeadDataHmm.csv",                              
      sep = "/"                                             
    ) 

TheList <- c("Hmm")

Data <- read.csv(CSV, check.names=FALSE)
Data$DateTime <- lubridate::ymd_hms(Data$DateTime)
#tail(Data, 1)

QC_Cutoffs <- data_path <-                                                
    paste(                                                  
      "https://raw.githubusercontent.com",                  
      "%s", "InstrumentQC",                   
      "main", "data", "QC_Hmm.CSV",                              
      sep = "/"                                             
    )

QC_Cutoffs <- readLines(QC_Cutoffs)
```

', githubusername, githubusername)
  
String1 <- gsub("Hmm", Instrument, String1)
  
String2 <- '
```{r}
MaintenancePath <- data_path <-                                                
    paste(                                                  
      "https://raw.githubusercontent.com",                  
      "THISONE", "InstrumentQC",                   
      "main", "Maintenance.csv",                              
      sep = "/"                                             
    )
Maintenance <- read.csv(MaintenancePath, check.names=FALSE)
Maintenance <- Maintenance |> filter(!str_detect(reason, "lean"))
Maintenance <- Maintenance |> filter(instrument %in% TheList)
```

```{r}
TheTHISYEAR <- Data |> dplyr::filter(year(DateTime) == THISYEAR)

TheTHISYEAR <- HolisticToArchived(data=TheTHISYEAR, manufacturer="Cytek",
 baselinecutoffs=QC_Cutoffs, gainmultiplier = 2)
```

'
  
String2 <- gsub("THISONE", githubusername, String2)
  
String1 <- unlist(strsplit(String1, "\n"))
String2 <- unlist(strsplit(String2, "\n"))  

Pattern <- '```'
Matches <- which(str_detect(Data, Pattern))
UpTillHere <- tail(Matches, 1)+1
  
NewData <- c(Data[1:7], String1, String2, Data[8:UpTillHere])
writeLines(NewData, Year)
}
