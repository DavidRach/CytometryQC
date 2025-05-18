#' Checks data folder, determines instruments, creates GitHub project for each,
#'  checks for historical data and creates a year page for each, wraps up by updating
#'  the main webpage links
#' 
#' @param githubusername lower case no spaces
#' @param Archive Whether to use Bead or Holistic data. 
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
HistoricalPages <- function(githubusername="umgccfcss", Archive="Bead"){
  DocumentsPath <- OperatingSystemCheck()
  InstrumentQC <- list.files(DocumentsPath, pattern="^InstrumentQC2$",
   full.names=TRUE)
  DataFolder <- file.path(InstrumentQC, "data")

  TheInstruments <- list.dirs(DataFolder, full.names=FALSE, recursive=FALSE)

  # x <- TheInstruments[1]
  walk(.x=TheInstruments, .f=InstrumentHistory, githubusername=githubusername,
  Archive=Archive)
}

#' Internal use Historical Pages, takes iterated instrument and creates website
#' 
#' @importFrom dplyr %>%
#' 
#' @noRd
InstrumentHistory <- function(x, githubusername, Archive){
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
  GeneralizingYear(InstrumentFolder=NewFolder, githubusername = githubusername)

  # Iterate out the years

  Dataset <- file.path(InstrumentQCPath, "data", x, "Archive")

  if (Archive == "Bead"){
    DataFile <- list.files(Dataset, pattern="Bead", full.names=TRUE)
  } else if (Archive == "Holistic"){
    DataFile <- list.files(Dataset, pattern="Holistic", full.names=TRUE)
  }

  Dataset <- read.csv(DataFile, check.names=FALSE)
  Dataset$DATE <- lubridate::ymd(Dataset$DATE)
  TheseYears <- Dataset |> dplyr::arrange(DATE) |>
    dplyr::pull(DATE) |> lubridate::year() |> unique()

  Instrument <- x
  # x <- TheseYears[1]
  walk(.x=TheseYears, .f=YearIterate, TheFile=New,
     Instrument=Instrument)
  
  # Update the .yml again
  TheYearQMDs <- list.files(NewFolder, pattern="Year")
  TheYearQMDs <- TheYearQMDs[!str_detect(TheYearQMDs, "^Year.qmd$")]

  Yaml <- list.files(NewFolder, pattern="_quarto.yml", full.names=TRUE)
  if (!length(Yaml) == 1){stop("No YML File Found")}
  Data <- readLines(Yaml)

  # Returning index.qmd
  Pattern <- "      - text: \"THIS\""
  Pattern <- sub("THIS", Instrument, Pattern)
  Matches <- which(str_detect(Data, Pattern))
  Este <- tail(Matches, 1)+1
  Value <- Data[Este]
  Replacement <- paste0("href: ", "index", ".qmd")
  Value <- sub("href:.*", Replacement, Value)
  Data[Este] <- Value
  
  # Adding Years

  String3<- '
    - text: "Year"
      menu:'
  String3 <- unlist(strsplit(String3, "\n"))

  Returned <- unlist(map(.x=TheseYears, .f=YearAppend))
  Assembled <- c(String3, Returned) %>% .[. != ""]

  endpattern <- '    right:'
  Matches <- which(str_detect(Data, endpattern))
  Matches <- head(Matches, 1)

  Data1 <- c(Data[1:(Matches-1)], Assembled, Data[Matches:length(Data)])
  
  # Returning Yml
  writeLines(Data1, Yaml)


  

}

#' Small internal for yml assembly in historical instrument pages
#' 
#' @return yml text with year inserted. 
#' 
#' @noRd
YearAppend <- function(x){
  String4 <- '
      - text: "Placeholder"
        href: YearPlaceholder.qmd'

  String4 <- gsub("Placeholder", x, String4)
  String4 <- unlist(strsplit(String4, "\n"))
  return(String4)
}

#' Internal, iterates out Year.qmd files for the Historical instrument repository
#' 
#' @param x The iterated year
#' @param TheFile Location of the Year.qmd template file
#' @param Instrument Name of the instrument
#' 
#' @importFrom stringr str_extract
#' @importFrom stringr str_detect
#' @importFrom stringr str_remove_all
#' 
#' @return A new Year.qmd file for respective year
#' 
#' @noRd
YearIterate <- function(x, TheFile, Instrument){
  Data <- readLines(TheFile)
  Data <- gsub("THISYEAR", x, Data)

  Pattern <- 'Dashboard data for the '
  Matches <- Data[which(str_detect(Data, Pattern))]
  Here <- which(str_detect(Data, Pattern))
  TheInstrumentName <- str_extract(Matches, "\\*\\*(.*?)\\*\\*") |> 
    str_remove_all("\\*")

  NewLine <- paste0("Dashboard contains historical data from **", x, '** for the **', TheInstrumentName, '**.')
  Data[Here] <- NewLine

  NewFileName <- paste0("Year", x, ".qmd")
  NewFileLocation <- sub("Year.qmd", NewFileName, TheFile)
  writeLines(Data, NewFileLocation)
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
