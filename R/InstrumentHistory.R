
#' Internal use Historical Pages takes iterated instrument and creates website
#' 
#' @importFrom purrr walk map
#' @importFrom dplyr arrange pull %>%
#' @importFrom utils read.csv
#' @importFrom lubridate ymd year
#' @importFrom stringr str_replace_all str_detect fixed
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
  Dataset$DATE <- ymd(Dataset$DATE)
  TheseYears <- Dataset |> arrange(DATE) |>
   pull(DATE) |> year() |> unique()

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

  Pattern <- "      - text: \"THIS\""
  Pattern <- sub("THIS", Instrument, Pattern)
  Matches <- which(str_detect(Data, Pattern))
  Este <- tail(Matches, 1)+1
  Value <- Data[Este]
  Replacement <- paste0("href: ", "index", ".qmd")
  Value <- sub("href:.*", Replacement, Value)
  Data[Este] <- Value

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
  
  writeLines(Data1, Yaml)

  # Editing index.qmd

  Index <- list.files(NewFolder, pattern="index.qmd",
    full.names=TRUE)

  IndexYearUpdate(path=Index, years=TheseYears, instrument=Instrument)

  # Updating original index.qmd with hyperlinks

  Yml <- list.files(InstrumentQCPath, pattern=".yml",
    full.names=TRUE)
  
  Data <- readLines(Yml)
  Pattern <- paste0("Historical_", Instrument, ".qmd")
  Match <- which(str_detect(Data, Pattern))
  Phrase <- Data[Match]
  NewURL <- paste0("https://", githubusername, ".github.io/", Instrument, "/")
  Chunk1 <- str_replace_all(Phrase, fixed(Pattern), NewURL)
  Data[Match] <- Chunk1
  writeLines(Data, Yml)

  # Remove Year Files
  file.remove(New)

}