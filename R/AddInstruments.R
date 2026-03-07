#' Adds Instruments to the base Webpage
#' 
#' @param name Desired name for the instrument
#' @param manufacturer Instrument Manufacturer
#' @param uv Number of UV detectors
#' @param violet Number of violet detectors
#' @param blue Number of blue detectors
#' @param yellowgreen Number of yellow-green detectors
#' @param red Number of red detectors
#' @param organization_name Name of the organization, default NULL defaults to 
#' value provided during FolderSetup
#' @param organization_website Link to organization website, default NULL defaults
#' to value provided during FolderSetup
#' @param githubusername GitHub user name, default NULL defaults to value provided 
#' during FolderSetup
#' @param TheFCSFolderPath Default NULL sets path to Cytek file.path 
#' 'C:/CytekbioExport/Setup/DailyQC', for alternate file locations or manufacturers, please
#' provide a file.path to the fcs folder location
#' @param CytekbioExportFolderPath Default NULL sets path to Cytek file.path 'C:/CytekbioExport',
#'  for alternate locations of the CytekbioExport folder, please provide a file path.  
#' @param FolderName Default is InstrumentQC2
#' @param timepointType Whether QC .fcs files are "single" or "double" (ie, before and after)
#' @param datasource Whether to use the Archived, Bead or Holistic data .csv as source file 
#' 
#' @importFrom stringr str_which str_detect fixed str_replace
#' 
#' @return Updated webpage
#' 
#' @export
#' 
#' @examples
#' 
#' AddInstruments(name="5L", manufacturer="Cytek", uv=16, violet=16, blue=14,
#' yellowgreen=10, red=8, TheFCSFolderPath="C:/CytekbioExport/Setup/DailyQC")
#' 
AddInstruments <- function(name, manufacturer="Cytek", uv=16, violet=16,
 blue=14, yellowgreen=10, red=8, organization_name=NULL, organization_website=NULL,
 githubusername=NULL, TheFCSFolderPath=NULL, CytekbioExportFolderPath=NULL,
 FolderName="InstrumentQC2", timepointType="single", datasource="Both"){

  if (any(stringr::str_detect(datasource, "rchived"))){Gain <- TRUE
  } else {Gain <- FALSE}

  if (any(stringr::str_detect(datasource, "Gain"))){Gain <- TRUE
  } else {Gain <- FALSE}
  
  if (any(stringr::str_detect(datasource, "ead"))){MFI <- TRUE
  } else {MFI <- FALSE}

  if (any(stringr::str_detect(datasource, "MFI"))){MFI <- TRUE
  } else {MFI <- FALSE}

  if (any(stringr::str_detect(datasource, "olistic"))){Holistic <- TRUE
  } else {Holistic <- FALSE} 

  if (any(stringr::str_detect(datasource, "oth"))){Holistic <- TRUE
  } else {Holistic <- FALSE}

# Start Checks
  
  DocumentsPath <- OperatingSystemCheck()

  FolderPattern <- paste0("^", FolderName, "$")
  InstrumentQC <- list.files(DocumentsPath, pattern=FolderPattern,
    full.names=TRUE)

  if (length(InstrumentQC) == 0){stop("Run FolderSetup step first!")}
    
  InstrumentQCPath <- file.path(DocumentsPath, FolderName)
  DataPath <- file.path(DocumentsPath, FolderName, "data")

# Extracting previous inputs for Organization and URLs
  
  Yml <- list.files(InstrumentQCPath, pattern="yml", full.names=TRUE)
  Data <- readLines(Yml)
  pattern <- "  title:"
  Index <- which(str_detect(Data, pattern))
  Match <- Data[Index]

  if (is.null(organization_name)){
    organization_name <- gsub('.*\\"(.*?)\\".*', '\\1', Match)
  }

  pattern <- '      href: https:'
  Index <- which(str_detect(Data, pattern))
  Match <- Data[Index]

  if (is.null(githubusername)){
  githubusername <- str_extract(Match, "(?<=https://)[^/.]+(?=\\.github)")
  }

  README <- list.files(InstrumentQCPath, pattern="READ", full.names=TRUE)
  Data <- readLines(README)
  pattern <- "This"
  Index <- which(str_detect(Data, pattern))
  Match <- Data[Index]

  if (is.null(organization_website)){
  organization_website <- str_extract(Match, "(?<=\\()[^)]+(?=\\))")
  }

  # Manufacturer Forks

  if (!manufacturer %in% c("Cytek", "BD")){
    message("
    CytometryQC currently supported entries for manufacturer are
     `Cytek` and `BD`. 
    Refer to the vignettes for adding instruments from other
    manufacturers (https://davidrach.github.io/CytometryQC).
    If you want to help add support, please open a Discussion
    (https://github.com/DavidRach/CytometryQC/discussions)
    ")
  }

  # Creating Instrument Specific Folder
  
  Hits <- list.files(DataPath, pattern=name, full.names=TRUE)

  if (length(Hits) == 0){
    dir.create(file.path(DataPath, name),
     showWarnings = FALSE)
    dir.create(file.path(DataPath, name, "Archive"),
     showWarnings = FALSE)
  }
  
  References <- data.frame(
    Laser = c("uv", "violet", "blue", "yellowgreen", "red"),
    Detector = c(uv, violet, blue, yellowgreen, red)
  )

  # Add Instrument QMD file
  AddInstrumentQMD_Reorganized(name=name, manufacturer=manufacturer,
     outpath=InstrumentQCPath, organization_name=organization_name,
     organization_website=organization_website, timepointType=timepointType,
     datasource=datasource, references=References)
  
  Items <- list.files(InstrumentQCPath, pattern=paste0(name, ".qmd"),
   full.names=TRUE)
  
  
  if (length(Items) == 1){
    Draft <- readLines(Items)
    MFISegment <- grep("^## MFI", Draft)
    MFIAddition <- MFI_Display(uv=uv, violet=violet,
      blue=blue, yellowgreen=yellowgreen, red=red)
    
    GainSegment <- grep("^## Gain", Draft)
    GainAddition <- Gain_Display(uv=uv, violet=violet,
      blue=blue, yellowgreen=yellowgreen, red=red)
    
    RCVSegment <- grep("^## rCV", Draft)
    RCVAddition <- RCV_Display(uv=uv, violet=violet,
      blue=blue, yellowgreen=yellowgreen, red=red) 
    
    Draft[MFISegment] <- paste0("## MFI {.tabset}\n", MFIAddition)
    Draft[GainSegment] <- paste0("## Gain {.tabset}\n", GainAddition)
    Draft[RCVSegment] <- paste0("## rCV {.tabset}\n", RCVAddition)
      
    cat(Draft, file = Items, sep = "\n")

  } else {stop("This shouldn't have happened, check the InstrumentQC folder for a repeated Instrument.qmd file")}
  
  # Update .yaml
  Items <- list.files(InstrumentQCPath, pattern=paste0(name, ".qmd"),
   full.names=TRUE)
  Yaml <- list.files(InstrumentQCPath, pattern=".yml",
   full.names=TRUE)
  
  if (length(Items) ==1){
    Draft <- readLines(Yaml)
    Line1 <- '    - text: "Levey-Jennings Plots"'
    Line2 <- '      menu:'
    TheLocation <- str_which(Draft, fixed(Line1))

    InsertOne <- "      - text: \"Instrument\""
    InsertTwo <- "        href: Instrument.qmd"

    NewInsertOne <- str_replace(InsertOne, fixed("Instrument"), name)
    NewInsertTwo <- str_replace(InsertTwo, fixed("Instrument"), name)

    if (length(TheLocation) > 0) {
      Draft <- append(Draft, c(NewInsertOne, NewInsertTwo),
        after = TheLocation + 1)
    }

    Line1 <- "    - text: \"Historical\""
    Line2 <- "      menu:"
    TheLocation <- str_which(Draft, fixed(Line1))

    InsertThree <- "      - text: \"Instrument\""
    InsertFour <- "        href: Historical.qmd"

    HistoricalName <- paste0("Historical_", name)
    NewInsertOne <- str_replace(InsertThree, fixed("Instrument"), name)
    NewInsertTwo <- str_replace(InsertFour, fixed("Historical"), HistoricalName)

    QMD_HistoricalInstrument(outpath=InstrumentQCPath, manufacturer=manufacturer, name=name,
       organization_name=organization_name, organization_website=organization_website)

    if (length(TheLocation) > 0) {
      Draft <- append(Draft, c(NewInsertOne, NewInsertTwo),
        after = TheLocation + 1)
    }

    # Cleaning out placeholders if present
    Draft <- Draft[!(Draft %in% c(InsertOne, InsertTwo, InsertThree, InsertFour))]

    writeLines(Draft, Yaml)
  }
  
  # Add Instrument Script
  if (is.null(CytekbioExportFolderPath)){
    if (manufacturer == "Cytek"){
      CytekbioExportFolderPath <- file.path("C:", "CytekbioExport")
      message("Default CytekbioExportFolderPath used, if file path is not ", CytekbioExportFolderPath,
        " please provide correct location to the CytekbioExportFolderPath argument")
    } else {CytekbioExportFolderPath <- NULL}
  }

  if (is.null(TheFCSFolderPath)){
    if (manufacturer == "Cytek"){
      TheFCSFolderPath <- file.path("C:", "CytekbioExport", "Setup", "DailyQC")
      message("Default TheFCSFolderPath used, if file path is not ", TheFCSFolderPath,
       " please provide correct location to the TheFCSFolderPath argument")
    } else {
      stop("Please specify TheFCSFolderPath for the instrument")
    }
  }

  AddInstrumentScript(name=name, outpath=InstrumentQCPath, manufacturer=manufacturer, 
     TheFCSFolderPath=TheFCSFolderPath, CytekbioExportFolderPath=CytekbioExportFolderPath,
     timepointType=timepointType, FolderName=FolderName)
  
  # Update Index
  IndexUpdate(outpath=InstrumentQCPath, name=name, githubusername=githubusername,
  Gain=Gain, MFI=MFI, Holistic=Holistic)
  
  # Update Data
  DataUpdate(outpath=InstrumentQCPath, name=name)

  # Create Initial Processing Script
  InitialData(name=name, outpath=InstrumentQCPath, manufacturer=manufacturer,
    TheFCSFolderPath=TheFCSFolderPath,
    CytekbioExportFolderPath=CytekbioExportFolderPath,
    timepointType=timepointType)
  
  # Staff Update
  
  }