#' Checks Operating System, sets appropiate file path
#' 
#' @return The Path to the Documents Folder
#' 
#' @export
OperatingSystemCheck <- function(){
  OperatingSystem <- Sys.info()["sysname"]
  
if (OperatingSystem == "Linux"){OS <- "Linux"
  DocumentsFolder <- Sys.getenv("HOME")
  DocumentsPath <- file.path(DocumentsFolder, "Documents")
} else if (OperatingSystem == "FreeBSD"){OS <- "FreeBSD"
  warning("Missing next steps for your Operating System, please remind maintainer")
}else if (OperatingSystem == "Windows"){OS <- "Windows"
  DocumentsFolder <- Sys.getenv("HOME")
  Components <- strsplit(DocumentsFolder, "[/\\\\]")[[1]]
  DocumentsPath <- do.call(file.path, as.list(Components))
} else if (OperatingSystem == "Darwin"){OS <- "Mac"
  warning("Missing next steps for your Operating System, please remind maintainer")
} else {stop("Operating System Not Recognized")}
  return(DocumentsPath)
}


#' Checks for existing InstrumentQC folder, if none present
#' creates a new one and populates with the correct folders
#' 
#' @importFrom utils write.csv
#' @importFrom usethis create_project
#' @importFrom quarto quarto_render
#' @importFrom usethis use_git
#' @importFrom usethis use_github
#' 
#' @return If folder not present, creates folder under user Documents
#' folder
#' 
#' @export
#' 
FolderSetup <- function(){

  DocumentsPath <- OperatingSystemCheck()
  InstrumentQC <- list.files(DocumentsPath, pattern="^InstrumentQC2$",
   full.names=TRUE)
  
  if (length(InstrumentQC) > 0){message("InstrumentQC folder found")
  } else {message("InstrumentQC folder not found, creating")
    FolderName <- "InstrumentQC2"
    dir.create(file.path(DocumentsPath, FolderName), showWarnings = FALSE)
    InstrumentQCPath <- file.path(DocumentsPath, FolderName)
    dir.create(file.path(InstrumentQCPath, "data"), showWarnings = FALSE)
    dir.create(file.path(InstrumentQCPath, "docs"), showWarnings = FALSE)
    dir.create(file.path(InstrumentQCPath, "images"), showWarnings = FALSE)
    message("Folders Created")
    
    PackageLocation <- system.file(package = "CytometryQC")

    # License
    License <- list.files(PackageLocation, pattern="LICENSE", full.names=TRUE)
    Report <- file.copy(from=License, to=InstrumentQCPath, recursive=FALSE)

    # Styles
    StylesLocation <- file.path(PackageLocation, "extdata")
    Styles <- list.files(StylesLocation, pattern="styles", full.names=TRUE)
    Report <- file.copy(from=Styles, to=InstrumentQCPath, recursive=FALSE)

    # Images
    ImagesLocation <- file.path(PackageLocation, "extdata", "images")
    ImageMoveLocation <- file.path(InstrumentQCPath, "images")
    Images <- list.files(ImagesLocation, pattern="png", full.names=TRUE)
    Report <- file.copy(from=Images, to=ImageMoveLocation, recursive=FALSE)

    # 404.qmd
    QMD_404(outpath=InstrumentQCPath, organization="UMGCC FCSS", 
    github_page="umgccfcss.github.io")

    # help.qmd
    QMD_help(outpath = InstrumentQCPath)

    # Miscellaneous.qmd
    QMD_Miscellaneous(outpath = InstrumentQCPath)

    # Instrument.qmd
    QMD_Instrument(outpath = InstrumentQCPath)

    # Historical.qmd
    QMD_Historical(outpath = InstrumentQCPath)

    # Historical.qmd
    QMD_index(outpath = InstrumentQCPath)

    # Historical.qmd
    QMD_Data(outpath = InstrumentQCPath)

    # quarto.yaml
    QMD_yaml(outpath=InstrumentQCPath, organization="UMGCC FCSS", 
    github_page="umgccfcss.github.io", institution="University of Maryland, Baltimore")

    create_project(InstrumentQCPath, open=FALSE)
    #setwd(InstrumentQCPath)
    #use_git(message="Initial project setup")
    #use_github(private=FALSE)
  }
  
  return(InstrumentQCPath)
}