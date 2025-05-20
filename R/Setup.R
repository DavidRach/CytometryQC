#' Checks Operating System, sets appropiate file path to Documents folder
#' 
#' @return File path to Documents folder
#' 
#' @noRd 
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


#' Checks for an existing InstrumentQC folder, if not present creates the folder
#' and populates it with the required website components
#' 
#' @param organization_name The organization name, ex. UMGCC FCSS
#' @param organization_website The organizations website, ex. https://www.medschool.umaryland.edu/cibr/core/umgccc_flow/
#' @param githubusername The GitHub user name, ex. umgccfcss
#' @param institution_name The institution name, ex. University of Maryland, Baltimore
#' @param SetUpGit Default is FALSE, when git token credentials are present, 
#' it will generate a git repository for the folder and push to GitHub. 
#' 
#' @importFrom utils write.csv
#' @importFrom usethis create_project
#' @importFrom quarto quarto_render
#' @importFrom usethis use_git
#' @importFrom usethis use_github
#' 
#' @return A generalized InstrumentQC folder to which additional elements can be added
#' 
#' @export
#' 
#' @examples
#' 
#' \dontrun{
#' FolderSetup()
#' }
#' 
FolderSetup <- function(SetUpGit=FALSE, organization_name="UMGCC FCSS",
  organization_website="https://www.medschool.umaryland.edu/cibr/core/umgccc_flow/",
  githubusername="umgccfcss", institution_name="University of Maryland, Baltimore"){
  
  TheURL <- paste0("https://", githubusername, ".github.io/InstrumentQC2/")

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

    # Maintenance.csv
    Maintenance <- list.files(StylesLocation, pattern="Maintenance", full.names=TRUE)
    Report <- file.copy(from=Maintenance, to=InstrumentQCPath, recursive=FALSE)

    # Images
    ImagesLocation <- file.path(PackageLocation, "extdata", "images")
    ImageMoveLocation <- file.path(InstrumentQCPath, "images")
    Images <- list.files(ImagesLocation, pattern="png", full.names=TRUE)
    Report <- file.copy(from=Images, to=ImageMoveLocation, recursive=FALSE)

    # 404.qmd
    QMD_404(outpath=InstrumentQCPath, organization_name=organization_name, 
    githubusername=githubusername)

    # help.qmd
    QMD_help(outpath = InstrumentQCPath)

    # Miscellaneous.qmd
    QMD_Miscellaneous(outpath = InstrumentQCPath)

    # Instrument.qmd
    QMD_Instrument(outpath = InstrumentQCPath)

    # Historical.qmd
    QMD_Historical(outpath = InstrumentQCPath)

    # Historical.qmd
    QMD_index(outpath = InstrumentQCPath, organization_name=organization_name,
     organization_website=organization_website)

    # Historical.qmd
    QMD_Data(outpath = InstrumentQCPath, organization_name=organization_name,
      organization_website=organization_website)

    # quarto.yaml
    QMD_yaml(outpath=InstrumentQCPath, organization_name=organization_name, 
    githubusername=githubusername, institution_name=institution_name)

    # README.md
    QMD_README(outpath=InstrumentQCPath, organization_name=organization_name,
     organization_website=organization_website)

    create_project(InstrumentQCPath, open=FALSE)

    if (SetUpGit == TRUE){
        setwd(InstrumentQCPath)
        use_git(message="Initial project setup")
        use_github(private=FALSE)
      }
    }
  
  return(InstrumentQCPath)
}