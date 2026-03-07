#' Checks for an existing InstrumentQC folder, if not present creates the folder
#' and populates it with the required website components
#' 
#' @param organization_name The organization name, ex. UMGCC FCSS
#' @param organization_website The organizations website, ex. https://www.medschool.umaryland.edu/cibr/core/umgccc_flow/
#' @param githubusername The GitHub user name, ex. umgccfcss
#' @param institution_name The institution name, ex. University of Maryland, Baltimore
#' @param AlternateDirectory Provide a file path if desire to save somewhere not Documents folder. 
#' @param FolderName Default InstrumentQC2, sets folder/repository name
#' @param SetUpGit Default is FALSE, when git token credentials are present, 
#' it will generate a git repository for the folder and push to GitHub.
#' 
#' @importFrom utils write.csv
#' @importFrom usethis create_project use_git use_github
#' 
#' @return A generalized InstrumentQC folder to which additional elements can be added
#' 
#' @export
#' 
#' @examples
#' 
#' tmp <- withr::local_tempdir(pattern = "CytometryQC")
#'  FolderSetup(
#'  organization_name = "UMGCCC FCSR",
#'  organization_website = "https://www.medschool.umaryland.edu/cibr/core/umgccc_flow/",
#'  githubusername = "umgcccfcsr",
#'  institution_name = "University of Maryland, Baltimore",
#'  AlternateDirectory = tmp # For Testing Only
#'  )
#' 
#' 
FolderSetup <- function(organization_name="UMGCCC FCSR",
  organization_website="https://www.medschool.umaryland.edu/cibr/core/umgccc_flow/",
  githubusername="umgcccfcsr", institution_name="University of Maryland, Baltimore", 
  AlternateDirectory=NULL, FolderName="InstrumentQC2", SetUpGit=FALSE){
  
  # TheURL <- paste0("https://", githubusername, ".github.io/", FolderName, "/")

  FolderPattern <- paste0("^", FolderName, "$")

  if(!is.null(AlternateDirectory)){(DocumentsPath <- AlternateDirectory)
  } else {DocumentsPath <- OperatingSystemCheck()}

  InstrumentQC <- list.files(DocumentsPath, pattern=FolderPattern,
   full.names=TRUE)
  
  # Checking for InstrumentQC folder
  if (length(InstrumentQC) > 0){
    message(FolderName, " folder found, skipping")
  } else {
    # Creating InstrumentQC folder and subfolders
    message(FolderName, " folder not found, creating")
    dir.create(file.path(DocumentsPath, FolderName), showWarnings = FALSE)
    InstrumentQCPath <- file.path(DocumentsPath, FolderName)
    dir.create(file.path(InstrumentQCPath, "data"), showWarnings = FALSE)
    dir.create(file.path(InstrumentQCPath, "docs"), showWarnings = FALSE)
    dir.create(file.path(InstrumentQCPath, "images"), showWarnings = FALSE)
    dir.create(file.path(InstrumentQCPath, "R"), showWarnings = FALSE)
    message("Folders Created")

    # Accesing CytometryQC extdata folder
    
    PackageLocation <- system.file(package = "CytometryQC")

    # Copying over License
    License <- list.files(PackageLocation, pattern="LICENSE", full.names=TRUE)
    Report <- file.copy(from=License, to=InstrumentQCPath, recursive=FALSE)

    # Copying over Styles
    StylesLocation <- file.path(PackageLocation, "extdata")
    Styles <- list.files(StylesLocation, pattern="styles", full.names=TRUE)
    Report <- file.copy(from=Styles, to=InstrumentQCPath, recursive=FALSE)

    # Copying over Maintenance.csv
    Maintenance <- list.files(StylesLocation, pattern="Maintenance", full.names=TRUE)
    Report <- file.copy(from=Maintenance, to=InstrumentQCPath, recursive=FALSE)

    # Copying over Images
    ImagesLocation <- file.path(PackageLocation, "extdata", "images")
    ImageMoveLocation <- file.path(InstrumentQCPath, "images")
    Images <- list.files(ImagesLocation, pattern="png", full.names=TRUE)
    Report <- file.copy(from=Images, to=ImageMoveLocation, recursive=FALSE)

    # Creating 404.qmd
    QMD_404(outpath=InstrumentQCPath, organization_name=organization_name, 
    githubusername=githubusername, FolderName = FolderName)

    # Creating help.qmd
    QMD_help(outpath = InstrumentQCPath)

    # Creating Miscellaneous.qmd
    QMD_Miscellaneous(outpath = InstrumentQCPath)

    # Creating Instrument.qmd
    QMD_Instrument(outpath = InstrumentQCPath)

    # Creating Historical.qmd
    QMD_Historical(outpath = InstrumentQCPath)

    # Creating Historical.qmd
    QMD_index(outpath = InstrumentQCPath, organization_name=organization_name,
     organization_website=organization_website)

    # Creating Historical.qmd
    QMD_Data(outpath = InstrumentQCPath, organization_name=organization_name,
      organization_website=organization_website)

    # Creating quarto.yaml
    QMD_yaml(outpath=InstrumentQCPath, organization_name=organization_name, 
    githubusername=githubusername, institution_name=institution_name,
    FolderName = FolderName)

    # Creating README.md
    QMD_README(outpath=InstrumentQCPath, organization_name=organization_name,
     organization_website=organization_website)
    
    # Doing Something
    
    CSV_Gates(outpath=InstrumentQCPath)

    # create_project(InstrumentQCPath, open=FALSE)

    #if (SetUpGit == TRUE){
    #    setwd(InstrumentQCPath)
    #    use_git(message="Initial project setup")
    #    use_github(private=FALSE)
    #  }

  }
  
  return(InstrumentQCPath)
}