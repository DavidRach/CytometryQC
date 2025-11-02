
#' Build a standalone offline webpage to visualize QC from BD instruments
#' that run the Chorus software. Please note, it is not yet tied in to the 
#' rest of the CytometryQC infrastructure, as this was a spontaneous
#' Cytometry Discord fun project.
#' 
#' @param inpath A file path to the folder that contains the Chorus QC.pdf files.
#' @param FolderName Default "QC_Chorus", the folder name under Documents where to 
#' keep this project. 
#' @param organization_name Default is "UMGCC FCSS", provide your own organization
#' name within " " for it to appear on the offline webpage
#' @param institution_name Default is "University of Maryland, Baltimore", provide
#' your own institutions name within " " for it to appear on the offline webpage
#' 
Standalone_Chorus <- function(input, organization_name="UMGCC FCSS",
  institution_name="University of Maryland, Baltimore", 
  FolderName="QC_Chorus"){
  
  FolderPattern <- paste0("^", FolderName, "$")

  DocumentsPath <- OperatingSystemCheck()
  InstrumentQC <- list.files(DocumentsPath, pattern=FolderPattern,
   full.names=TRUE)
  
  if (length(InstrumentQC) > 0){message(FolderName, " folder found")
  } else {message(FolderName, " folder not found, creating")
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
    githubusername="", FolderName = FolderName)

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
     organization_website="")

    # Historical.qmd
    QMD_Data(outpath = InstrumentQCPath, organization_name=organization_name,
      organization_website="")

    # quarto.yaml
    QMD_yaml(outpath=InstrumentQCPath, organization_name=organization_name, 
    githubusername="", institution_name=institution_name,
    FolderName = FolderName)

    # README.md
    QMD_README(outpath=InstrumentQCPath, organization_name=organization_name,
     organization_website="")
    
    CSV_Gates(outpath=InstrumentQCPath)

    create_project(InstrumentQCPath, open=FALSE)
  }
  
  return(InstrumentQCPath)
}