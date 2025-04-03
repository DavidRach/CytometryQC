#' Checks for existing InstrumentQC folder, if none present
#' creates a new one and populates with the correct folders
#' 
#' @importFrom utils write.csv
#' 
#' @return If folder not present, creates folder under user Documents
#' folder
#' 
#' @export
#' 
FolderSetup <- function(){

OperatingSystem <- Sys.info()["sysname"]
  
if (OperatingSystem == "Linux"){OS <- "Linux"
} else if (OperatingSystem == "FreeBSD"){OS <- "FreeBSD"
  warning("Missing next steps for your Operating System, please remind maintainer")
}else if (OperatingSystem == "Windows"){OS <- "Windows"
  DocumentsFolder <- Sys.getenv("HOME")
  Components <- strsplit(DocumentsFolder, "[/\\\\]")[[1]]
  DocumentsPath <- do.call(file.path, as.list(Components))
} else if (OperatingSystem == "Darwin"){OS <- "Mac"
  warning("Missing next steps for your Operating System, please remind maintainer")
} else {stop("Operating System Not Recognized")}

  InstrumentQC <- list.files(DocumentsPath, pattern="^InstrumentQC2$", full.names=TRUE)
  
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
    License <- list.files(PackageLocation, pattern="LICENSE", full.names=TRUE)
    
    Report <- file.copy(from=License, to=InstrumentQCPath, recursive=FALSE)
  }
  
  
  
  
  
  return(InstrumentQCPath)
}