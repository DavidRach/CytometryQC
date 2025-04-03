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
  
if(OperatingSystem == "Linux"){OS <- "Linux"
} else if (OperatingSystem == "FreeBSD"){OS <- "FreeBSD"
}else if (OperatingSystem == "Windows"){OS <- "Windows"
} else if (OperatingSystem == "Darwin"){OS <- "Mac"
} else {stop("Operating System Not Recognized")}
  
return(OS)
}