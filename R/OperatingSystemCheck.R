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
