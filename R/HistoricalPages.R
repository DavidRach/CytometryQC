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





