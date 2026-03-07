
#' Internal, iterates out Year.qmd files for the Historical instrument repository
#' 
#' @param x The iterated year
#' @param TheFile Location of the Year.qmd template file
#' @param Instrument Name of the instrument
#' 
#' @importFrom stringr str_extract str_detect str_remove_all
#' 
#' @return A new Year.qmd file for respective year
#' 
#' @noRd
YearIterate <- function(x, TheFile, Instrument){
  Data <- readLines(TheFile)
  Data <- gsub("THISYEAR", x, Data)

  Pattern <- 'Dashboard data for the '
  Matches <- Data[which(str_detect(Data, Pattern))]
  Here <- which(str_detect(Data, Pattern))
  TheInstrumentName <- str_extract(Matches, "\\*\\*(.*?)\\*\\*") |> 
    str_remove_all("\\*")

  NewLine <- paste0("Dashboard contains historical data from **", x, '** for the **', TheInstrumentName, '**.')
  Data[Here] <- NewLine

  NewFileName <- paste0("Year", x, ".qmd")
  NewFileLocation <- sub("Year.qmd", NewFileName, TheFile)
  writeLines(Data, NewFileLocation)
}