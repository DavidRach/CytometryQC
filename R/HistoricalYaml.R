
#' Internal function that does as written. 
#' 
#' @importFrom stringr str_detect
#' 
#' @noRd
HistoricalYAML <- function(InstrumentFolder, githubusername){
  # githubusername <- "umgccfcss"
  TheURL <- paste0("https://", githubusername, ".github.io/InstrumentQC2/")

  Index <- list.files(InstrumentFolder, pattern="index.qmd", full.names=TRUE)
  if (!length(Index) == 1){stop("No Index File Found")}

  Yaml <- list.files(InstrumentFolder, pattern="_quarto.yml", full.names=TRUE)
  if (!length(Yaml) == 1){stop("No YML File Found")}
  Data <- readLines(Yaml)
  Pattern <- '        href:'
  Matches <- Data[which(str_detect(Data, Pattern))]

  Modified <- sub(pattern = "href: (.*?)\\.qmd",
  replacement = paste0("href: ", TheURL, "\\1"),
  x = Matches
  )

  Data[which(str_detect(Data, Pattern))] <- Modified

  Pattern <- 'Historical_'
  Matches <- Data[which(str_detect(Data, Pattern))]
  Updated <- sub("InstrumentQC2/Historical_", "", Matches)

  Data[which(str_detect(Data, Pattern))] <- Updated

  Pattern <- '      href: index.qmd'
  Matches <- Data[which(str_detect(Data, Pattern))]
  Updated <- sub("index.qmd", TheURL, Matches)
  Data[which(str_detect(Data, Pattern))] <- Updated

  Pattern <- '      href: help.qmd'
  Matches <- Data[which(str_detect(Data, Pattern))]
  Updated <- sub("help.qmd", paste0(TheURL, "help"), Matches)
  Data[which(str_detect(Data, Pattern))] <- Updated

  Pattern <- '      href: Data.qmd'
  Matches <- Data[which(str_detect(Data, Pattern))]
  Updated <- sub("Data.qmd", paste0(TheURL, "Data"), Matches)
  Data[which(str_detect(Data, Pattern))] <- Updated

  writeLines(Data, Yaml)
}
