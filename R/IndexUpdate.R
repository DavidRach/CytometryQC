#' Triggered by AddInstrument, adds index.qmd chunks for that instrument.
#' 
#' @importFrom stringr str_replace_all
#' 
IndexUpdate <- function(outpath, name){
  InstrumentQC <- outpath
  Index <- list.files(InstrumentQC, pattern="index.qmd", full.names=TRUE)

  if (!length(Index) == 1){stop("No Index File Found")}

  Data <- readLines(Index)

  # Adding to TheList
  Pattern <- 'TheList <- c("Placeholder")'
  Matches <- which(Data == Pattern)
  if (length(Matches) == 1){
    Data[Matches] <- str_replace(Data[Matches], fixed("Placeholder"), name)
  } else {
    Pattern <- 'TheList <- c('
    Matches <- which(str_starts(Data, fixed(Pattern)))
    ThisString <- Data[Matches]
    ThisString <- sub("\\)$", paste0(", \"", name, "\")"), ThisString)
    Data[Matches] <- ThisString
  }

  Pattern <- '#MFIPlaceholder'
  Matches <- which(Data == Pattern)
  Chunk1 <- str_replace_all('
    MFI_%s <- Luciernaga:::CurrentData(x="%s", MainFolder=MainFolder, type = "MFI")
    Gain_%s <- Luciernaga:::CurrentData(x="%s", MainFolder=MainFolder, type = "Gain")
    ', fixed("%s"), name)
  Data <- append(Data, values = unlist(strsplit(Chunk1, "\n")), after = Matches[1] - 1)

  Pattern <- '#CurrentWindowPlaceholder'
  Matches <- which(Data == Pattern)
  Chunk2 <- str_replace_all('
  MFI_%s <- MFI_%s |> filter(DateTime >= WindowOfInterest)
  Gain_%s <- Gain_%s |> filter(DateTime >= WindowOfInterest)
  ', fixed("%s"), name)
  Data <- append(Data, values = unlist(strsplit(Chunk2, "\n")), after = Matches[1] - 1)
  
  Pattern <- '#MaintenancePlaceholder'
  Matches <- which(Data == Pattern)
  Chunk3 <- str_replace_all('
  Repair%s <- Data |> filter(instrument %in% "%s")
  ', fixed("%s"), name)
  Data <- append(Data, values = unlist(strsplit(Chunk3, "\n")), after = Matches[1] - 1)

  Pattern <- '#VisualQCPlaceholder'
  Matches <- which(Data == Pattern)
  Chunk4 <- str_replace_all('
  The%s <- Luciernaga:::VisualQCSummary(x=Gain_%s)
  ', fixed("%s"), name)
  Data <- append(Data, values = unlist(strsplit(Chunk4, "\n")), after = Matches[1] - 1)

  Pattern <- '#SmallTablePlaceholder'
  Matches <- which(Data == Pattern)
  Chunk5 <- str_replace_all('
  Table%s <- Luciernaga:::SmallTable(data=The%s)
  ', fixed("%s"), name)
  Data <- append(Data, values = unlist(strsplit(Chunk5, "\n")), after = Matches[1] - 1)

  Pattern <- '#HistPlaceholder1'
  Matches <- which(Data == Pattern)
  Chunk6 <- str_replace_all('
  Data%s <- Luciernaga:::ShinyQCSummary(x=Gain_%s, Instrument="%s")
  ', fixed("%s"), name)
  Data <- append(Data, values = unlist(strsplit(Chunk6, "\n")), after = Matches[1] - 1)
  
  # Adding HistoricalData
  Pattern <- '#HistPlaceholder2'
  Matches <- which(Data == Pattern)
  if (length(Matches) == 1){
    TheReplacement <- paste0("HistoricalData <- rbind(\"", name, "\")")
    Data[Matches] <- str_replace(Data[Matches], fixed("#HistPlaceholder2"), TheReplacement)
  } else {
    Pattern <- 'HistoricalData <- rbind('
    Matches <- which(str_starts(Data, fixed(Pattern)))
    ThisString <- Data[Matches]
    ThisString <- sub("\\)$", paste0(", \"", name, "\")"), ThisString)
    Data[Matches] <- ThisString
  }

  
#x <-  c("3L", "4L", "5L", "CS")
#y <- list(Gain_3L, Gain_4L, Gain_5L, Gain_CS)
  
Chunk7 <- str_replace_all('```{r}
TheStatus%s <- CurrentStatus(x="%s", data=Data) %>% InstrumentText(.)
TheColor%s <- CurrentStatus(x="%s", data=Data) %>% InstrumentColor(.)
```', fixed("%s"), name)
  
Chunk8 <- sprintf('

', name)


}