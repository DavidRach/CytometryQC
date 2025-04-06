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
  Chunk1 <- str_replace_all('MFI_%s <- Luciernaga:::CurrentData(x="%s", MainFolder=MainFolder, type = "MFI")
  Gain_%s <- Luciernaga:::CurrentData(x="%s", MainFolder=MainFolder, type = "Gain")
  ', fixed("%s"), name)
  Data <- append(Data, values = unlist(strsplit(Chunk1, "\n")), after = Matches[1] - 1)

  Pattern <- '#CurrentWindowPlaceholder'
  Matches <- which(Data == Pattern)
  Chunk2 <- str_replace_all('MFI_%s <- MFI_%s |> filter(DateTime >= WindowOfInterest)
  Gain_%s <- Gain_%s |> filter(DateTime >= WindowOfInterest)
  ', fixed("%s"), name)
  Data <- append(Data, values = unlist(strsplit(Chunk2, "\n")), after = Matches[1] - 1)
  
  Pattern <- '#MaintenancePlaceholder'
  Matches <- which(Data == Pattern)
  Chunk3 <- str_replace_all('Repair%s <- Data |> filter(instrument %in% "%s")
  ', fixed("%s"), name)
  Data <- append(Data, values = unlist(strsplit(Chunk3, "\n")), after = Matches[1] - 1)

  Pattern <- '#VisualQCPlaceholder'
  Matches <- which(Data == Pattern)
  Chunk4 <- str_replace_all('The%s <- Luciernaga:::VisualQCSummary(x=Gain_%s)
  ', fixed("%s"), name)
  Data <- append(Data, values = unlist(strsplit(Chunk4, "\n")), after = Matches[1] - 1)

  Pattern <- '#SmallTablePlaceholder'
  Matches <- which(Data == Pattern)
  Chunk5 <- str_replace_all('Table%s <- Luciernaga:::SmallTable(data=The%s)
  ', fixed("%s"), name)
  Data <- append(Data, values = unlist(strsplit(Chunk5, "\n")), after = Matches[1] - 1)

  Pattern <- '#HistPlaceholder1'
  Matches <- which(Data == Pattern)
  Chunk6 <- str_replace_all('Data%s <- Luciernaga:::ShinyQCSummary(x=Gain_%s, Instrument="%s")
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

  # Replacing Global Summary Placeholder1
  Pattern <- '# Global Summary Placeholder1'
  Matches <- which(Data == Pattern)
  if (length(Matches) == 1){
    TheReplacement <- paste0("x <- c(\"", name, "\")")
    Data[Matches] <- str_replace(Data[Matches],
      fixed("# Global Summary Placeholder1"), TheReplacement)
  } else {
    Pattern <- 'x <- c('
    Matches <- which(str_starts(Data, fixed(Pattern)))
    ThisString <- Data[Matches]
    ThisString <- sub("\\)$", paste0(", \"", name, "\")"), ThisString)
    Data[Matches] <- ThisString
  }

  # Replacing Global Summary Placeholder2
  Pattern <- '# Global Summary Placeholder2'
  Matches <- which(Data == Pattern)
  if (length(Matches) == 1){
    TheReplacement <- paste0("y <- list(\"", name, "\")")
    Data[Matches] <- str_replace(Data[Matches],
      fixed("# Global Summary Placeholder2"), TheReplacement)
  } else {
    Pattern <- 'y <- list('
    Matches <- which(str_starts(Data, fixed(Pattern)))
    ThisString <- Data[Matches]
    ThisString <- sub("\\)$", paste0(", \"", name, "\")"), ThisString)
    Data[Matches] <- ThisString
  }

  # Replacing Global Summary Placeholder3
  Pattern <- '# Global Summary Placeholder3'
  Matches <- which(Data == Pattern)
  if (length(Matches) == 1){
    TheReplacement <- paste0('Data <- Data |> relocate("', name, '", .after="Date")')
    Data[Matches] <- str_replace(Data[Matches],
      fixed("# Global Summary Placeholder3"), TheReplacement)
  } else {
    Pattern <- 'Data <- Data |> relocate('
    Matches <- which(str_starts(Data, fixed(Pattern)))
    ThisString <- Data[Matches]
    ThisString <- sub("\\.after", paste0("\"", name, "\", .after"), ThisString)
    Data[Matches] <- ThisString
  }

  Pattern <- '#ColorStatusPlaceholder'
  Matches <- which(Data == Pattern)
  Chunk7 <- str_replace_all('TheStatus%s <- CurrentStatus(x="%s", data=Data) %>% InstrumentText(.)
  TheColor%s <- CurrentStatus(x="%s", data=Data) %>% InstrumentColor(.)
  ', fixed("%s"), name)
  Data <- append(Data, values = unlist(strsplit(Chunk7, "\n")), after = Matches[1] - 1)

  Data
  Pattern1 <- '#| title: \"PlaceHolder1\"'
  Pattern2 <- '#| title: \"PlaceHolder2\"'
  Matches1 <- which(Data == Pattern1)
  Matches2 <- which(Data == Pattern2)
  if (length(Matches1) == 1){
    Pattern <- "#| title: \"PlaceHolder1\""
    Matches <- which(Data == Pattern)
    Chunk8 <- str_replace_all("#| title: \"PlaceHolder1\"", fixed("PlaceHolder1"), name)
    Data[Matches] <- Chunk8

    Pattern <- "#PlaceHolder1"
    Matches <- which(Data == Pattern)
    Chunk9 <- str_replace_all('list(value = paste0("QC Status: ", TheStatusPlaceholder), color = TheColorPlaceholder)',
     fixed("Placeholder"), name)
    Data[Matches] <- Chunk9
  } else if (length(Matches2) == 1){
    Pattern <- "#| title: \"PlaceHolder2\""
    Matches <- which(Data == Pattern)
    Chunk8 <- str_replace_all("#| title: \"PlaceHolder2\"", fixed("PlaceHolder2"), name)
    Data[Matches] <- Chunk8

    Pattern <- "#PlaceHolder2"
    Matches <- which(Data == Pattern)
    Chunk9 <- str_replace_all('list(value = paste0("QC Status: ", TheStatusPlaceholder), color = TheColorPlaceholder)',
     fixed("Placeholder"), name)
    Data[Matches] <- Chunk9
  } else {

    ChunkReplacement <- '
### Row {height="50%"}

#### Column

```{r}
#| content: valuebox
#| title: "PlaceHolder1"
#| icon: cup-hot

#PlaceHolder1

```

#### Column

```{r}
#| content: valuebox
#| title: "PlaceHolder2"
#| icon: cup-hot

#PlaceHolder2

```

'
  Pattern <- '## Second {.tabset}'
  Matches <- which(Data == Pattern)  
  Data <- append(Data, values = unlist(strsplit(ChunkReplacement, "\n")), after = Matches[1] - 1)
  Pattern1 <- '#| title: \"PlaceHolder1\"'
  Matches1 <- which(Data == Pattern1)

  if (length(Matches1) == 1){
    Pattern <- "#| title: \"PlaceHolder1\""
    Matches <- which(Data == Pattern)
    Chunk8 <- str_replace_all("#| title: \"PlaceHolder1\"", fixed("PlaceHolder1"), name)
    Data[Matches] <- Chunk8

    Pattern <- "#PlaceHolder1"
    Matches <- which(Data == Pattern)
    Chunk9 <- str_replace_all('list(value = paste0("QC Status: ", TheStatusPlaceholder), color = TheColorPlaceholder)',
     fixed("Placeholder"), name)
    Data[Matches] <- Chunk9
    }
  }

  Pattern <- '## Second {.tabset}'
  Matches <- which(Data == Pattern)
  Chunk10 <- str_replace_all('```{r}
  #| title: Placeholder
  TablePlaceholder
  ```

  ', fixed("Placeholder"), name)
  Data <- append(Data, values = unlist(strsplit(Chunk10, "\n")), after = Matches[1])

  
  
  Pattern <- '             column(12, align = "center", #testing'
  Chunk11 <- '                    actionButton("btn_Placeholder", label = "Placeholder")'


  Data <- gsub("^\\s?```\\s*\\{r\\}", "```{r}", Data)
  
  writeLines(Data, Index)
}