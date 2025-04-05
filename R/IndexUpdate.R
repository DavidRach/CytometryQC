#' Triggered by AddInstrument, adds index.qmd chunks for that instrument.
#' 
#' @importFrom stringr str_replace_all
#' 
IndexUpdate <- function(){

# TheList <- c("Placeholder")

Chunk1 <- str_replace_all('```{r}
MFI_%s <- Luciernaga:::CurrentData(x="%s", MainFolder=MainFolder, type = "MFI")
Gain_%s <- Luciernaga:::CurrentData(x="%s", MainFolder=MainFolder, type = "Gain")
```', fixed("%s"), name)
  
Chunk2 <- str_replace_all('```{r}
MFI_%s <- MFI_%s |> filter(DateTime >= WindowOfInterest)
Gain_%s <- Gain_%s |> filter(DateTime >= WindowOfInterest)
```', fixed("%s"), name)
  
Chunk3 <- str_replace_all('```{r}
Repair%s <- Data |> filter(instrument %in% "%s")
```', fixed("%s"), name)
  
Chunk4 <- str_replace_all('```{r}
The%s <- Luciernaga:::VisualQCSummary(x=Gain_%s)
```', fixed("%s"), name)
  
Chunk5 <- str_replace_all('```{r}
Table%s <- Luciernaga:::SmallTable(data=The%s)
```', fixed("%s"), name)
  
Chunk6 <- str_replace_all('```{r}
Data%s <- Luciernaga:::ShinyQCSummary(x=Gain_%s, Instrument="%s")
```', fixed("%s"), name)
  
# HistoricalData <- rbind(Data3L, Data4L, Data5L, DataCS)
  
#x <-  c("3L", "4L", "5L", "CS")
#y <- list(Gain_3L, Gain_4L, Gain_5L, Gain_CS)
  
Chunk7 <- str_replace_all('```{r}
TheStatus%s <- CurrentStatus(x="%s", data=Data) %>% InstrumentText(.)
TheColor%s <- CurrentStatus(x="%s", data=Data) %>% InstrumentColor(.)
```', fixed("%s"), name)
  
Chunk8 <- sprintf('

', name)


}