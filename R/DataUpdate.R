

#' Updated the Data.qmd file
#' 
#' @param outpath Location to store the file, default in InstrumentQC folder
#' @param name See \code{\link{AddInstruments}}
#' 
#' @importFrom stringr str_replace_all fixed
#' 
#' @return An updated Data.qmd file
#' 
#' @noRd
DataUpdate <- function(outpath, name){
  DataPath <- list.files(outpath, pattern="Data.qmd", full.names=TRUE)
  if (!length(DataPath) == 1){stop("Data.qmd file not present")}

  Data <- readLines(DataPath)

  Pattern <- '### Standin {width=\"50%\"}'
  Matches <- which(Data == Pattern)
  Pattern2 <- '### Standin2 {width=\"50%\"}'
  Matches2 <- which(Data == Pattern2)
  
  if (length(Matches) == 1){
    Chunk1 <- str_replace_all('### Standin {width=\"50%\"}', fixed("Standin"), name)
    Data[Matches] <- Chunk1

    Pattern <- '#| title: \"Standin1\"'
    Matches <- which(Data == Pattern)
    Chunk2 <- str_replace_all('#| title: \"Standin1\"', fixed("Standin1"), name)
    Data[Matches] <- Chunk2

    Pattern <- '#HTML_Standin1'
    Matches <- which(Data == Pattern)

    TheHTML <- "HTML(\"
<ul>
  <li><a href='data/Placeholder/Archive/TheCSVs.csv'>TheCSVs</a></li>
  <li><a href='data/QCPlots_Placeholder.pdf'>Plots</a></li>
</ul>
\")" 
    Chunk2 <- str_replace_all(TheHTML, fixed("Placeholder"), name)
    Data[Matches] <- Chunk2

  } else if (length(Matches2) == 1){

    Chunk1 <- str_replace_all('### Standin2 {width=\"50%\"}', fixed("Standin2"), name)
    Data[Matches2] <- Chunk1

    Pattern <- '#| title: \"Standin2\"'
    Matches <- which(Data == Pattern)
    Chunk2 <- str_replace_all('#| title: \"Standin2\"', fixed("Standin2"), name)
    Data[Matches] <- Chunk2

    Pattern <- '#HTML_Standin2'
    Matches <- which(Data == Pattern)

    TheHTML <- "HTML(\"
<ul>
  <li><a href='data/Placeholder/Archive/TheCSVs.csv'>TheCSVs</a></li>
  <li><a href='data/QCPlots_Placeholder.pdf'>Plots</a></li>
</ul>
\")" 
    Chunk2 <- str_replace_all(TheHTML, fixed("Placeholder"), name)
    Data[Matches] <- Chunk2

  } else {

    Section3 <- '
### Standin {width="50%"}

::: {.card title="" width="33%"}

:::

```{r}
#| content: valuebox
#| title: "Standin1"
#| icon: cup-hot

#HTML_Standin1
```
'

Section4 <- '
### Standin2 {width="50%"}

```{r}
#| content: valuebox
#| title: "Standin2"
#| icon: cup-hot

#HTML_Standin2
```


::: {.card title="" width="33%"}

:::
  
'

Pattern <- '## Second Row {height="50%"}'
Matches <- which(Data == Pattern)-1
Section3 <- strsplit(Section3, "\n")[[1]]

Pattern <- '::: {.card title="" width="33%"}'
Matches2 <- which(Data == Pattern)
ThisOne <- Matches2[length(Matches2)] +2   
Section4 <- strsplit(Section4, "\n")[[1]]   
    
Data <- c(Data[1:Matches], Section3, Data[(Matches+1):ThisOne], Section4)
    
Pattern <- '### Standin {width=\"50%\"}'
Matches <- which(Data == Pattern)
    
if (length(Matches) == 1){
  Chunk1 <- str_replace_all('### Standin {width=\"50%\"}', fixed("Standin"), name)
  Data[Matches] <- Chunk1

  Pattern <- '#| title: \"Standin1\"'
  Matches <- which(Data == Pattern)
  Chunk2 <- str_replace_all('#| title: \"Standin1\"', fixed("Standin1"), name)
  Data[Matches] <- Chunk2

  Pattern <- '#HTML_Standin1'
  Matches <- which(Data == Pattern)

  TheHTML <- "HTML(\"
<ul>
<li><a href='data/Placeholder/Archive/TheCSVs.csv'>TheCSVs</a></li>
<li><a href='data/QCPlots_Placeholder.pdf'>Plots</a></li>
</ul>
\")" 
  Chunk2 <- str_replace_all(TheHTML, fixed("Placeholder"), name)
  Data[Matches] <- Chunk2
}

  }

  writeLines(Data, DataPath)
}
