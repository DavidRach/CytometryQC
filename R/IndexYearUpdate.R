


#' Internal, implements a for-loop to update Historical index.qmd page for all the years
#' 
#' @param path File.path to index.qmd for respective instrument
#' @param years A vector of years to insert
#' @param instrument name of the instrument, for pathing of links
#' 
#' @importFrom stringr str_replace_all fixed
#' 
#' @return An updated index.qmd file
#' 
#' @noRd
IndexYearUpdate <- function(path, years, instrument){
  ThisInstrument <- instrument
  Data <- readLines(path)

  # for-loop here

  for (name in years){

  Pattern <- '### Standin1 {width=\"50%\"}'
  Matches <- which(Data == Pattern)
  Pattern2 <- '### Standin2 {width=\"50%\"}'
  Matches2 <- which(Data == Pattern2)
  
  if (length(Matches) == 1){
    Chunk1 <- str_replace_all('### Standin1 {width=\"50%\"}', fixed("Standin1"), name)
    Data[Matches] <- Chunk1

    Pattern <- '#| title: \"Standin1\"'
    Matches <- which(Data == Pattern)
    Chunk2 <- str_replace_all('#| title: \"Standin1\"', fixed("Standin1"), name)
    Data[Matches] <- Chunk2

    Pattern <- '# HTMLStandin1'
    Matches <- which(Data == Pattern)

    TheHTML <- "HTML(\"
<ul>
  <li><a href='YearPlaceholder.html'>Interactive</a></li>
  <li><a href='data/Historical_THISINSTRUMENT_Placeholder.pdf'>Plots</a></li>
  <li><a href='data/Historical_THISINSTRUMENT_Placeholder.csv'>Gain, RCV and MFI</a></li>
</ul>
\")" 
    Chunk2 <- str_replace_all(TheHTML, fixed("Placeholder"), name)
    Chunk2 <- str_replace_all(Chunk2, fixed("THISINSTRUMENT"), ThisInstrument)
    ChunkLines <- unlist(strsplit(Chunk2, "\n"))  

    Data <- c(Data[1:(Matches[1] - 1)], ChunkLines, Data[(Matches[1] + 1):length(Data)])  

  } else if (length(Matches2) == 1){

    Chunk1 <- str_replace_all('### Standin2 {width=\"50%\"}', fixed("Standin2"), name)
    Data[Matches2] <- Chunk1

    Pattern <- '#| title: \"Standin2\"'
    Matches <- which(Data == Pattern)
    Chunk2 <- str_replace_all('#| title: \"Standin2\"', fixed("Standin2"), name)
    Data[Matches] <- Chunk2

    Pattern <- '#HTMLStanding2'
    Matches <- which(Data == Pattern)

    TheHTML <- "HTML(\"
<ul>
  <li><a href='YearPlaceholder.html'>Interactive</a></li>
  <li><a href='data/Historical_THISINSTRUMENT_Placeholder.pdf'>Plots</a></li>
  <li><a href='data/Historical_THISINSTRUMENT_Placeholder.csv'>Gain, RCV and MFI</a></li>
</ul>
\")" 
    Chunk2 <- str_replace_all(TheHTML, fixed("Placeholder"), name)
    Chunk2 <- str_replace_all(Chunk2, fixed("THISINSTRUMENT"), ThisInstrument)
    ChunkLines <- unlist(strsplit(Chunk2, "\n"))  

    Data <- c(Data[1:(Matches[1] - 1)], ChunkLines, Data[(Matches[1] + 1):length(Data)]) 

  } else {

    Section3 <- '
### Standin1 {width=\"50%\"}

::: {.card title="" width="33%"}

:::

```{r}
#| content: valuebox
#| title: "Standin1"
#| icon: cup-hot

# HTMLStandin1
```
'

Section4 <- '
### Standin2 {width="50%"}

```{r}
#| content: valuebox
#| title: "Standin2"
#| icon: cup-hot

#HTMLStanding2
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
    
Pattern <- '### Standin1 {width=\"50%\"}'
Matches <- which(Data == Pattern)
    
if (length(Matches) == 1){

  Chunk1 <- str_replace_all('### Standin1 {width=\"50%\"}', fixed("Standin1"), name)
    Data[Matches] <- Chunk1

    Pattern <- '#| title: \"Standin1\"'
    Matches <- which(Data == Pattern)
    Chunk2 <- str_replace_all('#| title: \"Standin1\"', fixed("Standin1"), name)
    Data[Matches] <- Chunk2

    Pattern <- '# HTMLStandin1'
    Matches <- which(Data == Pattern)

    TheHTML <- "HTML(\"
<ul>
  <li><a href='YearPlaceholder.html'>Interactive</a></li>
  <li><a href='data/Historical_THISINSTRUMENT_Placeholder.pdf'>Plots</a></li>
  <li><a href='data/Historical_THISINSTRUMENT_Placeholder.csv'>Gain, RCV and MFI</a></li>
</ul>
\")" 
    Chunk2 <- str_replace_all(TheHTML, fixed("Placeholder"), name)
    Chunk2 <- str_replace_all(Chunk2, fixed("THISINSTRUMENT"), ThisInstrument)
    ChunkLines <- unlist(strsplit(Chunk2, "\n"))  

    Data <- c(Data[1:(Matches[1] - 1)], ChunkLines, Data[(Matches[1] + 1):length(Data)])  
}

    }
  }

  writeLines(Data, path)
}