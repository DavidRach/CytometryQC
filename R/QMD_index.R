

#' Creates generic index.qmd file
#'
#' @param outpath Location to save the file, default is InstrumentQC folder 
#' @param organization_name Name of the organization, ex. UMGCC FCSS
#' @param organization_website The organizations website
#' 
#' @return A index.qmd placeholder
#' 
#' @noRd
QMD_index <- function(outpath, organization_name, organization_website){
  StorageLocation <- file.path(outpath, "index.qmd")

Chunk1 <- '---
format:
  dashboard:
    orientation: columns
aliases: 
  - home.html
project:
  output-dir: docs/
---

```{r}
#| message: FALSE
library(dplyr)
library(purrr)
library(stringr)
library(plotly)
library(Luciernaga)

Computer <- getwd()
MainFolder <- file.path(Computer, "data")
TheList <- c("Placeholder")

```


```{r}

#MFIPlaceholder
```


'
  
Chunk2 <- '
```{r}
WindowOfInterest <- Sys.time() - months(12)

#CurrentWindowPlaceholder
```


```{r}
Data <- read.csv("Maintenance.csv", check.names=FALSE)

#MaintenancePlaceholder
```


```{r}

#VisualQCPlaceholder
```


```{r}

#SmallTablePlaceholder
```

'

Chunk3 <- '
```{r}
#| include: false
#| echo: false

Computer <- getwd()
MainFolder <- file.path(Computer, "data")
TheName <- "HistoricalData.csv"
HistoricalPath <- file.path(MainFolder, TheName)
ShinyData <- read.csv(HistoricalPath, check.names = FALSE)

#HistoricalDataPlaceholder
```

'
  
Chunk4 <- '
```{r}
# Global Summary Placeholder1

DataForPlot <- Luciernaga:::QCHistoryArchive(x=x, historydata=ShinyData, timewindow=24)
Transposed <- t(DataForPlot)
colnames(Transposed) <- Transposed [1,]
Transposed <- Transposed[-1,]
DataForPlot1 <- data.frame(Transposed, check.names=FALSE)
DataForPlot1 <- DataForPlot1 %>% tibble::rownames_to_column(., var="Date")
DataForPlot1$Date <- as.Date(DataForPlot1$Date) 
Data <- DataForPlot1 |> arrange(desc(Date))

# Global Summary Placeholder3

LastColumn <- ncol(Data)

Data[2:LastColumn] <- Data[2:LastColumn] |> mutate(across(everything(), ~ na_if(., "Unknown")))

AltData <- Data #|> dplyr::filter(Date > lubridate::ymd("2023-04-10"))

GlobalSummary <- Luciernaga:::SmallTableGlobal(Data)
```


```{r}
TheDate <- Data |> slice(1) |> pull(Date)
```


```{r}

#ColorStatusPlaceholder
```

'
  
Chunk5 <- '
## {.sidebar}
Dashboard data last updated on **`r TheDate`**

**Definitions:**

**Pass:** All gains within 100% baseline and all RCVs <6% for all detectors.

**Caution:** All gains within 100% baseline, but at least one detector had a RCV above the >6% cutoff. Instrument remains usable but resolution for fluorophores on the failed detector may decrease. 

**Fail:** Either a gain exceeded 100% baseline, or RCVs exceeded >6% for at least one indicator detector. Significant variation and batch effects may occcur. 

For additional information, navigate to the [Help](help.qmd) page.
'
  
Chunk6 <- sprintf('**About**

This dashboard contains the visualized QC data for the cytometers at [%s](%s)

This dashboard was created with [Quarto](https://quarto.org/) using the [CytometryQC](https://github.com/DavidRach/CytometryQC) R package.
', organization_name, organization_website)
  
 Chunk7 <- '
## First {width="30%"}

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

## Second {.tabset}

```{r}
#| echo: false
#| include: false
library(shinylive)
```

#### History

'
  
  Chunk8 <- '
```{shinylive-r}
#| standalone: true
#| viewerHeight: 450

webr::install("dplyr")
webr::install("gt")

library(shiny)
library(dplyr)
library(gt)

ui <- fluidPage(
  fluidRow(
    column(6,
           fluidRow(
             column(12, align = "center",
                    dateInput("date", label = "Select Date:", value = Sys.Date())
             )
           ),
           fluidRow(
             column(12, align = "center", #testing
                    actionButton("btn_Placeholder", label = "Placeholder")
             )
           ),
           fluidRow(
             column(12, align = "center",
                    actionButton("render", label = "Render Output")
             )
           ),
           # Output Section
           fluidRow(
             column(12,
                    tableOutput("qc_table")
             )
           )
    )
  )
)

server <- function(input, output, session) {
  
  data_path <-                                                
    paste(                                                  
      "https://raw.githubusercontent.com",                  
      "PlaceHolder", "InstrumentQC",                   
      "main", "data", "HistoricalData.csv",                              
      sep = "/"                                             
    ) 
  
  Data <- read.csv(data_path, check.names = FALSE)
  Data$Date <- as.Date(Data$Date) 
  
  function_path <-                                               
    paste(                                                  
      "https://raw.githubusercontent.com",                  
      "DavidRach", "Luciernaga",                   
      "master", "R", "DashboardHelpers.R",                              
      sep = "/"                                             
    )
  source(function_path)
  
  selected_instrument <- reactiveVal()
  
  #observeEventPlaceholder

  table_data <- eventReactive(input$render, {
    req(input$date, selected_instrument())
    
    InstrumentSubset <- Data |> filter(Instrument == selected_instrument())
    DateSubset <- InstrumentSubset |> filter(Date == input$date)
    
    if (nrow(DateSubset) > 0) {
      TableData <- DateSubset |> select(-Instrument, -Date)
      SmallTable(data = TableData)
    } else {
      NULL
    }
  })

  output$qc_table <- render_gt({
    req(table_data())
    table_data()
  })
}

app <- shinyApp(ui = ui, server = server)
```
  

'
  
Chunk9 <- '
## Third {.tabset}{width="40%"}


```{r}
#| title: Instruments
GlobalSummary
```

'
  
  cat(Chunk1, Chunk2, Chunk3, Chunk4, Chunk5, Chunk6,
    Chunk7, Chunk8, Chunk9, file = StorageLocation)
}
