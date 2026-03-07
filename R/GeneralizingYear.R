#' Internal
#' 
#' @param InstrumentFolder Location of the Historical Archive Repo for given instrument
#' @param githubusername User name all lower case no spaces
#' 
#' @importFrom stringr str_detect
#' 
#' @return Updated Year.qmd file ready for use. 
#' 
#' @noRd
GeneralizingYear <- function(InstrumentFolder, githubusername){
  Instrument <- basename(InstrumentFolder)
  #githubusername <- "UMGCCFCSS"

  Year <- list.files(InstrumentFolder, pattern="Year.qmd", full.names=TRUE)
  if (!length(Year) == 1){stop("No Year File Found")}
  Data <- readLines(Year)
  Pattern <- 'x <- MFI'
  Matches <- which(str_detect(Data, Pattern))-3
  Data <- Data[-(7:Matches)]

  String1 <- sprintf('
```{r}
#| message: FALSE
library(dplyr)
library(purrr)
library(stringr)
library(plotly)
library(Luciernaga)
library(lubridate)

CSV <- data_path <-                                                
    paste(                                                  
      "https://raw.githubusercontent.com",                  
      "%s", "InstrumentQC",                   
      "main", "data", "Hmm", "Archive", "BeadDataHmm.csv",                              
      sep = "/"                                             
    ) 

TheList <- c("Hmm")

Data <- read.csv(CSV, check.names=FALSE)
Data$DateTime <- lubridate::ymd_hms(Data$DateTime)
#tail(Data, 1)

QC_Cutoffs <- data_path <-                                                
    paste(                                                  
      "https://raw.githubusercontent.com",                  
      "%s", "InstrumentQC",                   
      "main", "data", "QC_Hmm.CSV",                              
      sep = "/"                                             
    )

QC_Cutoffs <- readLines(QC_Cutoffs)
```

', githubusername, githubusername)
  
String1 <- gsub("Hmm", Instrument, String1)
  
String2 <- '
```{r}
MaintenancePath <- data_path <-                                                
    paste(                                                  
      "https://raw.githubusercontent.com",                  
      "THISONE", "InstrumentQC",                   
      "main", "Maintenance.csv",                              
      sep = "/"                                             
    )
Maintenance <- read.csv(MaintenancePath, check.names=FALSE)
Maintenance <- Maintenance |> filter(!str_detect(reason, "lean"))
Maintenance <- Maintenance |> filter(instrument %in% TheList)
```

```{r}
TheTHISYEAR <- Data |> dplyr::filter(year(DateTime) == THISYEAR)

TheTHISYEAR <- HolisticToArchived(data=TheTHISYEAR, manufacturer="Cytek",
 baselinecutoffs=QC_Cutoffs, gainmultiplier = 2)
```

'
  
String2 <- gsub("THISONE", githubusername, String2)
  
String1 <- unlist(strsplit(String1, "\n"))
String2 <- unlist(strsplit(String2, "\n"))  

Pattern <- '```'
Matches <- which(str_detect(Data, Pattern))
UpTillHere <- tail(Matches, 1)+1
  
NewData <- c(Data[1:7], String1, String2, Data[8:UpTillHere])
writeLines(NewData, Year)
}
