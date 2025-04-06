#' Creates the 404.qmd file
#'
#' @param outpath The file.location to save the .qmd file to
#' 
#' @return A 404.qmd file
#' 
#' @export
QMD_404 <- function(outpath, organization="UMGCC FCSS",
 github_page="umgccfcss.github.io"){
  
StorageLocation <- file.path(outpath, "404.qmd")
Homepage <- paste0("https://", github_page, "/InstrumentQC")
  
content <- sprintf(
'---
title: "Page not found"
format: html
---

Sorry, the page you are looking for is no longer there.

To access the %s  Instrument QC dashboard, please click here to go to the [homepage](%s)
', organization, Homepage)

cat(content, file = StorageLocation)  
}

#' Creates generic Data.qmd file
#'
#' @param outpath The file.location to save the .qmd file to
#' 
#' @return A generic Data.qmd file
#' 
#' @export
QMD_Data <- function(outpath, organization="UMGCC FCSS",
github_page="umgccfcss.github.io"){
  StorageLocation <- file.path(outpath, "Data.qmd")

  content <- '---
format:
  dashboard:
    orientation: rows
project:
  output-dir: docs/
---

```{r}
library(htmltools)
```

```{r}
TheDate <- Sys.Date()
```

## {.sidebar}
Dashboard data last updated on **`r TheDate`**

**Contents:**

**Gain and RCV** A .csv file containing the Daily QC data used for the Gain and RCV plots. 

**MFI** A .csv file containing Gain and MFI information derrived from before and after Daily QC .fcs files. 

**Gain and MFI** A .csv file containing Gain and MFI information derrived from Daily QC .fcs files.

**Plots** A .pdf file containing non-interactive version of all the plots for each instrument.

For additional information, navigate to the [Help](help.qmd) page.

**About**
'

Section2 <- sprintf('
This dashboard contains the visualized QC data for the cytometers at the [%s](%s)

This dashboard was created with [Quarto](https://quarto.org/) and was created with [CytometryQC](https://github.com/DavidRach/CytometryQC)
', organization, github_page)
  
Section3 <- '## First Row {height="50%"}
### Standin {width="50%"}

::: {.card title="" width="33%"}

:::

```{r}
#| content: valuebox
#| title: "Standin1"
#| icon: cup-hot

#HTML_Standin1
```

## Second Row {height="50%"}

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
    
  cat(content, Section2, Section3, file = StorageLocation)
}

#' Creates the help.qmd file
#' 
#' @param outpath The file.location to save the .qmd file to
#' 
#' @return A help.qmd file 
#' 
#' @export
QMD_help <- function(outpath) {
  StorageLocation <- file.path(outpath, "help.qmd")
  
  content <- '
---
toc: true
---

# Measurement Types

## MFI

As part of daily QC, spectral flow cytometers use QC beads to adjust the gains for individual detectors to ensure the Median Fluorescent Intensity (MFI) of the beads matches lot-specific thresholds. This allows for .fcs files acquired on different days to be comparable, reducing instrumental batch effects.
To monitor these adjustments, we can retrieve the MFI values of the QC bead directly from their acquisition .fcs files. We normally observe something like this:

 ![](images/MFI_B3.png){fig-align="center"}

 In this plot, you can notice when the QC bead lots were switched on the instrument. The new QC bead lot had different MFI setpoint for each detector, which the instrument accounts for.

 On some instruments and detectors, by monitoring MFI we can observe extensive drift from the setpoint for the before Daily QC samples. 

 ![](images/MFI_R1.png){fig-align="center"}

 This is why we encourage weekend users to make sure DailyQC is run before acquiring their samples. 

## Gain

Gain (similarly voltage on certain cytometers) is an unit by which the detected signal is amplified by for each detector. An instrument applies different gains to each detector, which allows for the use of both dim and bright fluorophores.
During the Daily QC, minor adjustments to the gains occur to ensure the MFI of the QC beads remains stays constant, allowing comparison .fcs files acquired on different days.
As individual lasers in the cytometer get used, they slowly wear down, resulting in more gain needing to be applied to reach the MFI setpoint. This leads to the slowly increasing trend visible in some gain plots:

 ![](images/Gain_UV3.png){fig-align="center"}

When a laser beings to rapidly wear out, we see steeper increases in the Gain plots. When a field service engineer replaces the laser with a new one, the gain values are reset.
You can see the violet laser of an instrument was replaced twice in the following figure (red dashed lines indicating field service engineer visit):

 ![](images/Gain_V1.png){fig-align="center"}

For Cytek instruments, a Daily QC fail is triggered when the Gain is >100% of the original baseline value for its detector. When this occurs, a red flag is added to that timepoint.
For .fcs files acquired on these "gain spike" days, unmixing issues may occur even if the values did not exceed the >100% threshold, so it is worth monitoring. 

 ![](images/Gain_R8.png){fig-align="center"}

## %rCV

The robust coefficient of variation (rCV) is a measure of resolution for each detector, with the value shown as a percent. When this value is low, QC beads have similar MFI values (low spread). When the value is high, QC beads have more spread in their MFI values for a given detector. 
This spread can be due to a laser coming out of alignment, debris in the flow cell, or damage to the QC bead. While diagnosing which is contributing, the increased %rCV can make resolving dim staining populations from the negative population more challenging. 
For large spectral panels (>20 colors) where entire spectrum is used to identify a fluorophore, increased %rCV on the detector where a panel fluorophore has its main fluorescence peak can contribute to unmixing issues.
The %rCV plots will typically resemble the following:

 ![](images/RCV_UV3.png){fig-align="center"}

For Cytek instruments, a Daily QC fail is only triggered when %rCV exceeds either 8% for SSC, or 6% for FSC and the following detectors: UV3, V3, B3, YG3, R3.
When this happens, the instrument will appear as failed on the home page:

 ![](images/InstrumentFail.png){fig-align="center"}

 When we navigate to the History tab and find that date, we can see that the %rCV for UV3 failed, which was what triggered the QC fail:

 ![](images/HistoryView.png){fig-align="center"} 

What if the %RCV fails on any of the other detectors (UV1-> UV2, UV4 -> UV16,etc.)?
Since they are not the indicator detectors, they wont trigger a Cytek QC fail.
However, we have observed unmixing issues on days when these detectors %RCV were high, so we dont ignore them. 
When a non-indicator %RCV fails, we 1) show it as failed under the Daily View:

 ![](images/InstrumentCautionDaily.png){fig-align="center"} 

And give the instrument the "caution" status for that given day:

 ![](images/InstrumentCaution.png){fig-align="center"} 

Interpreting these "Caution" status we need to check the Levey-Jennings plots. For a few detectors (especially UV1/UV2), they often exceed the 6% cutoff but are stable over time. The impact of this increased %RCV across acquisition days is therefore relatively minimal.

 ![](images/RCV_UV1Stable.png){fig-align="center"} 

 By contrast, if the %RCV went from consistently stable, to spiking, the impact on your unmixing is likely to be significantly greater.

 ![](images/RCV_UV16Issue.png){fig-align="center"} 

# Interactive Levey-Jennings Plots

For each spectral instrument, the interactive plots are laid out in three columns (MFI, Gain, %rCV), with tabs for each of the lasers on that instrument. Additional parameters (Scatter, LaserPower, LaserDelay, LaserAreaScaling) may appear if they are recorded by the instrument.  

When a QC fail occurs for a given parameter, it appears as a red box on that individual date.

 ![](images/RCV_UV16Issue.png){fig-align="center"} 

By default, the interactive plots display a year of QC data for that instrument when available.  You can hover over the upper right of the plot to access additional options to select, scroll, zoom in and save an individual plot as a .png on your computer.

 ![](images/Options.png){fig-align="center"} 

# Data

All data that was derived for use in the interactive dashboards for the individual instuments is available for download under the Data tab on the upper right of the page. 

 ![](images/DataView.png){fig-align="center"} 

Gain and %rCV links to .csv file containing those types of data. 

MFI links to .csv file containing the data derrived from the before and after QC Bead .fcs files. 

Both the above can be used if you want to plot the data yourself using a different ggplot2 color-scheme in R than the one we have selected. Additionally, data for timepoints older than the current year can be found here. 

Plots is a .pdf containing static versions of all the interactive plots (MFI, Gain, %rCV) that are visible for the Levey-Jennings Plots for the individual instrument. QC fails appear as red squares, and vertical red dashed lines correspond to dates the field service engineer was on site for repairs or preventative maintenance.

 ![](images/RCV_V3.png){fig-align="center"} 
'
  
  writeLines(content, StorageLocation)
}

#' Creates generic Historical.qmd file
#'
#' @param outpath The file.location to save the .qmd file to
#' 
#' @return A generic Historical.qmd file
#' 
#' @export
QMD_Historical <- function(outpath){
  
  StorageLocation <- file.path(outpath, "Historical.qmd")

  content <- '---
format:
  dashboard:
    orientation: columns
    scrolling: true
---

This is a placeholder
'
  cat(content, file = StorageLocation)
}

#' Creates generic HistoricalInstument.qmd file
#'
#' @param outpath The file.location to save the .qmd file to
#' 
#' @return A generic HistoricalInstument.qmd file
#' 
#' @export
QMD_HistoricalInstrument <- function(outpath, name, manufacturer,
   organization, organization_website){
  
  FullName <- paste0("Historical_", name, ".qmd")
  StorageLocation <- file.path(outpath, FullName)

  TheInstrument <- paste(manufacturer, name, sep=" ")

  Section1 <- sprintf('---
format:
  dashboard:
    orientation: rows
project:
  output-dir: docs/
---

```{r}
library(htmltools)
```

```{r}
TheDate <- Sys.Date()
```

## {.sidebar}
Dashboard contains historical data for the **%s**.

**Contents:**

**Interactive** A redirect to the interactive Levey-Jennings plots for the respective year.

**Plots** A .pdf file containing non-interactive version of all the plots for the respective year.

**Gain and MFI** A .csv file containing Gain and MFI information derrived from Daily QC .fcs files used to generate the plots.
', TheInstrument)
  
Section2 <- sprintf('For additional information, navigate to the [Help](help.qmd) page.

**About**

This dashboard contains the visualized QC data for the cytometers at [%s](%s)

This dashboard was created with [Quarto](https://quarto.org/) using [CytometryQC](https://github.com/DavidRach/CytometryQC)
', organization, organization_website)
  
Section3 <- '## First Row {height="50%"}

### Standin1 {width="50%"}

::: {.card title="" width="33%"}

:::

```{r}
#| content: valuebox
#| title: "Standin1"
#| icon: cup-hot

# HTMLStandin1
```
## Second Row {height="50%"}

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
  
cat(Section1, Section2, Section3, file = StorageLocation)
}

#' Creates generic index.qmd file
#'
#' @param outpath The file.location to save the .qmd file to
#' 
#' @return A generic index.qmd file
#' 
#' @export
QMD_index <- function(outpath, organization, organization_website){
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

# Updating Data
walk(.x=TheList, MainFolder=MainFolder, .f=Luciernaga:::DailyQCParse)
walk(.x=TheList, .f=Luciernaga:::QCBeadParse, MainFolder=MainFolder)
```


```{r}

#MFIPlaceholder
```


'
  
Chunk2 <- '```{r}
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

Chunk3 <- '```{r}
#| include: false
#| echo: false

#HistPlaceholder1

#HistPlaceholder2

Computer <- getwd()
MainFolder <- file.path(Computer, "data")
TheName <- "HistoricalData.csv"
HistoricalPath <- file.path(MainFolder, TheName)

write.csv(HistoricalData, HistoricalPath, row.names = FALSE)

#HistoricalDataPlaceholder
```

'
  
Chunk4 <- '```{r}
# Global Summary Placeholder1

# Global Summary Placeholder2

DataForPlot <- Luciernaga:::QCHistory(x=x, y=y)
Transposed <- t(DataForPlot)
colnames(Transposed) <- Transposed[1,]
Transposed <- Transposed[-1,]
DataForPlot1 <- data.frame(Transposed, check.names=FALSE)
DataForPlot1 <- DataForPlot1 %>% tibble::rownames_to_column(., var="Date")
DataForPlot1$Date <- as.Date(DataForPlot1$Date) 
Data <- DataForPlot1 |> arrange(desc(Date))

# Global Summary Placeholder3

GlobalSummary <- Luciernaga:::SmallTableGlobal(Data)
```


```{r}
TheDate <- Data |> slice(1) |> pull(Date)
```


```{r}

#ColorStatusPlaceholder
```

'
  
Chunk5 <- '## {.sidebar}
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

', organization, organization_website)
  
  cat(Chunk1, Chunk2, Chunk3, Chunk4, Chunk5, Chunk6, file = StorageLocation)
}

#' Creates generic Instrument.qmd file
#'
#' @param outpath The file.location to save the .qmd file to
#' 
#' @return A generic instrument.qmd file
#' 
#' @export
QMD_Instrument <- function(outpath){
  StorageLocation <- file.path(outpath, "Instrument.qmd")

  content <- '---
format:
  dashboard:
    orientation: columns
    scrolling: true
---

This is a placeholder
'
    
  cat(content, file = StorageLocation)
}

#' Creates generic Miscellaneous.qmd file
#'
#' @param outpath The file.location to save the .qmd file to
#' 
#' @return A generic Miscellaneous.qmd file
#' 
#' @export
QMD_Miscellaneous <- function(outpath){
  
  StorageLocation <- file.path(outpath, "Miscellaneous.qmd")

  content <- '---
project:
  output-dir: docs/
toc: true
---

This is a placeholder
'
  cat(content, file = StorageLocation)
}

#' Creates generic README.md file
#'
#' @param outpath The file.location to save the .qmd file to
#' 
#' @return A generic README.md file
#' 
#' @export
QMD_README <- function(outpath, organization, organization_website){
  
  StorageLocation <- file.path(outpath, "README.md")

  content <- sprintf('This repository contains the code for the InstrumentQC dashboard for the [%s](%s)cytometers. 

The dashboard track changes in MFI, Gain and %%RCV over time by processing .fcs files of QC beads acquired during QC in [R](https://www.r-project.org/) using the [Luciernaga](https://github.com/DavidRach/Luciernaga) package. 
The results are then turned into a website using [Quarto](https://quarto.org/) using functions found in the [CytometryQC](https://github.com/DavidRach/CytometryQC) package. All code is available under the AGPL3-0 copyleft license. Additional how-to-replicate-this-dashboard details can be found [here](https://github.com/DavidRach/InstrumentQC_Install)
', organization, organization_website)
  
  cat(content, file = StorageLocation)
}




#' Creates initial quarto yaml
#' 
#' @param outpath The file.location to save the .yaml file to
#' 
#' @return The .yaml file needed to generate the website
#' 
#' @export
QMD_yaml <- function(outpath, organization="UMGCC FCSS",
github_page="umgccfcss.github.io", institution="University of Maryland, Baltimore"){
  StorageLocation <- file.path(outpath, "_quarto.yml")
  
  content <- sprintf('project:
  type: website
  output-dir: docs/
website:
  announcement: 
    icon: info-circle
    dismissable: true
    content: "Please see left-sidebar for definitions"
    type: info
    position: below-navbar  
  title: "%s"
  site-path: "/InstrumentQC/"
  navbar:
    logo: images/hex.svg
    left:
    - text: "Home"
      href: index.qmd
    - text: "Levey-Jennings Plots"
      menu:
      - text: "Instrument"
        href: Instrument.qmd
    - text: "Historical"
      menu:
      - text: "Instrument"
        href: Historical.qmd
    right:
    - text: "Help"
      href: help.qmd
    - text: "Download Data"
      href: Data.qmd
    - text: "Other"
      menu:
      - text: "Miscellaneous"
        href: Miscellaneous.qmd 
    - icon: github
      href: %s
      aria-label: GitHub          
  page-footer:
    background: light
    left: %s
    right: Built with [Quarto](https://quarto.org/). How? [Find Out](https://davidrach.github.io/CytometryQC/){target=_blank}
format:
  html:
    theme: styles.scss
', organization, github_page, institution)
  
cat(content, file = StorageLocation)    
  
}
  