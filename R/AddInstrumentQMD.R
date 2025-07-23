#' Internal for AddInstruments, creates initial template for an instrument.qmd file
#' 
#' @param name See \code{\link{AddInstruments}}
#' @param manufacturer See \code{\link{AddInstruments}}
#' @param outpath Location to save file, default is the InstrumentQC folder
#' @param organization_name See \code{\link{AddInstruments}}
#' @param organization_website See \code{\link{AddInstruments}}
#' @param timepointType Whether QC .fcs files are "single" or "double" (ie, before and after)
#' @param references The reference data.frame of laser detectors for the instrument
#' 
#' @importFrom dplyr filter pull
#' 
#' @return The Instrument.qmd template to the designated location
#' 
#' @noRd
AddInstrumentQMD_Reorganized <- function(name, manufacturer, outpath, organization_name,
  organization_website, timepointType, references){
 
 # Input values
 filename <- paste0(name, ".qmd")
 PDFValue <- paste0("QCPlots_", name)
 InstrumentName <- paste0(manufacturer, " ", name)
 StorageLocation <- file.path(outpath, filename)

 # Quarto Document Start
 Section1 <- sprintf('---
format:
 dashboard:
   orientation: columns
   scrolling: true
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
TheList <- c("%s")

# Updating Data
CSTGates <- file.path(Computer, "Gates.csv")

walk(.x=TheList, .f=Luciernaga:::HolisticQCParse, MainFolder=MainFolder,
 Template=CSTGates, subsets="Staining")
```

```{r}
InstrumentData <- Luciernaga:::CurrentData(x="%s", MainFolder=MainFolder, type = "Both")
TheDate <- InstrumentData |> slice(1) |> pull(DATE)
```

```{r}
WindowOfInterest <- Sys.time() - months(12)

InstrumentData <- InstrumentData |> filter(DateTime >= WindowOfInterest)
```

```{r}
Data <- read.csv("Maintenance.csv", check.names=FALSE)

Data <- Data |> filter(!str_detect(reason, "lean"))

Repair <- Data |> filter(instrument %%in%% "%s")
```

', name, name, name)

SectionMFI <- '
```{r}
x <- InstrumentData
TheColumns <- x %>% select(where(~is.numeric(.)||is.integer(.))) %>% colnames()
TheColumns <- setdiff(TheColumns, "TIME")

# MFIs
TheIntermediate <- TheColumns[!str_detect(TheColumns, "Gain")]
TheIntermediate <- TheIntermediate[!str_detect(TheIntermediate, "rCV")]
TheColumnNames <- TheIntermediate[str_detect(TheIntermediate, "-A")]

UltraVioletGains <- TheColumnNames[str_detect(TheColumnNames, "^UV")]
VioletGains <- TheColumnNames[str_detect(TheColumnNames, "^V")]
BlueGains <- TheColumnNames[str_detect(TheColumnNames, "^B")]
YellowGreenGains <- TheColumnNames[str_detect(TheColumnNames, "^Y")]
RedGains <- TheColumnNames[str_detect(TheColumnNames, "^R")]

ScatterGains <- TheIntermediate[str_detect(TheIntermediate, "SC-")]
ScatterGains <- Luciernaga:::ScalePriority(ScatterGains)
LaserGains <- TheIntermediate[str_detect(TheIntermediate, "Laser")]
LaserGains <- Luciernaga:::ColorPriority(LaserGains)
ScalingGains <- TheIntermediate[str_detect(TheIntermediate, "Scaling")]
ScalingGains <- Luciernaga:::ColorPriority(ScalingGains)
OtherGains <- c(ScatterGains, LaserGains, ScalingGains)
'

TheseLasers <- references |> dplyr::filter(Detector > 0) |> dplyr::pull(Laser)
  
SectionMFI_2 <- '

ScatterPlotsMFI <- QC_Plots(x=x, FailedFlag=FALSE, MeasurementType=ScatterGains,
                    plotType = "comparison", returntype = "plots",
                    Metadata="Timepoint", strict = TRUE, YAxisLabel = " ",
                    RepairVisits=Repair)

LaserPlotsMFI <- QC_Plots(x=x, FailedFlag=FALSE, MeasurementType=LaserGains,
                    plotType = "comparison", returntype = "plots",
                    Metadata="Timepoint", strict = TRUE, YAxisLabel = " ",
                    RepairVisits=Repair)
```

'

if (any(TheseLasers %in% "red")){

  if (timepointType == "single"){
RMFI_Intermediate <- '
RedPlotsMFI <- QC_Plots(x=x, FailedFlag=FALSE, MeasurementType=RedGains,
                          plotType = "individual", returntype = "plots",
                          Metadata="NULL", strict = TRUE, YAxisLabel = "MFI",
                          RepairVisits=Repair)
    '
      } else {
RMFI_Intermediate <- '
RedPlotsMFI <- QC_Plots(x=x, FailedFlag=FALSE, MeasurementType=RedGains,
                         plotType = "comparison", returntype = "plots",
                         Metadata="Timepoint", strict = TRUE, YAxisLabel = "MFI",
                         RepairVisits=Repair)
    '
    }
    
    SectionMFI_2 <- paste(RMFI_Intermediate, SectionMFI_2, sep = "\n")
    #cat(SectionMFI_2) 
}
  
if (any(TheseLasers %in% "yellowgreen")){

  if (timepointType == "single"){
YMFI_Intermediate <- '
YellowGreenPlotsMFI <- QC_Plots(x=x, FailedFlag=FALSE, MeasurementType=YellowGreenGains,
                          plotType = "individual", returntype = "plots",
                          Metadata="NULL", strict = TRUE, YAxisLabel = "MFI",
                          RepairVisits=Repair)
    '
      } else {
YMFI_Intermediate <- '
YellowGreenPlotsMFI <- QC_Plots(x=x, FailedFlag=FALSE, MeasurementType=YellowGreenGains,
                         plotType = "comparison", returntype = "plots",
                         Metadata="Timepoint", strict = TRUE, YAxisLabel = "MFI",
                         RepairVisits=Repair)
    '
    }
    
    SectionMFI_2 <- paste(YMFI_Intermediate, SectionMFI_2, sep = "\n")
    #cat(SectionMFI_2)  
}
  
if (any(TheseLasers %in% "blue")){

  if (timepointType == "single"){
BMFI_Intermediate <- '
BluePlotsMFI <- QC_Plots(x=x, FailedFlag=FALSE, MeasurementType=BlueGains,
                          plotType = "individual", returntype = "plots",
                          Metadata="NULL", strict = TRUE, YAxisLabel = "MFI",
                          RepairVisits=Repair)
    '
      } else {
BMFI_Intermediate <- '
BluePlotsMFI <- QC_Plots(x=x, FailedFlag=FALSE, MeasurementType=BlueGains,
                         plotType = "comparison", returntype = "plots",
                         Metadata="Timepoint", strict = TRUE, YAxisLabel = "MFI",
                         RepairVisits=Repair)
    '
    }
    
    SectionMFI_2 <- paste(BMFI_Intermediate, SectionMFI_2, sep = "\n")
    #cat(SectionMFI_2) s
}
  
if (any(TheseLasers %in% "violet")){

  if (timepointType == "single"){
VMFI_Intermediate <- '
VioletPlotsMFI <- QC_Plots(x=x, FailedFlag=FALSE, MeasurementType=VioletGains,
                          plotType = "individual", returntype = "plots",
                          Metadata="NULL", strict = TRUE, YAxisLabel = "MFI",
                          RepairVisits=Repair)
    '
      } else {
VMFI_Intermediate <- '
VioletPlotsMFI <- QC_Plots(x=x, FailedFlag=FALSE, MeasurementType=VioletGains,
                         plotType = "comparison", returntype = "plots",
                         Metadata="Timepoint", strict = TRUE, YAxisLabel = "MFI",
                         RepairVisits=Repair)
    '
    }
    
  SectionMFI_2 <- paste(VMFI_Intermediate, SectionMFI_2, sep = "\n")
  #cat(SectionMFI_2) 
}

if (any(TheseLasers %in% "uv")){

  if (timepointType == "single"){
UVMFI_Intermediate <- '
UltraVioletPlotsMFI <- QC_Plots(x=x, FailedFlag=FALSE, MeasurementType=UltraVioletGains,
                      plotType = "individual", returntype = "plots",
                      Metadata="NULL", strict = TRUE, YAxisLabel = "MFI",
                      RepairVisits=Repair)
'
  } else {
UVMFI_Intermediate <- '
UltraVioletPlotsMFI <- QC_Plots(x=x, FailedFlag=FALSE, MeasurementType=UltraVioletGains,
                     plotType = "comparison", returntype = "plots",
                     Metadata="Timepoint", strict = TRUE, YAxisLabel = "MFI",
                     RepairVisits=Repair)
'
}
  

SectionMFI_2 <- paste(UVMFI_Intermediate, SectionMFI_2, sep = "\n")
#cat(SectionMFI_2) 
}
  

SectionGain <- '
```{r}
x <- InstrumentData
TheColumns <- x %>% select(where(~is.numeric(.)||is.integer(.))) %>% colnames()
TheColumns <- setdiff(TheColumns, "TIME")
TheColumnNames <- TheColumns[str_detect(TheColumns, "Gain")]
TheColumnNames <- TheColumnNames[str_detect(TheColumnNames, "-A")]

UltraVioletGains <- TheColumnNames[str_detect(TheColumnNames, "^UV")]
VioletGains <- TheColumnNames[str_detect(TheColumnNames, "^V")]
BlueGains <- TheColumnNames[str_detect(TheColumnNames, "^B")]
YellowGreenGains <- TheColumnNames[str_detect(TheColumnNames, "^YG")]
RedGains <- TheColumnNames[str_detect(TheColumnNames, "^R")]

ScatterGains <- TheColumnNames[str_detect(TheColumnNames, "SC-")]
ScatterGains <- Luciernaga:::ScalePriority(ScatterGains)
LaserGains <- TheColumns[str_detect(TheColumns, "Laser")]
LaserGains <- Luciernaga:::ColorPriority(LaserGains)
ScalingGains <- TheColumns[str_detect(TheColumns, "Scaling")]
ScalingGains <- Luciernaga:::ColorPriority(ScalingGains)
'

SectionGain2 <- '

ScatterPlotsGain <- QC_Plots(x=x, FailedFlag=FALSE, MeasurementType=ScatterGains,
                    plotType = "individual", returntype = "plots", YAxisLabel = " ",
                    RepairVisits=Repair)

LaserPlotsGain <- QC_Plots(x=x, FailedFlag=FALSE, MeasurementType=LaserGains,
                    plotType = "individual", returntype = "plots", YAxisLabel = " ",
                    RepairVisits=Repair)

ScalingPlotsGain <- QC_Plots(x=x, FailedFlag=FALSE, MeasurementType=ScalingGains,
                    plotType = "individual", returntype = "plots", YAxisLabel = " ",
                    RepairVisits=Repair)
```

'
  
if (any(TheseLasers %in% "red")){

RGain_Intermediate <- '
RedPlotsGain <- QC_Plots(x=x, FailedFlag=FALSE, MeasurementType=RedGains,
                         plotType = "individual", returntype = "plots",
                         YAxisLabel = "Gain", RepairVisits=Repair)
    
    '
    
    SectionGain2 <- paste(RGain_Intermediate, SectionGain2, sep = "\n")
    #cat(SectionGain2) 
}

if (any(TheseLasers %in% "yellowgreen")){

YGain_Intermediate <- '
YellowGreenPlotsGain <- QC_Plots(x=x, FailedFlag=FALSE, MeasurementType=YellowGreenGains,
                            plotType = "individual", returntype = "plots",
                            YAxisLabel = "Gain", RepairVisits=Repair)
      
      '
      
      SectionGain2 <- paste(YGain_Intermediate, SectionGain2, sep = "\n")
      #cat(SectionGain2)  
}

if (any(TheseLasers %in% "blue")){


BGain_Intermediate <- '
BluePlotsGain <- QC_Plots(x=x, FailedFlag=FALSE, MeasurementType=BlueGains,
                                plotType = "individual", returntype = "plots",
                                YAxisLabel = "Gain", RepairVisits=Repair)
          
          '
          
          SectionGain2 <- paste(BGain_Intermediate, SectionGain2, sep = "\n")
          #cat(SectionGain2) s
}
  
if (any(TheseLasers %in% "violet")){

VGain_Intermediate <- '
VioletPlotsGain <- QC_Plots(x=x, FailedFlag=FALSE, MeasurementType=VioletGains,
                            plotType = "individual", returntype = "plots",
                            YAxisLabel = "Gain", RepairVisits=Repair)
      
      '
    
      SectionGain2 <- paste(VGain_Intermediate, SectionGain2, sep = "\n")
      #cat(SectionGain2) 
}
  
if (any(TheseLasers %in% "uv")){

UVGain_Intermediate <- '
UltraVioletPlotsGain <- QC_Plots(x=x, FailedFlag=FALSE, MeasurementType=UltraVioletGains,
                        plotType = "individual", returntype = "plots",
                        YAxisLabel = "Gain", RepairVisits=Repair)
  
  '
  
  SectionGain2 <- paste(UVGain_Intermediate, SectionGain2, sep = "\n")
  #cat(SectionGain2) 
}

SectionRCV <- '
```{r}
x <- InstrumentData
TheColumns <- x %>% select(where(~is.numeric(.)||is.integer(.))) %>% colnames()
TheColumns <- setdiff(TheColumns, "TIME")
TheColumnNames <- TheColumns[str_detect(TheColumns, "rCV")]
TheColumnNames <- TheColumnNames[!str_detect(TheColumnNames, "-H")]

UltraVioletGains <- TheColumnNames[str_detect(TheColumnNames, "^UV")]
VioletGains <- TheColumnNames[str_detect(TheColumnNames, "^V")]
BlueGains <- TheColumnNames[str_detect(TheColumnNames, "^B")]
YellowGreenGains <- TheColumnNames[str_detect(TheColumnNames, "^YG")]
RedGains <- TheColumnNames[str_detect(TheColumnNames, "^R")]

ScatterGains <- TheColumnNames[str_detect(TheColumnNames, "SC-")]
ScatterGains <- Luciernaga:::ScalePriority(ScatterGains)
LaserGains <- TheColumns[str_detect(TheColumns, "Laser")]
LaserGains <- Luciernaga:::ColorPriority(LaserGains)
ScalingGains <- TheColumns[str_detect(TheColumns, "Scaling")]
ScalingGains <- Luciernaga:::ColorPriority(ScalingGains)
OtherGains <- c(ScatterGains)
'

SectionRCV2 <- '

ScatterPlotsRCV <- QC_Plots(x=x, FailedFlag=FALSE, MeasurementType=ScatterGains,
                    plotType = "individual", returntype = "plots", YAxisLabel = " ",
                    RepairVisits=Repair)
```

'
    
if (any(TheseLasers %in% "red")){
  
      RRCV_Intermediate <- '
RedPlotsRCV <- QC_Plots(x=x, FailedFlag=FALSE, MeasurementType=RedGains,
plotType = "individual", returntype = "plots", YAxisLabel = "%rCV",
RepairVisits=Repair)
      '
      
      SectionRCV2 <- paste(RRCV_Intermediate, SectionRCV2, sep = "\n")
      #cat(SectionRCV2) 
}
  
if (any(TheseLasers %in% "yellowgreen")){
  
  YRCV_Intermediate <- '
YellowGreenPlotsRCV <- QC_Plots(x=x, FailedFlag=FALSE, MeasurementType=YellowGreenGains,
plotType = "individual", returntype = "plots", YAxisLabel = "%rCV",
RepairVisits=Repair)
      '
      
      SectionRCV2 <- paste(YRCV_Intermediate, SectionRCV2, sep = "\n")
      #cat(SectionRCV2) 
}  
  
if (any(TheseLasers %in% "blue")){
  
  
  BRCV_Intermediate <- '
BluePlotsRCV <- QC_Plots(x=x, FailedFlag=FALSE, MeasurementType=BlueGains,
plotType = "individual", returntype = "plots", YAxisLabel = "%rCV",
RepairVisits=Repair)
      '
      
      SectionRCV2 <- paste(BRCV_Intermediate, SectionRCV2, sep = "\n")
      #cat(SectionRCV2) 
}  
  
if (any(TheseLasers %in% "violet")){
  
  VRCV_Intermediate <- '
VioletPlotsRCV <- QC_Plots(x=x, FailedFlag=FALSE, MeasurementType=VioletGains,
plotType = "individual", returntype = "plots", strict=TRUE, YAxisLabel = "%rCV",
RepairVisits=Repair)
      '
    
      SectionRCV2 <- paste(VRCV_Intermediate, SectionRCV2, sep = "\n")
      #cat(SectionRCV2) 
} 
  
if (any(TheseLasers %in% "uv")){

  UVRCV_Intermediate <- '
UltraVioletPlotsRCV <- QC_Plots(x=x, FailedFlag=FALSE, MeasurementType=UltraVioletGains,
plotType = "individual", returntype = "plots", YAxisLabel = "%rCV",
RepairVisits=Repair)
  '
  
  SectionRCV2 <- paste(UVRCV_Intermediate, SectionRCV2, sep = "\n")
  #cat(SectionRCV2) 
}
  
# Handling the PDF plot arguments
  
TheMFIs <- ""
TheGains <- ""
TheRCVs <- ""

if (any(TheseLasers %in% "uv")){
TheMFIs <- paste0(TheMFIs, "UltraVioletPlotsMFI, ")
TheGains <- paste0(TheGains, "UltraVioletPlotsGain, ")
TheRCVs <- paste0(TheRCVs, "UltraVioletPlotsRCV, ")
}
  
if (any(TheseLasers %in% "violet")){
TheMFIs <- paste0(TheMFIs, "VioletPlotsMFI, ")
TheGains <- paste0(TheGains, "VioletPlotsGain, ")
TheRCVs <- paste0(TheRCVs, "VioletPlotsRCV, ")
}

if (any(TheseLasers %in% "blue")){
TheMFIs <- paste0(TheMFIs, "BluePlotsMFI, ")
TheGains <- paste0(TheGains, "BluePlotsGain, ")
TheRCVs <- paste0(TheRCVs, "BluePlotsRCV, ")
}
  
if (any(TheseLasers %in% "yellowgreen")){
TheMFIs <- paste0(TheMFIs, "UltraVioletPlotsMFI, ")
TheGains <- paste0(TheGains, "UltraVioletPlotsGain, ")
TheRCVs <- paste0(TheRCVs, "UltraVioletPlotsRCV, ")
}
  
if (any(TheseLasers %in% "red")){
TheMFIs <- paste0(TheMFIs, "RedPlotsMFI, ")
TheGains <- paste0(TheGains, "RedPlotsGain, ")
TheRCVs <- paste0(TheRCVs, "RedPlotsRCV, ")
}
  
TheMFIs <- paste0(TheMFIs, "ScatterPlotsMFI, ", "LaserPlotsMFI, ")
TheGains <- paste0(TheGains, "ScatterPlotsGain, ", "LaserPlotsGain, ", "ScalingPlotsGain, ")
TheRCVs <- paste0(TheRCVs, "ScatterPlotsRCV")
  
TheCongregation <- paste0(TheMFIs, TheGains, TheRCVs)
# Please Remove Final Comma if terminal.
  
SectionPDF <- sprintf('
```{r}
#| include: false
#| echo: false

PDFPlots <- c(%s)

Filename <- paste0("%s")

PDF <- Utility_Patchwork(x=PDFPlots, filename=Filename, returntype="pdf", outfolder=MainFolder, thecolumns=1)
```
', TheCongregation, PDFValue)

if (timepointType == "single"){  
TextInput <- "during daily"
} else {TextInput <- "before and after daily"}

Section2 <- sprintf('

## {.sidebar}
Dashboard data for the **%s** last updated on **`r TheDate`**

**First Column: MFI** Median Fluorescent Intensity (MFI) values for QC beads acquired %s QC. Measures stability over time. 
**Second Column: Gain** Gain (Voltage) values set for instrument after QC. Changes over time reflective of laser health. 
**Third Colum: RCV** Percentage change of Robust Coefficient Variation (RCV) after QC. Higher values reflect decreased resolution between positive and negative for that detector. 

For additional information concerning individual parameter tabs, navigate to the [Help](help.qmd) page.

**About**

This dashboard contains the visualized QC data for the cytometers at the [%s](%s)


This dashboard was created with [Quarto](https://quarto.org/) using [CytometryQC](https://github.com/DavidRach/CytometryQC)

## MFI {.tabset}

## Gain {.tabset}

## rCV {.tabset}

', InstrumentName, TextInput, organization_name, organization_website)
 
cat(Section1, SectionMFI, SectionMFI_2, SectionGain, SectionGain2,
    SectionRCV, SectionRCV2, SectionPDF, Section2, file = StorageLocation)

}