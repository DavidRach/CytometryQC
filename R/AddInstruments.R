#' Adds Instruments to the base Webpage
#' 
#' @param outpath
#' 
#' @importFrom purrr map
#' @importFrom stringr str_which
#' @importFrom stringr str_detect
#' @importFrom stringr fixed 
#' @importFrom stringr str_replace
#' 
#' @return Updated webpage
#' 
#' @export
AddInstruments <- function(name, manufacturer=NULL, uv=16, violet=16, blue=14,
yellowgreen=10, red=8, organization="UMGCC FCSS", 
organization_website="https://www.medschool.umaryland.edu/cibr/core/umgccc_flow/"){

  DocumentsPath <- OperatingSystemCheck()
  InstrumentQC <- list.files(DocumentsPath, pattern="^InstrumentQC2$",
   full.names=TRUE)
  if (length(InstrumentQC) == 0){stop("Run FolderSetup step first!")}
  
  # Add Instrument Data Folder

  InstrumentQCPath <- file.path(DocumentsPath, "InstrumentQC2")
  DataPath <- file.path(DocumentsPath, "InstrumentQC2", "data")
  Hits <- list.files(DataPath, pattern=name, full.names=TRUE)

  if (length(Hits) == 0){
    dir.create(file.path(DataPath, name),
     showWarnings = FALSE)
    dir.create(file.path(DataPath, name, "Archive"),
     showWarnings = FALSE)
  }

  # Add Instrument QMD file

  AddInstrumentQMD(name=name, outpath=InstrumentQCPath, organization=organization,
    organization_website=organization_website)
  
  Items <- list.files(InstrumentQCPath, pattern=paste0(name, ".qmd"),
   full.names=TRUE)
  
  if (length(Items) == 1){
    Draft <- readLines(Items)
    MFISegment <- grep("^## MFI", Draft)
    MFIAddition <- MFI_Display(uv=uv, violet=violet,
      blue=blue, yellowgreen=yellowgreen, red=red)
    
    GainSegment <- grep("^## Gain", Draft)
    GainAddition <- MFI_Display(uv=uv, violet=violet,
      blue=blue, yellowgreen=yellowgreen, red=red)
    
    RCVSegment <- grep("^## rCV", Draft)
    RCVAddition <- MFI_Display(uv=uv, violet=violet,
      blue=blue, yellowgreen=yellowgreen, red=red) 
    
    Draft[MFISegment] <- paste0("## MFI\n", MFIAddition)
    Draft[GainSegment] <- paste0("## Gain\n", GainAddition)
    Draft[RCVSegment] <- paste0("## rCV\n", RCVAddition)
      
    cat(Draft, file = Items, sep = "\n")

  } else {stop("This shouldn't have happened")}
  
  # Update .yaml

  Items <- list.files(InstrumentQCPath, pattern=paste0(name, ".qmd"),
   full.names=TRUE)
  Yaml <- list.files(InstrumentQCPath, pattern=".yml",
   full.names=TRUE)
  
  if (length(Items) ==1){
    Draft <- readLines(Yaml)
    Line1 <- '    - text: "Levey-Jennings Plots"'
    Line2 <- '      menu:'
    TheLocation <- str_which(Draft, fixed(Line1))

    InsertOne <- "      - text: \"Instrument\""
    InsertTwo <- "        href: Instrument.qmd"

    NewInsertOne <- str_replace(InsertOne, fixed("Instrument"), name)
    NewInsertTwo <- str_replace(InsertTwo, fixed("Instrument"), name)

    if (length(TheLocation) > 0) {
      Draft <- append(Draft, c(NewInsertOne, NewInsertTwo),
        after = TheLocation + 1)
    }

    Line1 <- "    - text: \"Historical\""
    Line2 <- "      menu:"
    TheLocation <- str_which(Draft, fixed(Line1))

    InsertOne <- "      - text: \"Instrument\""
    InsertTwo <- "        href: Historical.qmd"

    NewInsertOne <- str_replace(InsertOne, fixed("Instrument"), name)
    NewInsertTwo <- str_replace(InsertTwo, fixed("Historical"), name)

    if (length(TheLocation) > 0) {
      Draft <- append(Draft, c(NewInsertOne, NewInsertTwo),
        after = TheLocation + 1)
    }

    writeLines(Draft, Yaml)
  }
  
  # Add Instrument Script()


}

MFI_Display <- function(uv=uv, violet=violet,
  blue=blue, yellowgreen=yellowgreen, red=red){
  
  UVCombined <- ""
  VioletCombined <- ""
  BlueCombined <- ""
  YellowGreenCombined <- ""
  RedCombined <- ""
  
  if (uv > 0){
    UVDetectors <- uv
    UVLines <- map(1:UVDetectors, ~ sprintf(
      "ggplotly(UltraVioletPlotsMFI[[%d]])", .x))
    UVCombined <- paste(
      "```{r}\n#| title: UltraViolet\n",
      paste(UVLines, collapse = "\n"),
      "\n```\n",
      sep = ""
    )
  }

  if (violet > 0){
    VioletDetectors <- violet
    VioletLines <- map(1:VioletDetectors, ~ sprintf(
      "ggplotly(VioletPlotsMFI[[%d]])", .x))
    VioletCombined <- paste(
      "```{r}\n#| title: Violet\n",
      paste(VioletLines, collapse = "\n"),
      "\n```\n",
      sep = ""
    )
  }

  if (blue > 0){
    BlueDetectors <- blue
    BlueLines <- map(1:BlueDetectors, ~ sprintf(
      "ggplotly(BluePlotsMFI[[%d]])", .x))
      BlueCombined <- paste(
      "```{r}\n#| title: Blue\n",
      paste(BlueLines, collapse = "\n"),
      "\n```\n",
      sep = ""
    )   
  }

  if (yellowgreen > 0){
    YellowGreenDetectors <- yellowgreen
    YellowGreenLines <- map(1:YellowGreenDetectors, ~ sprintf(
      "ggplotly(YellowGreenPlotsMFI[[%d]])", .x))
      YellowGreenCombined <- paste(
      "```{r}\n#| title: YellowGreen\n",
      paste(YellowGreenLines, collapse = "\n"),
      "\n```\n",
      sep = ""
    )    
  }

  if (red > 0){
    RedDetectors <- red
    RedLines <- map(1:RedDetectors, ~ sprintf(
      "ggplotly(RedPlotsMFI[[%d]])", .x))
      RedCombined <- paste(
      "```{r}\n#| title: Red\n",
      paste(RedLines, collapse = "\n"),
      "\n```\n",
      sep = ""
    )        
  }

  AllCombined <- paste(
    UVCombined,
    VioletCombined,
    BlueCombined,
    YellowGreenCombined,
    RedCombined,
    sep = ""
  )

  AllCombined <- trimws(AllCombined)
  return(AllCombined)
}

Gain_Display <- function(uv=uv, violet=violet,
  blue=blue, yellowgreen=yellowgreen, red=red){
  
  UVCombined <- ""
  VioletCombined <- ""
  BlueCombined <- ""
  YellowGreenCombined <- ""
  RedCombined <- ""
  
  if (uv > 0){
    UVDetectors <- uv
    UVLines <- map(1:UVDetectors, ~ sprintf(
      "ggplotly(UltraVioletPlotsGain[[%d]])", .x))
    UVCombined <- paste(
      "```{r}\n#| title: UltraViolet\n",
      paste(UVLines, collapse = "\n"),
      "\n```\n",
      sep = ""
    )
  }

  if (violet > 0){
    VioletDetectors <- violet
    VioletLines <- map(1:VioletDetectors, ~ sprintf(
      "ggplotly(VioletPlotsGain[[%d]])", .x))
    VioletCombined <- paste(
      "```{r}\n#| title: Violet\n",
      paste(VioletLines, collapse = "\n"),
      "\n```\n",
      sep = ""
    )
  }

  if (blue > 0){
    BlueDetectors <- blue
    BlueLines <- map(1:BlueDetectors, ~ sprintf(
      "ggplotly(BluePlotsGain[[%d]])", .x))
      BlueCombined <- paste(
      "```{r}\n#| title: Blue\n",
      paste(BlueLines, collapse = "\n"),
      "\n```\n",
      sep = ""
    )   
  }

  if (yellowgreen > 0){
    YellowGreenDetectors <- yellowgreen
    YellowGreenLines <- map(1:YellowGreenDetectors, ~ sprintf(
      "ggplotly(YellowGreenPlotsGain[[%d]])", .x))
      YellowGreenCombined <- paste(
      "```{r}\n#| title: YellowGreen\n",
      paste(YellowGreenLines, collapse = "\n"),
      "\n```\n",
      sep = ""
    )    
  }

  if (red > 0){
    RedDetectors <- red
    RedLines <- map(1:RedDetectors, ~ sprintf(
      "ggplotly(RedPlotsGain[[%d]])", .x))
      RedCombined <- paste(
      "```{r}\n#| title: Red\n",
      paste(RedLines, collapse = "\n"),
      "\n```\n",
      sep = ""
    )        
  }

  AllCombined <- paste(
    UVCombined,
    VioletCombined,
    BlueCombined,
    YellowGreenCombined,
    RedCombined,
    sep = ""
  )

  AllCombined <- trimws(AllCombined)
  return(AllCombined)
}

RCV_Display <- function(uv=uv, violet=violet,
  blue=blue, yellowgreen=yellowgreen, red=red){
  
  UVCombined <- ""
  VioletCombined <- ""
  BlueCombined <- ""
  YellowGreenCombined <- ""
  RedCombined <- ""
  
  if (uv > 0){
    UVDetectors <- uv
    UVLines <- map(1:UVDetectors, ~ sprintf(
      "ggplotly(UltraVioletPlotsRCV[[%d]])", .x))
    UVCombined <- paste(
      "```{r}\n#| title: UltraViolet\n",
      paste(UVLines, collapse = "\n"),
      "\n```\n",
      sep = ""
    )
  }

  if (violet > 0){
    VioletDetectors <- violet
    VioletLines <- map(1:VioletDetectors, ~ sprintf(
      "ggplotly(VioletPlotsRCV[[%d]])", .x))
    VioletCombined <- paste(
      "```{r}\n#| title: Violet\n",
      paste(VioletLines, collapse = "\n"),
      "\n```\n",
      sep = ""
    )
  }

  if (blue > 0){
    BlueDetectors <- blue
    BlueLines <- map(1:BlueDetectors, ~ sprintf(
      "ggplotly(BluePlotsRCV[[%d]])", .x))
      BlueCombined <- paste(
      "```{r}\n#| title: Blue\n",
      paste(BlueLines, collapse = "\n"),
      "\n```\n",
      sep = ""
    )   
  }

  if (yellowgreen > 0){
    YellowGreenDetectors <- yellowgreen
    YellowGreenLines <- map(1:YellowGreenDetectors, ~ sprintf(
      "ggplotly(YellowGreenPlotsRCV[[%d]])", .x))
      YellowGreenCombined <- paste(
      "```{r}\n#| title: YellowGreen\n",
      paste(YellowGreenLines, collapse = "\n"),
      "\n```\n",
      sep = ""
    )    
  }

  if (red > 0){
    RedDetectors <- red
    RedLines <- map(1:RedDetectors, ~ sprintf(
      "ggplotly(RedPlotsRCV[[%d]])", .x))
      RedCombined <- paste(
      "```{r}\n#| title: Red\n",
      paste(RedLines, collapse = "\n"),
      "\n```\n",
      sep = ""
    )        
  }

  AllCombined <- paste(
    UVCombined,
    VioletCombined,
    BlueCombined,
    YellowGreenCombined,
    RedCombined,
    sep = ""
  )

  AllCombined <- trimws(AllCombined)
  return(AllCombined)
}

AddInstrumentScript <- function(){
message("In preparation")
}

AddInstrumentQMD <- function(name="5L", outpath="/home/david/Desktop", organization="UMGCC FCSS",
 organization_website="https://www.medschool.umaryland.edu/cibr/core/umgccc_flow/"){
  
  filename <- paste0(name, ".qmd")
  PDFValue <- paste0("QCPlots_", name)
  InstrumentName <- paste0("Cytek Aurora ", name)

  StorageLocation <- file.path(outpath, filename)

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
walk(.x=TheList, MainFolder=MainFolder, .f=Luciernaga:::DailyQCParse)
walk(.x=TheList, .f=Luciernaga:::QCBeadParse, MainFolder=MainFolder)
```

```{r}
MFI <- Luciernaga:::CurrentData(x="%s", MainFolder=MainFolder, type = "MFI")
Gain <- Luciernaga:::CurrentData(x="%s", MainFolder=MainFolder, type = "Gain")
TheDate <- MFI |> slice(1) |> pull(DATE)
```

```{r}
WindowOfInterest <- Sys.time() - months(12)

MFI <- MFI |> filter(DateTime >= WindowOfInterest)
Gain <- Gain |> filter(DateTime >= WindowOfInterest)
```

```{r}
Data <- read.csv("AuroraMaintenance.csv", check.names=FALSE)

Data <- Data |> filter(!str_detect(reason, "lean"))

Repair <- Data |> filter(instrument %%in%% "%s")
```
', name, name, name, name)

SectionMFI <- '
```{r}
x <- MFI
x <- x |> dplyr::filter(Timepoint %in% c("Before", "After"))
TheColumns <- x %>% select(where(~is.numeric(.)||is.integer(.))) %>% colnames()
TheColumns <- setdiff(TheColumns, "TIME")
TheIntermediate <- TheColumns[!str_detect(TheColumns, "Gain")]
TheColumnNames <- TheIntermediate[str_detect(TheIntermediate, "-A")]
  
UltraVioletGains <- TheColumnNames[str_detect(TheColumnNames, "^UV")]
VioletGains <- TheColumnNames[str_detect(TheColumnNames, "^V")]
BlueGains <- TheColumnNames[str_detect(TheColumnNames, "^B")]
YellowGreenGains <- TheColumnNames[str_detect(TheColumnNames, "^YG")]
RedGains <- TheColumnNames[str_detect(TheColumnNames, "^R")]

ScatterGains <- TheIntermediate[str_detect(TheIntermediate, "SC-")]
ScatterGains <- Luciernaga:::ScalePriority(ScatterGains)
LaserGains <- TheIntermediate[str_detect(TheIntermediate, "Laser")]
LaserGains <- Luciernaga:::ColorPriority(LaserGains)
ScalingGains <- TheIntermediate[str_detect(TheIntermediate, "Scaling")]
ScalingGains <- Luciernaga:::ColorPriority(ScalingGains)
OtherGains <- c(ScatterGains, LaserGains, ScalingGains)

UltraVioletPlotsMFI <- QC_Plots(x=x, FailedFlag=TRUE, MeasurementType=UltraVioletGains,
                      plotType = "comparison", returntype = "plots",
                      Metadata="Timepoint", strict = TRUE, YAxisLabel = "MFI",
                      RepairVisits=Repair)

VioletPlotsMFI <- QC_Plots(x=x, FailedFlag=TRUE, MeasurementType=VioletGains,
                      plotType = "comparison", returntype = "plots",
                      Metadata="Timepoint", strict = TRUE, YAxisLabel = "MFI",
                      RepairVisits=Repair)

BluePlotsMFI <- QC_Plots(x=x, FailedFlag=TRUE, MeasurementType=BlueGains,
                      plotType = "comparison", returntype = "plots",
                      Metadata="Timepoint", strict = TRUE, YAxisLabel = "MFI",
                      RepairVisits=Repair)

YellowGreenPlotsMFI <- QC_Plots(x=x, FailedFlag=TRUE, MeasurementType=YellowGreenGains,
                      plotType = "comparison", returntype = "plots",
                      Metadata="Timepoint", strict = TRUE, YAxisLabel = "MFI",
                      RepairVisits=Repair)

RedPlotsMFI <- QC_Plots(x=x, FailedFlag=TRUE, MeasurementType=RedGains,
                     plotType = "comparison", returntype = "plots",
                     Metadata="Timepoint", strict = TRUE, YAxisLabel = "MFI",
                     RepairVisits=Repair)

ScatterPlotsMFI <- QC_Plots(x=x, FailedFlag=TRUE, MeasurementType=ScatterGains,
                     plotType = "comparison", returntype = "plots",
                     Metadata="Timepoint", strict = TRUE, YAxisLabel = " ",
                     RepairVisits=Repair)

LaserPlotsMFI <- QC_Plots(x=x, FailedFlag=TRUE, MeasurementType=LaserGains,
                     plotType = "comparison", returntype = "plots",
                     Metadata="Timepoint", strict = TRUE, YAxisLabel = " ",
                     RepairVisits=Repair)
```
'

SectionGain <- '
```{r}
x <- Gain
TheColumns <- x %>% select(where(~is.numeric(.)||is.integer(.))) %>% colnames()
TheColumns <- setdiff(TheColumns, "TIME")
TheColumnNames <- TheColumns[str_detect(TheColumns, "Gain")]

UltraVioletGains <- TheColumnNames[str_detect(TheColumnNames, "^UV")]
VioletGains <- TheColumnNames[str_detect(TheColumnNames, "^V")]
BlueGains <- TheColumnNames[str_detect(TheColumnNames, "^B")]
YellowGreenGains <- TheColumnNames[str_detect(TheColumnNames, "^YG")]
RedGains <- TheColumnNames[str_detect(TheColumnNames, "^R")]

ScatterGains <- TheColumnNames[str_detect(TheColumnNames, "SC-")]
ScatterGains <- Luciernaga:::ScalePriority(ScatterGains)
LaserGains <- TheColumns[str_detect(TheColumns, "Laser")]
LaserDelayGains <- LaserGains[str_detect(LaserGains, "Delay")]
LaserDelayGains <- Luciernaga:::ColorPriority(LaserDelayGains)
LaserPowerGains <- LaserGains[str_detect(LaserGains, "Power")]
LaserPowerGains <- Luciernaga:::ColorPriority(LaserPowerGains)
ScalingGains <- TheColumns[str_detect(TheColumns, "Scaling")]
ScalingGains <- Luciernaga:::ColorPriority(ScalingGains)

UltraVioletPlotsGain <- QC_Plots(x=x, FailedFlag=TRUE, MeasurementType=UltraVioletGains,
                      plotType = "individual", returntype = "plots", YAxisLabel = "Gain",
                      RepairVisits=Repair)

VioletPlotsGain <- QC_Plots(x=x, FailedFlag=TRUE, MeasurementType=VioletGains,
                      plotType = "individual", returntype = "plots", strict = TRUE, YAxisLabel = "Gain",
                      RepairVisits=Repair)

BluePlotsGain <- QC_Plots(x=x, FailedFlag=TRUE, MeasurementType=BlueGains,
                      plotType = "individual", returntype = "plots", YAxisLabel = "Gain",
                      RepairVisits=Repair)

YellowGreenPlotsGain <- QC_Plots(x=x, FailedFlag=TRUE, MeasurementType=YellowGreenGains,
                      plotType = "individual", returntype = "plots", YAxisLabel = "Gain",
                      RepairVisits=Repair)

RedPlotsGain <- QC_Plots(x=x, FailedFlag=TRUE, MeasurementType=RedGains,
                     plotType = "individual", returntype = "plots", YAxisLabel = "Gain",
                     RepairVisits=Repair)

ScatterPlotsGain <- QC_Plots(x=x, FailedFlag=TRUE, MeasurementType=ScatterGains,
                     plotType = "individual", returntype = "plots", YAxisLabel = " ",
                     RepairVisits=Repair)

LaserDelayPlotsGain <- QC_Plots(x=x, FailedFlag=TRUE, MeasurementType=LaserDelayGains,
                     plotType = "individual", returntype = "plots", YAxisLabel = " ",
                     RepairVisits=Repair)

LaserPowerPlotsGain <- QC_Plots(x=x, FailedFlag=TRUE, MeasurementType=LaserPowerGains,
                                plotType = "individual", returntype = "plots",
                                YAxisLabel = " ", RepairVisits=Repair)

ScalingPlotsGain <- QC_Plots(x=x, FailedFlag=TRUE, MeasurementType=ScalingGains,
                     plotType = "individual", returntype = "plots", YAxisLabel = " ",
                     RepairVisits=Repair)
```

'

SectionRCV <- '
```{r}
x <- Gain
TheColumns <- x %>% select(where(~is.numeric(.)||is.integer(.))) %>% colnames()
TheColumns <- setdiff(TheColumns, "TIME")

TheColumnNames <- TheColumns[str_detect(TheColumns, "rCV")]
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

UltraVioletPlotsRCV <- QC_Plots(x=x, FailedFlag=TRUE, MeasurementType=UltraVioletGains,
                      plotType = "individual", returntype = "plots", YAxisLabel = "%rCV",
                      RepairVisits=Repair)

VioletPlotsRCV <- QC_Plots(x=x, FailedFlag=TRUE, MeasurementType=VioletGains,
                      plotType = "individual", returntype = "plots", strict=TRUE, YAxisLabel = "%rCV",
                      RepairVisits=Repair)

BluePlotsRCV <- QC_Plots(x=x, FailedFlag=TRUE, MeasurementType=BlueGains,
                      plotType = "individual", returntype = "plots", YAxisLabel = "%rCV",
                      RepairVisits=Repair)

YellowGreenPlotsRCV <- QC_Plots(x=x, FailedFlag=TRUE, MeasurementType=YellowGreenGains,
                      plotType = "individual", returntype = "plots", YAxisLabel = "%rCV",
                      RepairVisits=Repair)

RedPlotsRCV <- QC_Plots(x=x, FailedFlag=TRUE, MeasurementType=RedGains,
                     plotType = "individual", returntype = "plots", YAxisLabel = "%rCV",
                     RepairVisits=Repair)

ScatterPlotsRCV <- QC_Plots(x=x, FailedFlag=TRUE, MeasurementType=ScatterGains,
                     plotType = "individual", returntype = "plots", YAxisLabel = " ",
                     RepairVisits=Repair)
```
'
  
SectionPDF <- sprintf('
```{r}
#| include: false
#| echo: false

PDFPlots <- c(UltraVioletPlotsMFI, VioletPlotsMFI, BluePlotsMFI, YellowGreenPlotsMFI, RedPlotsMFI, LaserPlotsMFI, ScatterPlotsMFI, UltraVioletPlotsGain, VioletPlotsGain, BluePlotsGain, YellowGreenPlotsGain, RedPlotsGain, ScatterPlotsGain, LaserDelayPlotsGain, LaserPowerPlotsGain,  ScalingPlotsGain, UltraVioletPlotsRCV, VioletPlotsRCV, BluePlotsRCV, YellowGreenPlotsRCV, RedPlotsRCV, ScatterPlotsRCV)

Filename <- paste0("%s")

PDF <- Utility_Patchwork(x=PDFPlots, filename=Filename, returntype="pdf", outfolder=MainFolder, thecolumns=1)
```
', PDFValue)

Section2 <- sprintf('

## {.sidebar}
Dashboard data for the **%s** last updated on **`r TheDate`**

**First Column: MFI** Median Fluorescent Intensity (MFI) values for QC beads acquired Before and After QC. Measures stability over time. 
**Second Column: Gain** Gain (Voltage) values set for instrument after QC. Changes over time reflective of laser health. 
**Third Colum: RCV** Percentage change of Robust Coefficient Variation (RCV) after QC. Higher values reflect decreased resolution between positive and negative for that detector. 

For additional information concerning individual parameter tabs, navigate to the [Help](help.qmd) page.

**About**

This dashboard contains the visualized QC data for the cytometers at the [%s](%s)


This dashboard was created with [Quarto](https://quarto.org/) using [CytometryQC](https://github.com/DavidRach/CytometryQC)

## MFI {.tabset}

## Gain {.tabset}

## rCV {.tabset}

', InstrumentName, organization, organization_website)
  
cat(Section1, SectionMFI, SectionGain, SectionRCV, SectionPDF, Section2, file = StorageLocation)

}
