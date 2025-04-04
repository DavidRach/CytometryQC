#' Creates the 404.qmd file
#'
#' @param outpath The file.location to save the .qmd file to
#' 
#' @return A 404.qmd file
#' 
#' @export
QMD_404 <- function(outpath, organization="UMGCC FCSS Instrument",
 github_page="umgccfcss.github.io"){
  
StorageLocation <- file.path(outpath, "404.qmd")
Homepage <- paste0("https://", github_page, "/InstrumentQC")
  
content <- sprintf(
'---
title: "Page not found"
format: html
---

Sorry, the page you are looking for is no longer there.

To access the %s QC dashboard, please click here to go to the [homepage](%s)
', organization, Homepage)

cat(content, file = StorageLocation)  
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
