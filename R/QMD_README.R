
#' Creates generic README.md file
#'
#' @param outpath The file.location to save the .qmd file to
#' @param organization_name internal
#' @param organization_website internal
#' 
#' @return A generic README.md file
#' 
#' @noRd
QMD_README <- function(outpath, organization_name, organization_website){
  
  StorageLocation <- file.path(outpath, "README.md")

  content <- sprintf('This repository contains the code for the InstrumentQC dashboard for the [%s](%s) cytometers. 

The dashboard track changes in MFI, Gain and %%RCV over time by processing .fcs files of QC beads acquired during QC in [R](https://www.r-project.org/) using the [Luciernaga](https://github.com/DavidRach/Luciernaga) package. 
The results are then turned into a website using [Quarto](https://quarto.org/) using functions found in the [CytometryQC](https://github.com/DavidRach/CytometryQC) package. All code is available under the AGPL3-0 copyleft license. Additional how-to-replicate-this-dashboard details can be found [here](https://github.com/DavidRach/InstrumentQC_Install)
', organization_name, organization_website)
  
  cat(content, file = StorageLocation)
}