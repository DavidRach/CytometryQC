#' Internal adds the necessary number of plotly MFI arguments the plots
#'  
#' @param uv Number of blue detectors
#' @param violet Number detectors
#' @param blue Number detectors
#' @param yellowgreen Number detectors
#' @param red Number detectors
#' 
#' @importFrom purrr map
#' 
#' @return Updated code chunk to add to the Instrument.qmd file
#' 
#' @noRd
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