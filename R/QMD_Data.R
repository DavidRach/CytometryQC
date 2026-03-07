
#' Creates generic Data.qmd file
#'
#' @param outpath The location to save the file to, default is InstrumentQC folder
#' @param organization_name Name of the organization, ex. UMGCC FCSS
#' @param organization_website The organizations website
#' 
#' @return A Data.qmd placeholder
#' 
#' @noRd
QMD_Data <- function(outpath, organization_name,
organization_website){

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

**MFI** A .csv file containing Gain and MFI information derrived from Daily QC .fcs files. 

**Gain and MFI** A .csv file containing Gain and MFI information derrived from Daily QC .fcs files.

**Plots** A .pdf file containing non-interactive version of all the plots for each instrument.

For additional information, navigate to the [Help](help.qmd) page.

**About**

'

Section2 <- sprintf('
This dashboard contains the visualized QC data for the cytometers at the [%s](%s)

This dashboard was created with [Quarto](https://quarto.org/) and was created with [CytometryQC](https://github.com/DavidRach/CytometryQC)


', organization_name, organization_website)
  
Section3 <- '
## First Row {height="50%"}


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