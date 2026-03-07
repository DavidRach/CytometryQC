

#' Creates a HistoricalInstument.qmd placeholder
#'
#' @param outpath The location file is saved to, default is InstrumentQC
#' @param name See \code{\link{AddInstruments}}
#' @param manufacturer See \code{\link{AddInstruments}}
#' @param organization_name See \code{\link{AddInstruments}}
#' @param organization_website See \code{\link{AddInstruments}}
#' 
#' @return A HistoricalInstument.qmd placeholder
#' 
#' @noRd
QMD_HistoricalInstrument <- function(outpath, name, manufacturer,
   organization_name, organization_website){
  
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
', organization_name, organization_website)
  
Section3 <- '
## First Row {height="50%"}

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