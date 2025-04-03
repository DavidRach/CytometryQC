#' Creates a 404.qmd file
#'
#' @param outpath The file.location to save the .qmd file to
#' 
#' @return A 404.qmd file
#' 
#' @noRd
QMD_404 <- function(outpath){
  
StorageLocation <- file.path(outpath, "404.qmd")
  
cat(sprintf(
'---
title: "Some title"
date: 09/01/2014
---

## Important links

This is my test of how this works

## DAG

```{r}
print("Hello")
```
'), file = StorageLocation)
  
  
}
