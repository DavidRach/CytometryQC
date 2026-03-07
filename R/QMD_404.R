#' Creates the 404.qmd file
#'
#' @param outpath File storage location, by default the Instrument QC folder
#' @param organization_name Name of the organization, ex. UMGCC FCSS
#' @param githubusername GitHub user name, ex. umgccfcss.
#' @param FolderName Passed from FolderSetup 
#' 
#' @return A 404.qmd file
#' 
#' @noRd
QMD_404 <- function(outpath, organization_name, githubusername,
FolderName){
  
StorageLocation <- file.path(outpath, "404.qmd")
Homepage <- paste0("https://", githubusername, ".github.io/", FolderName, "/")
  
content <- sprintf(
'---
title: Page Not Found
---

Sorry, the page you are looking for is no longer there.

To access the %s  %s dashboard, please click here to go to the [homepage](%s)
', organization_name, FolderName, Homepage)

cat(content, file = StorageLocation)  
}
