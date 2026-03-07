#' Small internal for yml assembly in historical instrument pages
#' 
#' @return yml text with year inserted. 
#' 
#' @noRd
YearAppend <- function(x){
  String4 <- '
      - text: "Placeholder"
        href: YearPlaceholder.qmd'

  String4 <- gsub("Placeholder", x, String4)
  String4 <- unlist(strsplit(String4, "\n"))
  return(String4)
}