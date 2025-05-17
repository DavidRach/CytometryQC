#' Internal, ensures required packages are present
#' 
#' @importFrom Biobase exprs
#' @importFrom dplyr filter
#' @importFrom flowCore read.FCS
#' @importFrom flowWorkspace gs_pop_get_count_fast
#' @importFrom ggcyto geom_gate
#' @importFrom ggplot2 ggplot
#' @importFrom gt gt
#' @importFrom htmltools tags
#' @importFrom lubridate ymd
#' @importFrom plotly plot_ly
#' @importFrom tibble tibble
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect contains
#' @importFrom viridis viridis
#' 
#' @return Absolutely nothing
#' 
#' @noRd
RequiredPackages <- function(InternalCheck=FALSE){
  if (InternalCheck==TRUE){
    Biobase::exprs()
    dplyr::filter()
    flowCore::read.FCS()
    flowWorkspace::gs_pop_get_count_fast()
    ggcyto::geom_gate()
    ggplot2::ggplot()
    gt::gt()
    htmltools::tags()
    lubridate::ymd()
    plotly::plot_ly()
    tibble::tibble()
    tidyr::pivot_longer()
    tidyselect::contains()
    viridis::viridis()
  }
}
