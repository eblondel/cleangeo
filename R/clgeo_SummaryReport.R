# clgeo_SummaryReport.R
# ------------------------
# Author: Emmanuel Blondel <emmanuel.blondel1 at gmail.com>
# Created: 2014-09-24
#
#' @title clgeo_SummaryReport
#' 
#' @description
#' Function to get summary of a spatial data collection report returned by 
#' \code{\link{clgeo_CollectionReport}}  
#'
#' @author
#' Emmanuel Blondel \email{emmanuel.blondel1@@gmail.com}
#'
#' @param report a report object as returned by\code{\link{clgeo_CollectionReport}} 
#' @return an object of class \code{table} giving the report summary. The summary
#' gives the counting by value for each of the report columns:
#' \itemize{
#'   \item \emph{type} eventual \pkg{rgeos} issue
#'   \item \emph{valid} geometry validity status (according to OGC specifications)
#'   \item \emph{issue_type} type of geometry issue
#'   \item \emph{error_msg} catched message when error raised about geometry
#'   \item \emph{warning_msg} catched message when warning raised about geometry
#' }
#'
#' @examples
#'  require(maptools)
#'  file <- system.file("extdata", "example.shp", package = "cleangeo")
#'  sp <- readShapePoly(file)
#'  
#'  report <- clgeo_CollectionReport(sp)
#'  clgeo_SummaryReport(report)
#'  
#' @seealso \code{\link{clgeo_CollectionReport}}
#' 
#' @aliases clgeo_SummaryReport
#' 
#' @keywords geometry validity summary
#' 
#'
clgeo_SummaryReport <- function(report){
  return(summary(report[,-c(4,5)]))
}