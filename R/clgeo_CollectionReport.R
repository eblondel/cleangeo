# clgeo_CollectionReport.R
# ------------------------
# Author: Emmanuel Blondel <emmanuel.blondel1 at gmail.com>
# Created: 2014-09-23
#
#' @title clgeo_CollectionReport
#' 
#' @description
#' Function to get a spatial data collection validation report. The function outputs
#' a \code{data.frame} binding all geometry validity reports, each one produced by 
#' \code{\link{clgeo_GeometryReport}}  
#'
#' @author
#' Emmanuel Blondel \email{emmanuel.blondel1@@gmail.com}
#'
#' @param sp object extending the \code{\link[sp]{Spatial-class}} as defined in \pkg{sp}
#' @return an object of class \code{data.frame} with the following columns:
#' \itemize{
#'   \item \emph{type} eventual \pkg{rgeos} issue
#'   \item \emph{valid} geometry validity status (according to OGC specifications)
#'   \item \emph{issue_type} type of geometry issue
#'   \item \emph{error_msg} catched message when error raised about geometry
#'   \item \emph{warning_msg} catched message when warning raised about geometry
#' }
#'
#' @seealso \code{\link{clgeo_GeometryReport}}
#' 
#' @aliases clgeo_CollectionReport
#' 
#' @keywords geometry validity
#' 
#' 
clgeo_CollectionReport <- function(sp){
  
  clgeo_report <- as.data.frame(do.call("rbind", lapply(1:length(sp), function(x){
    report <- unlist(clgeo_GeometryReport(sp[x,]))
  })), stringsAsFactors = FALSE)
  clgeo_report$valid <- as(clgeo_report$valid, "logical")
  clgeo_report$type <- as.factor(clgeo_report$type)
  clgeo_report$issue_type <- as.factor(clgeo_report$issue_type)
  return(clgeo_report)
}
