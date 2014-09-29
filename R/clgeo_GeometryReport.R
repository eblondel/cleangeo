# clgeo_GeometryReport.R
# ----------------------
# Author: Emmanuel Blondel <emmanuel.blondel1 at gmail.com>
# Created: 2014-09-23
#
#' @title clgeo_GeometryReport
#' 
#' @description
#' Function to get a geometry validation report: The report informs on the following:
#'
#' \itemize{
#'   \item \emph{type} eventual \pkg{rgeos} issue
#'   \item \emph{valid} geometry validity status (according to OGC specifications)
#'   \item \emph{issue_type} type of geometry issue
#'   \item \emph{error_msg} catched message when error raised about geometry
#'   \item \emph{warning_msg} catched message when warning raised about geometry
#' }  
#'
#' @author
#' Emmanuel Blondel \email{emmanuel.blondel1@@gmail.com}
#'
#' @param spgeom object extending the \code{\link[sp]{Spatial-class}} as defined in \pkg{sp}
#' @return an object of class \code{list} giving the following:
#' \itemize{
#'   \item \emph{type} eventual \pkg{rgeos} issue
#'   \item \emph{valid} geometry validity status (according to OGC specifications)
#'   \item \emph{issue_type} type of geometry issue
#'   \item \emph{error_msg} catched message when error raised about geometry
#'   \item \emph{warning_msg} catched message when warning raised about geometry
#' }
#'
#' @seealso \code{\link[rgeos]{gIsValid}}
#' 
#' @aliases clgeo_GeometryReport clgeo_Geometry
#' 
#' @keywords geometry validity
#' 
#' 
clgeo_GeometryReport <- function(spgeom){
  
  clgeo_report <- list(type = NA, valid = FALSE, issue_type = NA,
                       error_msg = NA, warning_msg = NA)
  
  report <- tryCatch({
    isvalid <- gIsValid(spgeom)
    if(isvalid) clgeo_report$valid <- TRUE
    return(clgeo_report)
    
  },warning = function(w){
    clgeo_report$type <- "rgeos_validity"
    clgeo_report$valid <- FALSE
    if(regexpr("at or near point", conditionMessage(w), "match.length",
               ignore.case = TRUE) > 1) clgeo_report$issue_type = "GEOM_VALIDITY"
    clgeo_report$warning_msg <- conditionMessage(w)
    return(clgeo_report)
    
  },error = function(e){
    clgeo_report$type <- "rgeos_error"
    clgeo_report$valid <- FALSE
    if(regexpr("orphaned hole", conditionMessage(e), "match.length",
               ignore.case = TRUE) > 1) clgeo_report$issue_type = "ORPHANED_HOLE"
    clgeo_report$error_msg = conditionMessage(e)
    return(clgeo_report)
  })
  
  return(report)
}