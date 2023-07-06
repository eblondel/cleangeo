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
#'   \item \emph{msg} catched message when error raised about geometry
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
#'   \item \emph{msg} catched message when warning raised about geometry
#' }
#' 
#' @aliases clgeo_GeometryReport clgeo_Geometry
#' 
#' @keywords geometry validity
#' 
#' 
clgeo_GeometryReport <- function(spgeom){
  
  clgeo_report <- list(type = NA, valid = FALSE, issue_type = NA, msg = NA)
  
  sfgeom = sf::st_as_sf(spgeom)
  
  isvalid <- sf::st_is_valid(sfgeom)
  isvalidreason = sf::st_is_valid(sfgeom, reason = TRUE)
  if(isvalid){
    clgeo_report$valid <- TRUE
  }
  
  if(isvalidreason != "Valid Geometry"){
    clgeo_report$issue_type = "GEOM_VALIDITY"
    clgeo_report$type <- "geos_error_validity"
    clgeo_report$valid <- FALSE
    clgeo_report$msg <- isvalidreason
  }
    
  return(clgeo_report)
}