# clgeo_SuspiciousFeatures.R
# --------------------------
# Author: Emmanuel Blondel <emmanuel.blondel1 at gmail.com>
# Created: 2014-09-23
#
#' @title clgeo_SuspiciousFeatures
#' 
#' @description
#' Function to get the list of index of suspicious geometries within a spatial data
#' collection, given a spatial data collection report returned by  the function 
#' \code{\link{clgeo_CollectionReport}}  
#'
#' @author
#' Emmanuel Blondel \email{emmanuel.blondel1@@gmail.com}
#'
#' @param report a report object as returned by\code{\link{clgeo_CollectionReport}}
#' @param errors.only an object of class \code{vector} giving the types of errors
#' for which the output should bounded. Default value is NULL (\emph{i.e.} the output
#' will include features for which both errors and errors were raised.). At now, this
#' argument only accepts the error type \code{"ORPHANED_HOLE"}.
#' @return an object of class \code{vector} giving the numeric indexes of spatial
#' objects tagged as suspicious (\emph{i.e.} that are not valid acccording to OGC
#' specifications)
#'
#' @examples
#'  require(maptools)
#'  file <- system.file("extdata", "example.shp", package = "cleangeo")
#'  sp <- readShapePoly(file)
#'  
#'  report <- clgeo_CollectionReport(sp)
#'  nv <- clgeo_SuspiciousFeatures(report)
#'
#' @seealso \code{\link{clgeo_CollectionReport}}
#' 
#' @aliases clgeo_SuspiciousFeatures
#' 
#' @keywords geometry validity
#' 
#'
clgeo_SuspiciousFeatures <- function(report, errors.only = NULL){
  
  features <- sapply(1:nrow(report), function(x){
    idx <- NA
    if(!as(report[x,]$valid, "logical")) idx <- x
    if(!is.null(errors.only)){
      idx <- NA
      error <- as(report[x,]$issue_type, "character")
      if(error %in% errors.only) idx <- x
    }
    return(idx)
  })
  
  features <- features[!is.na(features)]
  if(length(features) == 0) features <- NA
  
  return(features)
}