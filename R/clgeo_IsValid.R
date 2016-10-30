# clgeo_IsValid.R
# -------------
# Author: Emmanuel Blondel <emmanuel.blondel1 at gmail.com>
# Created: 2016-10-30
#
#' @title clgeo_IsValid
#' 
#' @description
#' Wrapper method to try performing rgeos::gIsValid call and catch eventual 
#' warnings or errors (in particular GEOS exceptions).
#'
#' @author
#' Emmanuel Blondel \email{emmanuel.blondel1@@gmail.com}
#'
#' @param sp object extending the \code{\link[sp]{Spatial-class}} as defined in \pkg{sp}
#' @param verbose object of class "logical". Default value is FALSE.
#' @return an object of class "logical". TRUE if valid, FALSE otherwise
#'
#' @examples
#' \donttest{
#'  require(maptools)
#'  file <- system.file("extdata", "example.shp", package = "cleangeo")
#'  sp <- readShapePoly(file)
#'  clgeo_IsValid(sp)
#' }
#' 
#' @aliases clgeo_IsValid
#' 
#' @keywords geometry validity summary clean IsValid gIsValid
#' 
#'
clgeo_IsValid <- function(sp, verbose = FALSE){
  out <- tryCatch({
    out <- gIsValid(sp)
  },warning = function(msg){
    if(verbose) logger.info(sprintf("Catched RGEOS warning '%s'",msg))
    return(FALSE)
  },error = function(msg){
    if(verbose) logger.info(sprintf("Catched RGEOS error '%s'",msg))
    return(FALSE)
  })
  return(out)
}
