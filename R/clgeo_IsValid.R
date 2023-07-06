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
#'  require(sf)
#'  file <- system.file("extdata", "example.shp", package = "cleangeo")
#'  sf <- sf::st_read(file)
#'  sp <- as(sf, "Spatial")
#'  clgeo_IsValid(sp)
#' }
#' 
#' @aliases clgeo_IsValid
#' 
#' @keywords geometry validity summary clean IsValid gIsValid
#' 
#'
clgeo_IsValid <- function(sp, verbose = FALSE){
  sfgeom = try(sf::st_as_sf(sp), silent = T)
  if(is(sfgeom, "try-error")) return(FALSE)
  out <- all(sf::st_is_valid(sfgeom))
  if(verbose){
    logger.info("Geometry validity:")
    print(sf::st_is_valid(sfgeom, reason = TRUE))
  }
  return(out)
}
