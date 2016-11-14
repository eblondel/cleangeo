# clgeo_Clean.R
# -------------
# Author: Emmanuel Blondel <emmanuel.blondel1 at gmail.com>
# Created: 2014-09-23
#
#' @title clgeo_Clean
#' 
#' @description
#' Function to clean a spatial data collection 
#'
#' @author
#' Emmanuel Blondel \email{emmanuel.blondel1@@gmail.com}
#'
#' @param sp object extending the \code{\link[sp]{Spatial-class}}
#' as defined in \pkg{sp}
#' @param errors.only an object of class \code{vector} giving the types of errors
#' for which the output should bounded. Default value is NULL (\emph{i.e.} the output
#' will include features for which both errors and errors were raised.). At now, this
#' argument accepts the error type \code{"ORPHANED_HOLE"}.
#' @param strategy advanced strategy to clean geometries. Default is "POLYGONATION",
#'        alternate value is "BUFFER" (old method).
#' @param verbose Indicates wether the clean logs have to be printed. Default 
#' value is FALSE.
#' @return an object extending the \code{\link[sp]{Spatial-class}}
#' as defined in \pkg{sp}, with cleaned geometries.
#'
#' @examples
#' \donttest{
#'  require(maptools)
#'  file <- system.file("extdata", "example.shp", package = "cleangeo")
#'  sp <- readShapePoly(file)
#'  
#'  sp.clean <- clgeo_Clean(sp)
#'  report.clean <- clgeo_CollectionReport(sp.clean)
#'  clgeo_SummaryReport(report.clean)
#' }
#' 
#' @aliases clgeo_Clean
#' 
#' @keywords geometry validity summary clean
#' 
#' @note About cleaning strategy:
#' The polygonation method is a tentative alternate method to triangulation to clean
#' geometries and to the classical often used 'buffer' approach. In the polygonation
#' method, triangulation is skipped and a re-polygonation intuitive algorithm is 
#' applied to rebuild the source invalid geometry into one or more valid polygonal
#' geometries.
#' 
#'
clgeo_Clean <- function(sp, errors.only = NULL,
                        strategy = "POLYGONATION",
                        verbose = FALSE){
  
  if(!(strategy %in% c("POLYGONATION", "BUFFER")))
    stop("Unknown advanced cleaning method. Accepted values: 'POLYGONATION', 'BUFFER'")
  
  report <- clgeo_CollectionReport(sp)
  nv <- clgeo_SuspiciousFeatures(report, errors.only)
  
  fixed.sp.list <- lapply(1:length(sp), function(x){
    polygon <- slot(sp, "polygons")[[x]]
    ID <- slot(polygon, "ID")
    if(!all(is.na(nv))){
      if(x %in% nv){          
        polygons <- slot(polygon, "Polygons")
        poly.nb <- length(polygons)
        removedHoles <- vector()
        
        if(poly.nb > 0){
          newpolygons <- list()
          for(i in 1:poly.nb){
            #if we found an orphaned hole, we remove it
            if(slot(polygons[[i]], "hole")){
              if(dim(unique(slot(polygons[[i]], "coords")))[1] < 3){
                
                if(length(removedHoles) == 0 & verbose){
                  logger.info(sprintf("Cleaning orphaned holes at index %s", x))
                }
                removedHoles <- c(removedHoles, i)
              }else{
                newpolygon <- polygons[[i]]
                slot(newpolygon, "hole") <- TRUE
                newpolygons <- c(newpolygons, newpolygon)
              }
            }else{
              newpolygon <- polygons[[i]]
              slot(newpolygon, "hole") <- FALSE
              newpolygons <- c(newpolygons, newpolygon)
            }
            
          }
          slot(polygon, "Polygons") <- newpolygons
        }
        polygon <- SpatialPolygons(Srl = list(polygon))
        
        #testing validity after removing holes
        isValid <- report[x,]$valid
        if(length(removedHoles) > 0){
          if(verbose){
            logger.info(sprintf("Checking geometry validity at index %s", x))
          }
          
          tryCatch({
            slot(polygon, "polygons") <<- lapply(slot(polygon, "polygons"), checkPolygonsHoles)
          }, warning = function(msg){
            if(verbose) logger.info(sprintf("Catched MAPTOOLS warning '%s'",msg))
          }, error = function(err){
            if(verbose) logger.info(sprintf("Catched MAPTOOLS error '%s'",err))
          })
          
          isValid <<- clgeo_IsValid(polygon, verbose)
        }
        
        #test clean geometry validity
        if(is.null(errors.only) & !isValid){
          if(verbose){
            report.msg <- NULL
            if(!is.na(report[x,"error_msg"])){
              report.msg <- report[x,"error_msg"]
            }else if(!is.na(report[x,"warning_msg"])){
              report.msg <- report[x,"warning_msg"]
            }
            logger.info(sprintf("Cleaning geometry at index %s (%s)", x, report.msg))
          }
          if(strategy == "POLYGONATION"){
            #run polygonation algorithm
            polygon <- clgeo_CleanByPolygonation.SpatialPolygons(polygon, verbose)
            
          }else if(strategy == "BUFFER"){
            #try applying buffer attempts
            attempt <- 1
  		      polygon <- gBuffer(polygon, id = ID, width = 0)
  		      while(attempt < 3){
  			      if(!clgeo_IsValid(polygon, verbose)){
  				      attempt <- attempt + 1
        				polygon <- gBuffer(polygon, id = ID, width = 0)
  			      }else{
  				      break;
  			      }
  		      }
          }
        }
        if(!is.null(polygon)){
          polygon <- polygon@polygons[[1]]
          slot(polygon, "ID") <- ID #index integrity
        }else{
          if(verbose){
            logger.info(sprintf("Removing false polygon at index %s", x))
          }
        }
      }
    }
    
    return(polygon)
  })
  
  if(!is.list(fixed.sp.list)) fixed.sp.list <- as.list(fixed.sp.list)
  fixed.sp.list <- fixed.sp.list[!sapply(fixed.sp.list, is.null)]
  
  fixed.sp <- NULL
  if(length(fixed.sp.list) > 0){
    fixed.sp <- SpatialPolygons(
      Srl = fixed.sp.list,
      proj4string = CRS(proj4string(sp))
    )
    
    if(class(sp) == "SpatialPolygonsDataFrame"){
      sp.df <- as(sp, "data.frame")
      ids <- sapply(slot(fixed.sp,"polygons"), slot, "ID")
      if(nrow(sp.df) != length(ids)){
        sp.df <- sp.df[ids,]
      }
      row.names(sp.df) <- ids
      fixed.sp <- SpatialPolygonsDataFrame(Sr = fixed.sp, data = sp.df)
    }
  }
  
  return(fixed.sp)
}
