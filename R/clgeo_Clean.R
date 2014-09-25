# clgeo_Clean.R
# -------------
# Author: Emmanuel Blondel <emmanuel.blondel1 at gmail.com>
# Created: 2014-09-23
#

clgeo_Clean <- function(sp, errors.only = NULL, print.log = TRUE){
  
  report <- clgeo_CollectionReport(sp)
  nv <- clgeo_SuspiciousFeatures(report, errors.only)

  fixed.sp <- SpatialPolygons(
    Srl = lapply(1:length(sp), function(x){
      feature <- slot(sp, "polygons")[[x]]
      ID <- slot(feature, "ID")
      if(!all(is.na(nv))){
        if(x %in% nv){          
          polygons <- slot(feature, "Polygons")
          poly.nb <- length(polygons)
          removedHoles <- 0
          for(i in 1:poly.nb){
            #if we found an orphaned hole, we remove it
            if(slot(polygons[[i]], "hole")
               & dim(unique(slot(polygons[[i]], "coords")))[1] < 3){
              
              if(removedHoles == 0 & print.log){
                print(paste("Cleaning orphaned holes at index ", x, sep=""))
              }
              
              slt <- slot(feature, "Polygons")				
              slt[[i - removedHoles]] <- NULL
              removedHoles <- removedHoles + 1
              slot(feature, "Polygons") <- slt
            }
          }
          
          feature <- SpatialPolygons(Srl = list(feature), pO = 1L,
                                     proj4string = CRS(proj4string(sp)))
          
          #testing validity after removing holes
          isValid <- report[x,]$valid
          if(removedHoles > 0){
            if(print.log){
              print(paste("Checking geometry validity at index ", x, sep=""))
            }
              
            slot(feature, "polygons") <- lapply(slot(feature, "polygons"),
                                                checkPolygonsHoles)
            isValid <- gIsValid(feature)
          }
            
          #test clean geometry validity
          if(is.null(errors.only) & !isValid){
            if(print.log){
              print(paste("Cleaning geometry at index ", x, sep=""))
            }
            feature <- gBuffer(feature, id = ID, width = 0)
          }
          feature <- feature@polygons[[1]]
        }
      }
      return(feature)
    }),
    pO = 1:length(sp),
    proj4string = CRS(proj4string(sp))
  )
  
  if(class(sp) == "SpatialPolygonsDataFrame"){
    fixed.sp <- SpatialPolygonsDataFrame(Sr = fixed.sp,
                                         data = as(sp, "data.frame"))
  }
  
  return(fixed.sp)
}
