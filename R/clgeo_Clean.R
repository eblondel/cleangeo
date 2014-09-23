# clgeo_Clean.R
# -------------
# Author: Emmanuel Blondel <emmanuel.blondel1 at gmail.com>
# Created: 2014-09-23
#

clgeo_Clean <- function(sp, errors.only = NULL){
  
  report <- clgeo_CollectionReport(sp)
  nv <- clgeo_SuspiciousFeatures(report, errors.only)
  
  fixed.sp <- SpatialPolygons(
    lapply(1:length(sp), function(x){
      feature <- sp@polygons[[x]]
      if(!is.na(nv)){
        if(x %in% nv){
          polygons <- feature@Polygons
          poly.nb <- length(polygons)
          print(length(slot(feature, "Polygons")))
          removedHoles <- 0
          for(i in 1:poly.nb){
            #if we found an orphaned hole, we remove it
            if(slot(polygons[[i]], "hole")
               & dim(unique(slot(polygons[[i]], "coords")))[1] < 3){
              
              slt <- slot(feature, "Polygons")				
              slt[[i - removedHoles]] <- NULL
              removedHoles <- removedHoles + 1
              slot(feature, "Polygons") <- slt
            }
          }
          print(length(slot(feature, "Polygons")))
          
          #test clean geometry validty
          if(is.null(errors.only) & !as(report[x,]$valid, "logical")){
            feature <- gBuffer(feature, byid = TRUE, width = 0)
          }
        }
      }
      return(feature)
    }),
    1:length(sp)
  )
  
  if(class(sp) == "SpatialPolygonsDataFrame"){
    fixed.sp <- SpatialPolygonsDataFrame(Sr = fixed.sp,
                                         data = as(sp, "data.frame"))
  }
  
  return(fixed.sp)
}
