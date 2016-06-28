# clgeo_CleanByPolygonation.R
# -------------
# Author: Emmanuel Blondel <emmanuel.blondel1 at gmail.com>
# Created: 2016-06-28
#
#' @title clgeo_CleanByPolygonation.Polygon
#' 
#' @description
#' Function to clean a \code{\link[sp]{Polygon-class}} object by polygonation.
#'
#' @author
#' Emmanuel Blondel \email{emmanuel.blondel1@@gmail.com}
#'
#' @param p object of class \code{\link[sp]{Polygon-class}} as defined in \pkg{sp}
#' @return a list of objects of class \code{\link[sp]{Polygon-class}} as defined in \pkg{sp},
#' with cleaned geometries.
#'
#' @aliases clgeo_CleanByPolygonation.Polygon
#' 
#' @keywords geometry validity summary clean
#' @note The polygonation method is a tentative alternate method to triangulation to clean
#' geometries. In this method, triangulation is skipped and a re-polygonation algorithm is
#' applied.
#'
#' 
clgeo_CleanByPolygonation.Polygon <- function(p){
  pts <-  as.data.frame(p@coords)
  linesList <- lapply(1:(nrow(pts)-1),function(i){Line(pts[i:(i+1),])})
  
  #create splines
  splines <- lapply(
    1:length(linesList),
    function(x){
      spline <- SpatialLines(list(Lines(slinelist = list(linesList[[x]]), ID = x)))
      return(spline)
    }
  )
  
  #find Steiner points
  #===================
  spNewLines <- splines
  
  #function to enrich line from spNewLines object
  enrichLine <- function(idx, coords){
    xy <- slot(slot(spNewLines[[idx]],"lines")[[1]],"Lines")[[1]]@coords
    xy <- unique(as.data.frame(rbind(xy, coords)))
    if(xy[1,1] < xy[2,1] && xy[1,2] < xy[2,2]) xy <- xy[with(xy,order(x,y)),]
    if(xy[1,1] < xy[2,1] && xy[1,2] > xy[2,2]) xy <- xy[with(xy,order(x,-y)),]
    if(xy[1,1] > xy[2,1] && xy[1,2] < xy[2,2]) xy <- xy[with(xy,order(-x,y)),]
    if(xy[1,1] > xy[2,1] && xy[1,2] > xy[2,2]) xy <- xy[with(xy,order(-x,-y)),]
    row.names(xy) <- 1:nrow(xy)
    xy <- as.matrix(xy)
    slot(slot(spNewLines[[idx]],"lines")[[1]],"Lines")[[1]]@coords <<- xy
  }
  
  #try enrich lines
  invisible(
    lapply(1:length(splines),function(i){
      lapply(1:length(splines),function(j){
        if(i != j){
          if(gCrosses(splines[[i]],splines[[j]])){
            int <- gIntersection(splines[[i]],splines[[j]])
            if(class(int) == "SpatialPoints"){
              enrichLine(i, int@coords)
              enrichLine(j, int@coords)
            }
          }
        }
      })
    })
  )
  
  #all
  #sequence all coords but with lapply, starting with coords of the line after
  all.coords <- lapply(2:length(spNewLines),function(i){
    line.seq <- 1:length(spNewLines)
    start <- which(line.seq == i)
    line.seq <- c(start:max(line.seq))
    if(start != 1) line.seq <- c(line.seq,1:(start-1))
    out <- do.call("rbind",
      lapply(line.seq,
       function(x){
          l.coords <- slot(slot(spNewLines[[x]],"lines")[[1]],"Lines")[[1]]@coords
          l.coords<- l.coords[1:(nrow(l.coords)-1),]
          if(class(l.coords) != "matrix"){
            l.coords <- data.frame(
              x = l.coords[1],
              y = l.coords[2],
              intersect = FALSE,
              stringsAsFactors = FALSE
            )
          }else{
            l.coords <- data.frame(
              x = l.coords[,1],
              y = l.coords[,2],
              intersect = c(FALSE,rep(TRUE,nrow(l.coords)-1)),
              stringsAsFactors = FALSE
            )
          }
          return(l.coords)
       }))
    return(out)
  })
  
  #try create polygons (skipping raw triangulation)
  pol.pts <- NULL
  polygons <- lapply(2:length(spNewLines),function(i){
    
    #start coords
    before <- i-1
    lineBefore <- spNewLines[[before]]
    start.coords <- tail(slot(slot(lineBefore,"lines")[[1]],"Lines")[[1]]@coords,n = 2)[1,]
    
    #end coords
    allcoords <- all.coords[[i-1]]
    ncoords <- which(allcoords$intersect)
    if(length(ncoords) > 0){
      end.coords <- allcoords[1:(ncoords[1]),1:2]
    }else{
      end.coords <- allcoords[,1:2]
    }
    
    #polygon
    pol <- Polygon(rbind(start.coords, end.coords), hole = FALSE)
    cen <- as.character(slot(pol, "labpt"))
    out.pol <- NULL
    if(!all(cen %in% pol.pts)){
      pol.pts <<- rbind(pol.pts, cen)
      out.pol <- pol
    }
    return(out.pol)
  })
  polygons <- polygons[!sapply(polygons,is.null)]
  return(polygons)
}

#' @title clgeo_CleanByPolygonation.Polygons
#' 
#' @description
#' Function to clean a \code{\link[sp]{Polygons}} object by polygonation
#'
#' @author
#' Emmanuel Blondel \email{emmanuel.blondel1@@gmail.com}
#'
#' @param p object of class \code{\link[sp]{Polygons-class}} as defined in \pkg{sp}
#' @return an object of class \code{\link[sp]{Polygons-class}} as defined in \pkg{sp},
#' with cleaned geometries.
#'
#' @aliases clgeo_CleanByPolygonation.Polygons
#' 
#' @keywords geometry validity summary clean
#' @note The polygonation method is a tentative alternate method to triangulation to clean
#' geometries. In this method, triangulation is skipped and a re-polygonation algorithm is
#' applied.
#' 
clgeo_CleanByPolygonation.Polygons <- function(p){
  
  #not holes
  polygons <- slot(p, "Polygons")[sapply(slot(p,"Polygons"),
                                         function(x){
                                           return(!slot(x,"hole"))
                                         })]
  
  trsp <- SpatialPolygons(Srl = lapply(1:length(polygons), function(i){
    po <- clgeo_CleanByPolygonation.Polygon(polygons[[i]])
    
    if(!is.list(po)) po <- list(po)
    poly <- Polygons(srl = po, ID = as.character(i))
    return(poly)
  }))
  trsp <- gUnionCascaded(trsp, sapply(trsp@polygons, slot, "ID"))
  trsp <- SpatialPolygons(Srl = list(Polygons(srl = unlist(lapply(trsp@polygons, slot, "Polygons")), ID = "1")))
  
  #holes
  holes <- slot(p, "Polygons")[sapply(slot(p,"Polygons"), slot, "hole")]
  if(length(holes) > 0){
    holes <- lapply(holes, function(x){
      slot(x,"hole") <- FALSE
      slot(x,"ringDir") <- 1L
      return(x)
    })
    
    trspholes <- SpatialPolygons(Srl = lapply(1:length(holes), function(i){
      po <- clgeo_CleanByPolygonation.Polygon(holes[[i]])
      if(!is.list(po)) po <- list(po)
      polyholes <- Polygons(srl = po, ID = as.character(i))
      return(polyholes)
    }))
    trspholes <- gUnionCascaded(trspholes, sapply(trspholes@polygons, slot, "ID"))
    trspholes <- SpatialPolygons(Srl = list(Polygons(srl = unlist(lapply(trspholes@polygons, slot, "Polygons")), ID = "1")))
    slot(trspholes, "polygons") <- lapply(slot(trspholes, "polygons"), checkPolygonsHoles)
    
    #difference
    spout <- gDifference(trsp, trspholes)
  }else{
    spout <- trsp
  }
  return(spout@polygons)
  
}

#' @title clgeo_CleanByPolygonation.SpatialPolygons
#' 
#' @description
#' Function to clean a \code{\link[sp]{SpatialPolygons}} object by polygonation
#'
#' @author
#' Emmanuel Blondel \email{emmanuel.blondel1@@gmail.com}
#'
#' @param sp object extending the \code{\link[sp]{Spatial-class}} as defined in \pkg{sp}
#' @return an object extending the \code{\link[sp]{Spatial-class}} as defined in \pkg{sp},
#' with cleaned geometries.
#'
#' @aliases clgeo_CleanByTriangulation
#' 
#' @keywords geometry validity summary clean
#' @note The polygonation method is a tentative alternate method to triangulation to clean
#' geometries. In this method, triangulation is skipped and a re-polygonation algorithm is
#' applied.
#' 
clgeo_CleanByPolygonation.SpatialPolygons <- function(sp){
  polygons <- unlist(lapply(sp@polygons, clgeo_CleanByPolygonation.Polygons))
  spout <- SpatialPolygons(Srl = polygons)
  return(spout)
}
