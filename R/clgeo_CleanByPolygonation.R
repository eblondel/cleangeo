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
#' @param verbose Indicates wether the clean logs have to be printed. Default value is FALSE.
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
clgeo_CleanByPolygonation.Polygon <- function(p, verbose = FALSE){
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
    colnames(xy) <- c("x","y")
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
          sfi = sf::st_as_sf(splines[[i]])
          sfj = sf::st_as_sf(splines[[j]])
          if(sf::st_crosses(sfi, sfj, sparse = FALSE)){
            int <- sf::st_intersection(sfi, sfj)
            intsp <- as(int, "Spatial")
            if(is(intsp, "SpatialPoints")){
              enrichLine(i, intsp@coords)
              enrichLine(j, intsp@coords)
            }
          }
        }
      })
    })
  )
  
  #getAllCoords
  getAllCoords <- function(){
    out <- do.call("rbind",
                   lapply(spNewLines,
                          function(i){
                            l.coords <- slot(slot(i,"lines")[[1]],"Lines")[[1]]@coords
                            l.coords<- l.coords[1:(nrow(l.coords)-1),]
                            if(!inherits(l.coords, "matrix")){
                              l.coords <- data.frame(
                                x = l.coords[1],
                                y = l.coords[2],
                                intersect = FALSE,
                                linestart = TRUE,
                                stringsAsFactors = FALSE
                              )
                            }else{
                              l.coords <- data.frame(
                                x = l.coords[,1],
                                y = l.coords[,2],
                                intersect = c(FALSE,rep(TRUE,nrow(l.coords)-1)),
                                linestart = c(TRUE,rep(FALSE,nrow(l.coords)-1)),
                                stringsAsFactors = FALSE
                              )
                            }
                            return(l.coords)
                          }))
    row.names(out) <- 1:nrow(out)
    return(out)
  }
  
  
  #sequence all coords but with lapply, starting with coords of the line after
  cc <- getAllCoords()
  ncoords <- which(cc$intersect)
  if(length(ncoords) > 0){
    polygons <- lapply(1:length(ncoords),function(i){
      coords <- NULL
      if(i<length(ncoords)){
        coords <- cc[ncoords[i]:ncoords[i+1],1:2]
      }else{
        coords <- rbind(cc[ncoords[i]:nrow(cc),1:2],cc[1:ncoords[1],1:2])
      }
      out <- NULL
      if(!is.null(coords)) out <- Polygon(coords, hole=FALSE)
      return(out)
    })
  }else{
    polygons <- list(Polygon(cc[,1:2],hole=FALSE))
  }
  polygons <- polygons[!sapply(polygons,is.null)]
  polygons <- polygons[sapply(polygons, function(x){return(slot(x,"area") > 1e-08)})] #equiv to rgeos::getScale() in sf?
  
  #manage dangling edges (TODO investigate an easier way)
  if(length(polygons)>0){
    temp.poly <- SpatialPolygons(Srl=list(Polygons(srl=polygons,ID="1")))
    temp.polygon.sf <- sf::st_buffer(sf::st_as_sf(temp.poly), dist=0)
    temp.polygon = as(temp.polygon.sf, "Spatial")
    if(is.null(temp.polygon)){
      polygons <- NULL
    }else{
      polygons <- temp.polygon@polygons[[1]]@Polygons
    }
  }
  
  #in case polygons is empty list return null
  if(!is.null(polygons) & length(polygons) == 0) polygons <- NULL
  
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
#' @param verbose Indicates wether the clean logs have to be printed. Default value is FALSE.
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
clgeo_CleanByPolygonation.Polygons <- function(p, verbose = FALSE){
  
  #not holes
  polygons <- slot(p, "Polygons")[sapply(slot(p,"Polygons"),
                                         function(x){
                                           return(!slot(x,"hole"))
                                         })]
  
  ID <- 1
  new.polygons <- lapply(1:length(polygons), function(i){
    
    out <- NULL
    polygon <- polygons[[i]]
    tempsp <- SpatialPolygons(Srl = list(Polygons(srl=list(polygon),ID="1")))
    po <- polygon
    
    isValid <- clgeo_IsValid(tempsp, verbose)
    if(!isValid){
      po <- clgeo_CleanByPolygonation.Polygon(polygon, verbose)
    }
    if(!is.null(po)){
      if(!is.list(po)) po <- list(po)
      po <- po[!sapply(po,is.null)]  
      po<- lapply(po, function(x){
        outpo <- x
        if(nrow(unique(slot(x,"coords"))) < 3) outpo <- NULL
        return(outpo)
      })
      po <- po[!sapply(po,is.null)]
      
  
      if(!is.null(po) && length(po) > 0){
        poly <- Polygons(srl = po, ID = as.character(ID))
        if(slot(poly, "area") > 0) out <- poly
        if(!is.null(out)){
          if(slot(poly,"area") >= 1e-8){
            ID <<- ID + 1
          }else{
            out <- NULL
          }
        }
      }
    }
    return(out)
  })
  new.polygons <- new.polygons[!sapply(new.polygons,is.null)]
  if(length(new.polygons)==0) return(new.polygons)
  trsp <- SpatialPolygons(Srl = new.polygons)
  trsf = sf::st_as_sf(trsp)
  trsp <- as(sf::st_union(trsf, by_feature = TRUE), "Spatial")
  trsp <- SpatialPolygons(Srl = list(Polygons(srl = unlist(lapply(trsp@polygons, slot, "Polygons")), ID = "1")))
  
  #holes
  holes <- slot(p, "Polygons")[sapply(slot(p,"Polygons"), slot, "hole")]
  if(length(holes) > 0){
    holes <- lapply(holes, function(x){
      slot(x,"hole") <- FALSE
      slot(x,"ringDir") <- 1L
      return(x)
    })
    
    new.holes <- lapply(1:length(holes), function(i){
      hole <- holes[[i]]
      temphole <- SpatialPolygons(Srl = list(Polygons(srl=list(hole),ID="1")))
      po <- hole
      
      isValid <- clgeo_IsValid(temphole, verbose)
      if(!isValid){
        po <- clgeo_CleanByPolygonation.Polygon(hole, verbose)
      }
      if(!is.list(po)) po <- list(po)
      po <- po[!sapply(po,is.null)]
      po<- lapply(po, function(x){
        outpo <- x
        coords <- slot(x,"coords")
        coords[,1] <- as.character(coords[,1])
        coords[,2] <- as.character(coords[,2])
        if(nrow(unique(slot(x,"coords"))) < 3) outpo <- NULL
        return(outpo)
      })
      po <- po[!sapply(po,is.null)]
      
      out <- NULL
      if(!is.null(po) && length(po) > 0){
        if(!is.list(po)) po <- list(po)
        polyholes <- Polygons(srl = po, ID = as.character(ID))
        if(slot(polyholes, "area") > 0) out <- polyholes
        if(!is.null(out)){
          if(slot(polyholes,"area") >= 1e-8){
            ID <<- ID + 1
          }else{  
            out <- NULL
          }
        }
      }
      return(out)
    })
    new.holes <- new.holes[!sapply(new.holes,is.null)]
    if(length(new.holes)>0){
      trspholes <- SpatialPolygons(Srl = new.holes)
      trsfholes = sf::st_as_sf(trspholes)
      trspholes <- as(sf::st_union(trsfholes, by_feature = TRUE), "Spatial")
      trspholes <- SpatialPolygons(Srl = list(Polygons(srl = unlist(lapply(trspholes@polygons, slot, "Polygons")), ID = "1")))
      #slot(trspholes, "polygons") <- lapply(slot(trspholes, "polygons"), checkPolygonsHoles)
    
      #difference
      sfout <- sf::st_difference(sf::st_as_sf(trsp), sf::st_as_sf(trspholes))
      spout <- as(sfout, "Spatial")
      spout <- SpatialPolygons(Srl = list(Polygons(srl = unlist(lapply(spout@polygons, slot, "Polygons")), ID = "1")))
    }else{
      spout <- trsp
    } 
   
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
#' @param verbose Indicates wether the clean logs have to be printed. Default value is FALSE.
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
clgeo_CleanByPolygonation.SpatialPolygons <- function(sp, verbose = FALSE){
  spout <- NULL
  polygons <- unlist(lapply(1:length(sp@polygons), function(i){
    polygons = sp@polygons[[i]]
    newpolygons = clgeo_CleanByPolygonation.Polygons(polygons, verbose)
    newpolygons[[1]]@ID <- as.character(i)
    return(newpolygons)
  }))
  if(!is.null(polygons)) spout <- SpatialPolygons(Srl = polygons)
  return(spout)
}
