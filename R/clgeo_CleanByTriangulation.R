# clgeo_CleanByTriangulation.R
# -------------
# Author: Emmanuel Blondel <emmanuel.blondel1 at gmail.com>
# Created: 2016-03-02
#

#slight modification of RTriangle pslg function (no fatal error / stop in case 
#of duplicated vertices)
pslg2 = function (P, PB = NA, PA = NA, S = NA, SB = NA, H = NA) 
{
  P <- as.matrix(P)
  PB <- as.integer(PB)
  PA <- as.matrix(PA)
  S <- as.matrix(S)
  SB <- as.integer(SB)
  H <- as.matrix(H)
  check.na.nan <- function(x) {
    if (!is.null(x)) {
      if (any(is.nan(x))) {
        stop(paste("NaN in", deparse(substitute(x))))
      }
      if (any(is.na(x))) {
        stop(paste("NA in", deparse(substitute(x))))
      }
    }
  }
  check.na.nan(P)
  if (ncol(P) != 2) {
    stop("Matrix of vertices P should have 2 columns")
  }
  
  if (any(is.na(PA))) {
    PA <- matrix(0, nrow(P), 0)
  }
  if (nrow(PA) != nrow(P)) {
    stop("Point attribute matrix PA does not have same number of rows the point matrix P")
  }
  if (is.na(PB)) {
    PB <- 0
  }
  PB <- rep(PB, length.out = nrow(P))
  if (any(is.na(S))) {
    S <- matrix(0, 0, 2)
  }
  else {
    if (ncol(S) != 2) {
      stop("Matrix of segments S should have 2 columns")
    }
  }
  if (any(is.na(SB))) {
    SB <- 0
  }
  SB <- rep(SB, length.out = nrow(S))
  if (any(is.na(H))) {
    H <- matrix(0, 0, 2)
  }
  storage.mode(P) <- "double"
  storage.mode(PA) <- "double"
  storage.mode(PB) <- "integer"
  storage.mode(S) <- "integer"
  storage.mode(SB) <- "integer"
  storage.mode(H) <- "double"
  ret <- list(P = P, PA = PA, PB = PB, S = S, SB = SB, H = H)
  class(ret) <- "pslg"
  return(ret)
}


#' @title clgeo_CleanByTriangulation.Polygon
#' 
#' @description
#' Function to clean a \code{\link[sp]{Polygon-class}} object by triangulation
#'
#' @author
#' Emmanuel Blondel \email{emmanuel.blondel1@@gmail.com}
#'
#' @param p object of class \code{\link[sp]{Polygon-class}} as defined in \pkg{sp}
#' @return a list of objects of class \code{\link[sp]{Polygon-class}} as defined in \pkg{sp},
#' with cleaned geometries.
#'
#' @aliases clgeo_CleanByTriangulation.Polygon
#' 
#' @keywords geometry validity summary clean
#' 
clgeo_CleanByTriangulation.Polygon <- function(p){
  
  points <-  cbind(p@coords)
  #points <- unique(points)
  
  if(nrow(points) < 3) return(NULL)

  SList <- do.call("rbind",
                   lapply(1:nrow(points),
                          function(x){
                            pair <- NULL
                            if(x < nrow(points)){
                              pair <- c(x,x+1)  
                            }else{
                              pair <- c(nrow(points),1)
                            }
                          }))

  pp <- pslg2(P=points[,1:2],S=SList)
  tp <- RTriangle:::triangulate(pp);
  triangles <- tp$T
  sp.list <- lapply(1:nrow(triangles),
                     function(x){
                       tr = triangles[x,]
                       out <- Polygon(tp$P[c(tr[1],tr[2],tr[3]),],hole=FALSE)
                       return(out)
                     })
  return(sp.list)
}

#' @title clgeo_CleanByTriangulation.Polygons
#' 
#' @description
#' Function to clean a \code{\link[sp]{Polygons}} object by triangulation
#'
#' @author
#' Emmanuel Blondel \email{emmanuel.blondel1@@gmail.com}
#'
#' @param polygons object of class \code{\link[sp]{Polygons-class}} as defined in \pkg{sp}
#' @return an object of class \code{\link[sp]{Polygons-class}} as defined in \pkg{sp},
#' with cleaned geometries.
#'
#' @aliases clgeo_CleanByTriangulation
#' 
#' @keywords geometry validity summary clean
#' 
clgeo_CleanByTriangulation.Polygons <- function(p){
  
  #not holes
  polygons <- slot(p, "Polygons")[sapply(slot(p,"Polygons"),
                                  function(x){
                                    return(!slot(x,"hole"))
                                  })]
  polygons <- unlist(lapply(polygons, clgeo_CleanByTriangulation.Polygon))
  if(!is.list(polygons)) polygons <- list(polygons)
  poly <- Polygons(srl = polygons, ID="1")
  trsp <- SpatialPolygons(Sr = list(poly))
  trsp <- gUnaryUnion(trsp)
  
  #holes
  holes <- slot(p, "Polygons")[sapply(slot(p,"Polygons"), slot, "hole")]
  if(length(holes) > 0){
    holes <- lapply(holes, function(x){
      slot(x,"hole") <- FALSE
      slot(x,"ringDir") <- 1L
      return(x)
    })
    
    trspholes <- SpatialPolygons(Sr = lapply(1:length(holes), function(i){
      po <- clgeo_CleanByTriangulation.Polygon(holes[[i]])
      if(!is.list(po)) po <- list(po)
      polyholes <- Polygons(srl = po, ID = as.character(i))
      return(polyholes)
    }))
    trspholes <- gUnionCascaded(trspholes, sapply(trspholes@polygons, slot, "ID"))
    trspholes <- SpatialPolygons(Sr = list(Polygons(srl = unlist(lapply(trspholes@polygons, slot, "Polygons")), ID = "1")))
    slot(trspholes, "polygons") <- lapply(slot(trspholes, "polygons"), checkPolygonsHoles)
    
    #before
    #holes <- unlist(lapply(holes, clgeo_CleanByTriangulation.Polygon))
    #if(!is.list(holes)) holes <- list(holes)
    #polyholes <- Polygons(srl = holes, ID="1")
    #trspholes <- SpatialPolygons(Sr = list(polyholes))
    
    #difference
    spout <- gDifference(trsp, trspholes)
  }else{
    spout <- trsp
  }
  return(spout@polygons)
}


#' @title clgeo_CleanByTriangulation.SpatialPolygons
#' 
#' @description
#' Function to clean a \code{\link[sp]{SpatialPolygons}} object by triangulation
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
#' 
clgeo_CleanByTriangulation.SpatialPolygons <- function(sp){
  polygons <- unlist(lapply(sp@polygons, clgeo_CleanByTriangulation.Polygons))
  spout <- SpatialPolygons(Sr = polygons)
  return(spout)
}



