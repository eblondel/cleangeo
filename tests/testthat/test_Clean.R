# test_Clean.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for clgeo_Clean.R
#=======================
require(cleangeo, quietly = TRUE)
require(testthat)

#maptools needs to be loaded, other rgeosStatus() returns errors with tests
#(the latter function needs to access .MAPTOOLS_CACHE environment)
require(maptools)

context("clgeo_Clean")

file <- system.file("extdata", "example.shp", package = "cleangeo")
sp <- readShapePoly(file)
sp.ids <- sapply(slot(sp, "polygons"), slot, "ID")
report <- NULL
nv <- NULL

#CollectionReport
test_that("clgeo_CollectionReport",{
  
  report <<- clgeo_CollectionReport(sp)
  expect_is(report, "data.frame")
  expect_equal(nrow(report), 3L)
  
  for(i in 1:nrow(report)){
    geom <- SpatialPolygons(Srl=list(slot(sp, "polygons")[[i]]))
    expect_equal(report[i,"valid"],gIsValid(geom))
  }
 
  expect_equal(as.character(report[2,"type"]), "rgeos_validity")
  expect_equal(as.character(report[2,"issue_type"]), "GEOM_VALIDITY")
  expect_equal(as.character(report[3,"type"]), "rgeos_validity")
  expect_equal(as.character(report[3, "issue_type"]), "GEOM_VALIDITY")
  
})

#SuspiciousFeatures
test_that("clgeo_SuspiciousFeatures",{
  nv <<- clgeo_SuspiciousFeatures(report)
  expect_equal(length(nv), 2L)
  expect_equal(nv, c(2,3))
})

#Clean
test_that("clgeo_Clean",{
  sp.fixed <- clgeo_Clean(sp, print.log = FALSE)
  
  #data integrity
  expect_equal(sapply(slot(sp.fixed, "polygons"), slot, "ID"), sp.ids)
  expect_equal(slot(sp,"data"), slot(sp.fixed, "data"))
  
  #cleaning results
  report.fixed <- clgeo_CollectionReport(sp.fixed)
  nv.fixed <- clgeo_SuspiciousFeatures(report.fixed)
  expect_true(all(report.fixed[,"valid"]))
  expect_equal(nv.fixed, NA)
})

#Clean cases
#(list of cases taken from https://github.com/tudelft3d/prepair)

#TODO case to investigate
#-> validation using gBuffer NOT OK (lose of area)
test_that("Clean - 'bowtie' polygons",{
  wkt <- "POLYGON((0 0, 0 10, 10 0, 10 10, 0 0))"
  sp <- rgeos::readWKT(wkt)
})

test_that("Clean - 'bowtie' polygons with holes",{
  
  wkt <- "POLYGON((0 0, 0 5, 3 10, 0 10, 10 0, 10 10, 0 0),(1 3, 2 3, 2 4, 1 4, 1 3))"
  sp <- rgeos::readWKT(wkt)
  expect_false(gIsValid(sp))
  
  wkt <- "POLYGON((0 0, 0 5, 3 10, 0 10, 10 0, 10 10, 0 0),(1 3, 2 3, 2 4, 1 4, 1 3),
          (7 5, 7 6, 8 6, 8 5, 7 5))"
  sp <- rgeos::readWKT(wkt)
  expect_false(gIsValid(sp))
  
  wkt <- "POLYGON((0 0, 0 5, 3 10, 0 10, 10 0, 10 10, 0 0),(1 3, 2 3, 2 4, 1 4, 1 3),
          (7 5, 7 6, 8 6, 8 5, 7 5),(7 3.5, 7 4, 8 4, 7 3.5))"
  sp <- rgeos::readWKT(wkt)
  expect_false(gIsValid(sp))
  
  wkt <- "POLYGON((0 0, 0 5, 3 10, 0 10, 10 0, 10 10, 0 0),(1 3, 2 3, 2 4, 1 4, 1 3),
          (7 5, 7 6, 8 6, 8 5, 7 5),(7 3.5, 7 4, 8 3.5, 8 4, 7 3.5))"
  
})

#validation is OK (managed by rgeos)
test_that("Clean - Square with wrong orientation",{
  wkt <- "POLYGON((0 0, 0 10, 10 10, 10 0, 0 0))"
  sp <- rgeos::readWKT(wkt)
  expect_true(gIsValid(sp))
})

#TODO case to investigate
#-> validation may be considered OK (nested ring is removed)
#-> validation may be considered NOT OK? (lose of area - expect 2 overlapping polygons)
test_that("Clean - Inner ring with one edge sharing part of an edge of the outer ring",{
  wkt <- "POLYGON((0 0, 10 0, 10 10, 0 10, 0 0),(5 2,5 7,10 7, 10 2, 5 2))"
  sp <- rgeos::readWKT(wkt)
  sp.clean <- clgeo_Clean(sp)
  expect_false(gIsValid(sp))
  expect_true(gIsValid(sp.clean))
})

#validation is OK (managed by cleangeo)
test_that("Clean - Dangling edge",{
  wkt <- "POLYGON((0 0, 10 0, 15 5, 10 0, 10 10, 0 10, 0 0))"
  sp <- rgeos::readWKT(wkt)
  sp.clean <- clgeo_Clean(sp)
  expect_false(gIsValid(sp))
  expect_true(gIsValid(sp.clean))
})

#TODO case to investigate
test_that("Clean - Outer ring not closed",{
  wkt <- "POLYGON((0 0, 10 0, 10 10, 0 10))"
  #sp <- rgeos::readWKT(wkt)
  #sp.clean <- clgeo_Clean(sp)
  #expect_false(gIsValid(sp))
  #expect_true(gIsValid(sp.clean))
})

#validation is OK (managed by cleangeo)
test_that("Clean - Two adjacent inner rings",{
  wkt <- "POLYGON((0 0, 10 0, 10 10, 0 10, 0 0), (1 1, 1 8, 3 8, 3 1, 1 1), (3 1, 3 8, 5 8, 5 1, 3 1))"
  sp <- rgeos::readWKT(wkt)
  sp.clean <- clgeo_Clean(sp)
  expect_false(gIsValid(sp))
  expect_true(gIsValid(sp.clean))
})

#TODO case to investigate
test_that("Clean - Polygon with an inner ring inside another inner ring:",{
  wkt <- "POLYGON((0 0, 10 0, 10 10, 0 10, 0 0), (2 8, 5 8, 5 2, 2 2, 2 8), (3 3, 4 3, 3 4, 3 3))"
  sp <- rgeos::readWKT(wkt)
  sp.clean <- clgeo_Clean(sp)
  expect_false(gIsValid(sp))
  expect_true(gIsValid(sp.clean))
})
