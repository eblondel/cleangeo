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
  sp.fixed <- clgeo_Clean(sp, verbose = FALSE)
  
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

#validation is OK (managed by cleangeo)
test_that("Clean - 'bowtie' polygons",{
  wkt <- "POLYGON((0 0, 0 10, 10 0, 10 10, 0 0))"
  sp <- rgeos::readWKT(wkt)
  sp.clean <- clgeo_Clean(sp)
  expect_false(gIsValid(sp))
  expect_true(gIsValid(sp.clean))
})

#validation is OK (managed by cleangeo)
test_that("Clean - 'bowtie' polygons with holes",{
  
  wkt <- "POLYGON((0 0, 0 5, 3 10, 0 10, 10 0, 10 10, 0 0),(1 3, 2 3, 2 4, 1 4, 1 3))"
  sp <- rgeos::readWKT(wkt)
  expect_false(gIsValid(sp))
  sp.clean <- clgeo_Clean(sp)
  expect_true(gIsValid(sp.clean))
  
  wkt <- "POLYGON((0 0, 0 5, 3 10, 0 10, 10 0, 10 10, 0 0),(1 3, 2 3, 2 4, 1 4, 1 3),
          (7 5, 7 6, 8 6, 8 5, 7 5))"
  sp <- rgeos::readWKT(wkt)
  expect_false(gIsValid(sp))
  sp.clean <- clgeo_Clean(sp)
  expect_true(gIsValid(sp.clean))
  
  wkt <- "POLYGON((0 0, 0 5, 3 10, 0 10, 10 0, 10 10, 0 0),(1 3, 2 3, 2 4, 1 4, 1 3),
          (7 5, 7 6, 8 6, 8 5, 7 5),(7 3.5, 7 4, 8 4, 7 3.5))"
  sp <- rgeos::readWKT(wkt)
  expect_false(gIsValid(sp))
  sp.clean <- clgeo_Clean(sp)
  expect_true(gIsValid(sp.clean))
  
  wkt <- "POLYGON((0 0, 0 5, 3 10, 0 10, 10 0, 10 10, 0 0),(1 3, 2 3, 2 4, 1 4, 1 3),
          (7 5, 7 6, 8 6, 8 5, 7 5),(7 3.5, 7 4, 8 3.5, 8 4, 7 3.5))"
  sp <- rgeos::readWKT(wkt)
  expect_false(gIsValid(sp))
  sp.clean <- clgeo_Clean(sp)
  expect_true(gIsValid(sp.clean))
  
})

#validation is OK (managed by rgeos)
test_that("Clean - Square with wrong orientation",{
  wkt <- "POLYGON((0 0, 0 10, 10 10, 10 0, 0 0))"
  sp <- rgeos::readWKT(wkt)
  expect_true(gIsValid(sp))
})

#validation is OK (managed by cleangeo)
test_that("Clean - Inner ring with one edge sharing part of an edge of the outer ring",{
  wkt <- "POLYGON((0 0, 10 0, 10 10, 0 10, 0 0),(5 2,5 7,10 7, 10 2, 5 2))"
  sp <- rgeos::readWKT(wkt)
  sp.clean <- clgeo_Clean(sp)
  expect_false(gIsValid(sp))
  expect_true(gIsValid(sp.clean))
})

#validation is OK (managed by cleangeo)
test_that("Clean - Two adjacent inner rings",{
  wkt <- "POLYGON((0 0, 10 0, 10 10, 0 10, 0 0), (1 1, 1 8, 3 8, 3 1, 1 1), (3 1, 3 8, 5 8, 5 1, 3 1))"
  sp <- rgeos::readWKT(wkt)
  sp.clean <- clgeo_Clean(sp)
  expect_false(gIsValid(sp))
  expect_true(gIsValid(sp.clean))
})

#validation is OK (managed by cleangeo)
test_that("Clean - Nested polygon",{
  wkt <- "POLYGON((0 0, 10 0, 10 10, 0 10, 0 0), (2 8, 5 8, 5 2, 2 2, 2 8), (3 3, 4 3, 3 4, 3 3))"
  sp <- rgeos::readWKT(wkt)
  sp.clean <- clgeo_Clean(sp)
  expect_false(gIsValid(sp))
  expect_true(gIsValid(sp.clean))
})

#validation is OK (managed by cleangeo)
test_that("Clean - Nested polygon",{
  wkt <- "POLYGON((0 0, 10 0, 10 10, 0 10, 0 0), (1 1, 9 1, 9 9, 1 9, 1 1),
                  (2 2, 8 2, 8 8, 2 8, 2 2))"
  sp <- rgeos::readWKT(wkt)
  sp.clean <- clgeo_Clean(sp)
  expect_false(gIsValid(sp))
  expect_true(gIsValid(sp.clean))
})

#validation is OK (managed by cleangeo)
test_that("Clean - Multiple nested polygon",{
  wkt <- "POLYGON((0 0, 10 0, 10 10, 0 10, 0 0), (1 1, 9 1, 9 9, 1 9, 1 1),
                  (2 2, 8 2, 8 8, 2 8, 2 2),(3 3, 7 3, 7 7, 3 7, 3 3))"
  sp <- rgeos::readWKT(wkt)
  sp.clean <- clgeo_Clean(sp)
  expect_false(gIsValid(sp))
  expect_true(gIsValid(sp.clean))
  
  wkt <- "POLYGON((0 0, 10 0, 10 10, 0 10, 0 0), (1 1, 9 1, 9 9, 1 9, 1 1),
                  (2 2, 8 2, 8 8, 2 8, 2 2),(3 3, 7 3, 7 7, 3 7, 3 3),
                  (4 4, 6 4, 6 6, 4 6, 4 4))"
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

test_that("Clean - false holes - case 1",{
  wkt <- "POLYGON((0 0, 0 10, 10 10, 10 5, 5 5, 5 7, 3 7, 3 5, 5 5, 5 0, 0 0))"
  sp <- rgeos::readWKT(wkt)
  sp.clean <- clgeo_Clean(sp)
  expect_false(gIsValid(sp))
  expect_true(gIsValid(sp.clean))
})

test_that("Clean - false holes - case 2",{
  wkt <- "POLYGON((0 0, 0 10, 10 10, 10 5, 3 5, 3 7, 5 7, 5 0, 0 0))"
  sp <- rgeos::readWKT(wkt)
  sp.clean <- clgeo_Clean(sp)
  expect_false(gIsValid(sp))
  expect_true(gIsValid(sp.clean))
})

# TESTS TO INVESTIGATE FURTHER
#------------------------------

#hexagonal polygons - clgeo_Clean has no effect - deals with coordinates precision
#TODO investigate
test_that("Clean - hexagonal polygons",{
  p1 <- Polygon(cbind(c(1276503.26781119, 1281876.11747031, 1287248.96712942,
                        1287248.96712942, 1281876.11747031, 1276503.26781119,1276503.26781119),
                      c(204391.40834643, 207493.42454344, 204391.40834643,198187.37595242,
                        195085.35975541, 198187.37595242, 204391.40834643)))
  p2 <- Polygon(cbind(c(1287248.96712943, 1292621.81678854, 1297994.66644766,
                        1297994.66644766, 1292621.81678854, 1287248.96712943,1287248.96712943),
                      c(204391.40834643, 207493.42454344, 204391.40834643,198187.37595242,
                        195085.35975541, 198187.37595242, 204391.40834643)))
  p3 <- Polygon(cbind(c(1281876.11747031, 1287248.96712943, 1292621.81678854,
                        1292621.81678854, 1287248.96712943, 1281876.11747031,1281876.11747031),
                      c(213697.45693745, 216799.47313446, 213697.45693745,207493.42454344,
                        204391.40834643, 207493.42454344, 213697.45693745)))
  sp <- SpatialPolygons(list(Polygons(list(p1, p2, p3), 's1')))
  expect_false(gIsValid(sp))
  #sp.clean <- clgeo_Clean(sp)
  #expect_false(gIsValid(sp.clean))
  
})
