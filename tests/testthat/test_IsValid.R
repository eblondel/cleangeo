# test_IsValid.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for clgeo_IsValid.R
#=======================
require(cleangeo, quietly = TRUE)
require(testthat)

#maptools needs to be loaded, other rgeosStatus() returns errors with tests
#(the latter function needs to access .MAPTOOLS_CACHE environment)
require(maptools)

context("clgeo_IsValid")

#valid geometry
test_that("clgeo_IsValid - valid",{
  wkt <- "POLYGON((0 0, 0 10, 10 10, 10 0, 0 0))"
  sp <- rgeos::readWKT(wkt)
  expect_true(gIsValid(sp))
  expect_true(clgeo_IsValid(sp, verbose = TRUE))
})

#geometry with warning
test_that("clgeo_IsValid - warning",{
  wkt <- "POLYGON((0 0, 0 10, 10 0, 10 10, 0 0))"
  sp <- rgeos::readWKT(wkt)
  expect_warning(gIsValid(sp))
  expect_false(clgeo_IsValid(sp, verbose = TRUE))
})

#geometry with error (GEOS exception)
test_that("clgeo_IsValid - error",{
  sp <- SpatialPolygons(
    Srl = list(Polygons(
        srl = list(Polygon(as.matrix(data.frame(x=c(0,10,0),y=c(0,10,0))))),
        ID="1"
    ))
  )
  expect_error(gIsValid(sp))
  expect_false(clgeo_IsValid(sp, verbose = TRUE))
})