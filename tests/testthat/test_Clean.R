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
