# test_IsValid.R
# Author: Emmanuel Blondel <emmanuel.blondel1@gmail.com>
#
# Description: Unit tests for clgeo_IsValid.R
#=======================
require(cleangeo, quietly = TRUE)
require(testthat)

context("clgeo_IsValid")

#valid geometry
test_that("clgeo_IsValid - valid",{
  wkt <- "POLYGON((0 0, 0 10, 10 10, 10 0, 0 0))"
  sp = as(sf::st_sf(geom = sf::st_as_sfc(wkt), crs = 4326), "Spatial")
  expect_true(clgeo_IsValid(sp, verbose = TRUE))
})

#geometry with error
test_that("clgeo_IsValid - error",{
  wkt <- "POLYGON((0 0, 0 10, 10 0, 10 10, 0 0))"
  sp = as(sf::st_sf(geom = sf::st_as_sfc(wkt), crs = 4326), "Spatial")
  expect_false(clgeo_IsValid(sp, verbose = TRUE))
})
