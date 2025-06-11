library(testthat)
library(raster)
library(terra)
library(sf)

# 1. Raster merging: ensure old and new rasters are OR'ed cell-wise
test_that("raster merging ORs cells correctly", {
  past <- raster(matrix(c(0,1,0,0), nrow = 2))
  curr <- raster(matrix(c(1,0,0,0), nrow = 2))
  disturbanceList <- list(
    A = list(layer1 = past)
  )
  updatedLayers <- list(
    A = list(layer1 = curr)
  )
  
  out <- replaceList(disturbanceList, updatedLayers)
  
  expect_true(inherits(out$A$layer1, c("RasterLayer","SpatRaster")))
  v_expected <- pmax(values(past), values(curr))
  expect_equal(values(out$A$layer1), v_expected)
})

# 2. Missing sector: should return original layers unchanged
test_that("missing sector returns original sub-list", {
  potential <- raster(matrix(5, 2, 2))
  other <- raster(matrix(2, 2, 2))
  disturbanceList <- list(
    A = list(potentialInfra = potential, layer2 = other)
  )
  updatedLayers <- list()  # no entry for sector A
  
  out <- replaceList(disturbanceList, updatedLayers)
  
  # Should be identical to original list for sector A
  expect_identical(out$A, disturbanceList$A)
})

# 3. Explicit NULL update: single named layer present but NULL => returns original element
test_that("NULL update yields original element", {
  past <- raster(matrix(0, 3, 3))
  disturbanceList <- list(A = list(layer1 = past))
  updatedLayers <- list(A = list(layer1 = NULL))
  
  out <- replaceList(disturbanceList, updatedLayers)
  
  expect_identical(out$A$layer1, past)
})

# 4. Preserve potential-first ordering
test_that("preserves potential layers at front", {
  potential <- raster(matrix(2, 3, 3))
  past <- raster(matrix(0, 3, 3))
  curr <- raster(matrix(1, 3, 3))
  disturbanceList <- list(
    A = list(potentialInfra = potential, layer1 = past)
  )
  updatedLayers <- list(A = list(layer1 = curr))
  
  out <- replaceList(disturbanceList, updatedLayers)
  expect_equal(names(out$A), c("potentialInfra","layer1"))
  expect_identical(out$A$potentialInfra, potential)
})

# 5. Vector merging: same-geometry polygons are dissolved and class carried over
test_that("merges vector polygon layers correctly", {
  poly1 <- terra::vect("POLYGON((0 0,1 0,1 1,0 1,0 0))")
  poly2 <- terra::vect("POLYGON((1 0,2 0,2 1,1 1,1 0))")
  poly2$Class <- "newClass"
  disturbanceList <- list(A = list(layer1 = poly1))
  updatedLayers <- list(A = list(layer1 = poly2))
  
  out <- replaceList(disturbanceList, updatedLayers)
  expect_true(inherits(out$A$layer1, "SpatVector"))
  expect_equal(out$A$layer1$Class, "newClass")
})

# 6. Vector merging: different geometries (line -> polygon) still retains new Class
test_that("merges different-geometry vector layers", {
  line <- terra::vect("LINESTRING(0 0,1 1)")
  poly <- terra::vect("POLYGON((0 0,1 0,1 1,0 1,0 0))")
  poly$Class <- "polyClass"
  disturbanceList <- list(A = list(layer1 = line))
  updatedLayers <- list(A = list(layer1 = poly))
  
  out <- replaceList(disturbanceList, updatedLayers)
  expect_true(inherits(out$A$layer1, "SpatVector"))
  expect_equal(out$A$layer1$Class, "polyClass")
})
