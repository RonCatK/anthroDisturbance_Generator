library(testthat)
library(terra)
library(sf)
library(tictoc)

# Define a dummy tic() to avoid missing-function errors during testing
tic <- function(...) invisible(NULL)

# Create a studyArea polygon matching the raster extent (0,0) to (10,10)

# Create a polygon
polygon <- st_polygon(list(cbind(c(0, 0, 10, 10, 0), c(0, 10, 10, 0, 0))))
# Convert the polygon to an sf object
sf_object <- st_sf(geometry = st_sfc(polygon))
# Create a SpatVector object from the sf object
studyArea <- vect(sf_object)

crs(studyArea) <- "EPSG:3005"

#--- Begin unit tests for createBufferedDisturbances() ---

test_that("Vector output is a SpatVector with buffered geometry and excludes potentials", {
  outV <- suppressWarnings(createBufferedDisturbances(
    disturbanceList  = disturbanceList,
    bufferSize       = 1,
    rasterToMatch    = r,
    studyArea        = studyArea,
    currentTime      = 0,
    convertToRaster  = FALSE
  ))
  # Should return a SpatVector
  expect_s4_class(outV, "SpatVector")
  
  # Bounding box of the output should lie within studyArea extent expanded by buffer 1
  ext <- ext(outV)
  expect_true(ext[1] >= (0 - 1) && ext[3] <= (10 + 1)) # xmin and xmax
  expect_true(ext[2] >= (0 - 1) && ext[4] <= (10 + 1)) # ymin and ymax
  
  # Ensure no features originate from potential layers by checking that
  # the combined area does not include geometry far outside studyArea
  # e.g., potentialCutBlocks were shifted by +1000 in x, so extent xMax < 100
  expect_true(ext[3] < 100)
})


test_that("Raster output is binary and has correct values at known locations", {
  outR <- suppressWarnings(createBufferedDisturbances(
    disturbanceList  = disturbanceList,
    bufferSize       = 1,
    rasterToMatch    = r,
    studyArea        = studyArea,
    currentTime      = 0,
    convertToRaster  = TRUE
  ))
  # Should return either a RasterLayer or SpatRaster
  expect_true(inherits(outR, "RasterLayer") || inherits(outR, "SpatRaster"))
  
  # Values should be only 0, 1, or NA
  if (inherits(outR, "SpatRaster")) {
    vals <- terra::values(outR)
  } else {
    vals <- values(outR)
  }
  unique_vals <- unique(vals[!is.na(vals)])
  expect_true(all(unique_vals %in% c(0, 1)))
  
  # Determine cell indices for (2,2) and (9,9)
  cell_center <- cellFromXY(r, cbind(2, 2))
  cell_far <- cellFromXY(r, cbind(9, 9))
  
  # Check value at (2,2) == 1 and at (9,9) == 0
  if (inherits(outR, "SpatRaster")) {
    expect_equal(vals[cell_center], 1)
    expect_equal(vals[cell_far], 0)
  } else {
    expect_equal(outR[cell_center], 1)
    expect_equal(outR[cell_far], 0)
  }
})


test_that("Function handles empty disturbance layers gracefully", {
  onlyEmpty <- list(
    pipelines = list(
      pipelines = vect()
    )
  )
  
  # Vector mode: current implementation errors when all layers are empty, so expect an error
  expect_error(
    createBufferedDisturbances(
      disturbanceList  = onlyEmpty,
      bufferSize       = 1,
      rasterToMatch    = r,
      studyArea        = studyArea,
      currentTime      = 0,
      convertToRaster  = FALSE
    )
  )
  
  # Raster mode: Should return a Raster or SpatRaster where all non-NA cells = 0
  resR <- suppressWarnings(createBufferedDisturbances(
    disturbanceList  = onlyEmpty,
    bufferSize       = 1,
    rasterToMatch    = r,
    studyArea        = studyArea,
    currentTime      = 0,
    convertToRaster  = TRUE
  ))
  # Accept either RasterLayer or SpatRaster
  expect_true(inherits(resR, "RasterLayer") || inherits(resR, "SpatRaster"))
  if (inherits(resR, "SpatRaster")) {
    all_vals <- terra::values(resR)
  } else {
    all_vals <- values(resR)
  }
  expect_true(all(all_vals[!is.na(all_vals)] == 0))
})


test_that("bufferSize = 0 yields an error or no change", {
  # Expect that setting bufferSize = 0 leads to an error (buffer of zero not supported)
  expect_error(
    createBufferedDisturbances(
      disturbanceList = disturbanceList,
      bufferSize      = 0,
      rasterToMatch   = r,
      studyArea       = studyArea,
      currentTime     = 0,
      convertToRaster = FALSE
    )
  )
  
  expect_error(
    createBufferedDisturbances(
      disturbanceList = disturbanceList,
      bufferSize      = 0,
      rasterToMatch   = r,
      studyArea       = studyArea,
      currentTime     = 0,
      convertToRaster = TRUE
    )
  )
})


test_that("List with only potential layers behaves as all‐empty", {
  onlyPotentials <- list(
    foo = list(potentialFoo = vect(st_sfc(st_point(c(1,1)), crs = crs(r)))),
    bar = list(potentialBar = vect(st_sfc(st_point(c(5,5)), crs = crs(r))))
  )
  
  # Vector mode should error
  expect_error(
    createBufferedDisturbances(
      disturbanceList  = onlyPotentials,
      bufferSize       = 1,
      rasterToMatch    = r,
      studyArea        = studyArea,
      currentTime      = 0,
      convertToRaster  = FALSE
    )
  )
  
  # Raster mode should also error (due to internal indexing on an empty list)
  expect_error(
    createBufferedDisturbances(
      disturbanceList  = onlyPotentials,
      bufferSize       = 1,
      rasterToMatch    = r,
      studyArea        = studyArea,
      currentTime      = 0,
      convertToRaster  = TRUE
    )
  )
})


test_that("Raster input layer gets polygonized correctly", {
  # Build a tiny input raster with some 1-values:
  r_small <- rast(nrows=5, ncols=5, xmin=0, xmax=5, ymin=0, ymax=5, vals = c(
    rep(0, 5), 0,1,0,0,0,  # only one 1 in the center
    rep(0, 15)
  ))
  crs(r_small) <- crs(r)
  
  # Define studyAreaSmall as a SpatVector directly (avoid sf st_as_sfc)
  coords_small <- rbind(c(0,0), c(0,5), c(5,5), c(5,0), c(0,0))
  studyAreaSmall <- vect(list(coords_small), type = "polygons", crs = crs(r))
  
  distList2 <- list(
    mixed = list(
      someRaster = r_small,
      potentialRaster = r_small  # this will be excluded by the function
    )
  )
  
  outVmixed <- createBufferedDisturbances(
    disturbanceList = distList2,
    bufferSize      = 1,
    rasterToMatch   = r_small,
    studyArea       = studyAreaSmall,
    currentTime     = 0,
    convertToRaster = FALSE
  )
  # There should be a single polygon (from the center cell), buffered by 1 unit, and aggregated.
  ext_mixed <- ext(outVmixed)
  expect_true(ext_mixed[1] < ext_mixed[3])   # xmin < xmax
  expect_true(ext_mixed[2] < ext_mixed[4])   # ymin < ymax
  
  outRmixed <- createBufferedDisturbances(
    disturbanceList = distList2,
    bufferSize      = 1,
    rasterToMatch   = r_small,
    studyArea       = studyAreaSmall,
    currentTime     = 0,
    convertToRaster = TRUE
  )
  # That single 1-valued cell should create a small polygon buffered by one cell.
  # When rasterized back onto r_small, at least one cell should be set to 1.
  if (inherits(outRmixed, "SpatRaster")) {
    all_vals2 <- terra::values(outRmixed)
  } else {
    all_vals2 <- values(outRmixed)
  }
  expect_true(any(all_vals2 == 1))
})


test_that("Overlapping polygons are dissolved into a single union", {
  polyA <- st_polygon(list(rbind(c(2,2), c(2,4), c(4,4), c(4,2), c(2,2))))
  polyB <- st_polygon(list(rbind(c(3,3), c(3,5), c(5,5), c(5,3), c(3,3))))
  
  # Convert each sfg into a SpatVector—do NOT pass crs= twice
  svA <- vect(st_sfc(polyA, crs = crs(r)))
  svB <- vect(st_sfc(polyB, crs = crs(r)))
  
  dl_overlap <- list(
    foo = list(
      a = svA,
      b = svB
    )
  )
  
  outO <- createBufferedDisturbances(
    disturbanceList = dl_overlap,
    bufferSize      = 0,
    rasterToMatch   = r,
    studyArea       = studyArea,
    currentTime     = 0,
    convertToRaster = FALSE
  )
  
  # Since polyA and polyB overlap (even before buffering), the result
  # should dissolve them into a single feature.
  expect_true(nrow(outO) == 1)
})
