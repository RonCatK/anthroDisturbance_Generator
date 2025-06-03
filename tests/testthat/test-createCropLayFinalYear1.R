library(testthat)
library(terra)
library(reproducible)
library(data.table)
library(tictoc)
library(dplyr)

# Helper to disable caching between tests
reset_cache <- function() {
  options(reproducible.cachePath = tempfile())
}

test_that("Lines outside the potential polygon are dropped (cropping)", {
  reset_cache()
  pot <- vect("POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))", crs = "EPSG:4326")
  pot$Potential <- 1L
  line_inside  <- vect("LINESTRING (1 1, 5 5)", crs = "EPSG:4326")
  line_outside <- vect("LINESTRING (20 20, 30 30)", crs = "EPSG:4326")
  Lay <- rbind(line_inside, line_outside)
    
  result <- createCropLayFinalYear1(
    Lay                     = Lay,
    potLayTopValid          = pot,
    runClusteringInParallel = FALSE,
    clusterDistance         = 5,
    studyAreaHash           = "test_crop"
  )
  
  expect_s4_class(result, "SpatVector")
  expect_equal(nrow(result), 1)
  rel <- terra::relate(result, pot, relation = "T********", pairs = FALSE)
  expect_true(any(rel))
})


test_that("Overlapping collinear lines get deduplicated (keep the longer one)", {
  reset_cache()
  pot <- vect("POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))", crs = "EPSG:32631")
  pot$Potential <- 1L
  line1 <- vect("LINESTRING (1 1, 8 1)", crs = "EPSG:32631")
  line2 <- vect("LINESTRING (1 1, 5 1)", crs = "EPSG:32631")
  Lay <- rbind(line1, line2)
  
  result <- createCropLayFinalYear1(
    Lay                     = Lay,
    potLayTopValid          = pot,
    runClusteringInParallel = FALSE,
    clusterDistance         = 5,
    studyAreaHash           = "test_dedupe"
  )
  
  expect_s4_class(result, "SpatVector")
  expect_equal(nrow(result), 1)
  total_length <- terra::perim(result)
  expect_equal(total_length, 7, tolerance = 1e-6)
})


test_that("Intersecting lines at different angles are not removed", {
  reset_cache()
  pot <- vect("POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))", crs = "EPSG:4326")
  pot$Potential <- 1L
  horizontal <- vect("LINESTRING (2 5, 8 5)", crs = "EPSG:4326")
  vertical   <- vect("LINESTRING (5 2, 5 8)", crs = "EPSG:4326")
  Lay <- rbind(horizontal, vertical)
  
  result <- createCropLayFinalYear1(
    Lay                     = Lay,
    potLayTopValid          = pot,
    runClusteringInParallel = FALSE,
    clusterDistance         = 5,
    studyAreaHash           = "test_angles"
  )
  
  expect_s4_class(result, "SpatVector")
  expect_equal(nrow(result), 2)
  angles <- vapply(seq_len(nrow(result)), function(i) calculateLineAngle(result[i, ]), numeric(1))
  expect_true(any(abs(angles) < 1))
  expect_true(any(abs(abs(angles) - 90) < 1))
})


test_that("Clustering assigns separate clusters to distant line groups", {
  reset_cache()
  pot <- vect("POLYGON ((0 0, 0 100, 100 100, 100 0, 0 0))", crs = "EPSG:4326")
  pot$Potential <- 1L
  near_line1 <- vect("LINESTRING (10 10, 20 10)", crs = "EPSG:4326")
  near_line2 <- vect("LINESTRING (15 15, 25 15)", crs = "EPSG:4326")
  far_line   <- vect("LINESTRING (80 80, 90 80)", crs = "EPSG:4326")
  Lay <- rbind(near_line1, near_line2, far_line)
  
  result <- createCropLayFinalYear1(
    Lay                     = Lay,
    potLayTopValid          = pot,
    runClusteringInParallel = FALSE,
    clusterDistance         = 10,
    studyAreaHash           = "test_cluster"
  )
  
  expect_s4_class(result, "SpatVector")
  expect_equal(nrow(result), 3)
  clusters <- unique(result$cluster)
  expect_equal(length(clusters), 2)
  sizes <- as.integer(table(result$cluster))
  expect_true(any(sizes == 2))
  expect_true(any(sizes == 1))
})

# edge cases
# 1. No lines in Lay (empty SpatVector) => result should be empty

test_that("Empty Lay yields empty result", {
  reset_cache()
  # Create an empty lines SpatVector with same CRS
  tmp <- vect("LINESTRING (0 0, 0 0)", crs = "EPSG:4326")
  Lay <- tmp[0,]
  pot <- vect("POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))", crs = "EPSG:4326")
  pot$Potential <- 1L
  
  result <- createCropLayFinalYear1(
    Lay                     = Lay,
    potLayTopValid          = pot,
    runClusteringInParallel = FALSE,
    clusterDistance         = 5,
    studyAreaHash           = "edge_empty"
  )
  
  expect_true(is.null(result) || (inherits(result, "SpatVector") && nrow(result) == 0))
  # result may be NULL or empty SpatVector
  # expect_true(is.null(result) || nrow\(result\) == 0)
})

# 2. All lines outside potential => result should be empty

test_that("All lines outside potential yield empty result", {
  reset_cache()
  line_out <- vect("LINESTRING (20 20, 30 30)", crs = "EPSG:4326")
  Lay <- rbind(line_out)
  pot <- vect("POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))", crs = "EPSG:4326")
  pot$Potential <- 1L
  
  result <- createCropLayFinalYear1(
    Lay                     = Lay,
    potLayTopValid          = pot,
    runClusteringInParallel = FALSE,
    clusterDistance         = 5,
    studyAreaHash           = "edge_all_out"
  )
  
  expect_true(is.null(result) || (inherits(result, "SpatVector") && nrow(result) == 0))
  # result may be NULL or empty SpatVector
  # expect_true(is.null(result) || nrow\(result\) == 0)
})

# 3. Line with zero length inside potential => angle NA, but line should be kept and clustered

test_that("Zero-length line inside potential yields no result or NA angle", {
  reset_cache()
  zero_line <- vect("LINESTRING (5 5, 5 5)", crs = "EPSG:4326")
  Lay <- rbind(zero_line)
  pot <- vect("POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))", crs = "EPSG:4326")
  pot$Potential <- 1L
  
  result <- createCropLayFinalYear1(
    Lay                     = Lay,
    potLayTopValid          = pot,
    runClusteringInParallel = FALSE,
    clusterDistance         = 5,
    studyAreaHash           = "edge_zero_length"
  )
  
  # The function may legitimately return NULL (no features),
  # or a single-feature SpatVector with angle 0 or NA.
  expect_true(is.null(result) || (inherits(result, "SpatVector") && nrow(result) == 1))
  
  if (inherits(result, "SpatVector") && nrow(result) == 1) {
    # If it does return a feature, its angle should be 0 or NA.
    expect_true(is.na(result$angles) || round(result$angles, 1) == 0)
  }
})


# 4. Single line inside polygon with runInParallel = TRUE => still works

test_that("Single line with runInParallel=TRUE is processed", {
  reset_cache()
  line_inside <- vect("LINESTRING (1 1, 2 2)", crs = "EPSG:4326")
  Lay <- rbind(line_inside)
  pot <- vect("POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))", crs = "EPSG:4326")
  pot$Potential <- 1L
  
  result <- createCropLayFinalYear1(
    Lay                     = Lay,
    potLayTopValid          = pot,
    runClusteringInParallel = TRUE,
    clusterDistance         = 5,
    studyAreaHash           = "edge_parallel"
  )
  
  expect_s4_class(result, "SpatVector")
  expect_equal(nrow(result), 1)
  # Angle should be 45 degrees for this diagonal line
  expect_equal(round(result$angles, 1), 45.0)
})

# Added new tests; adjusted to work with revised function

# 1. Multiple potentials with runClusteringInParallel = TRUE
test_that("Clustering works with multiple potentials in parallel", {
  reset_cache()
  # Create two simple polygons via ext():
  pot1 <- as.polygons(ext(0, 10, 0, 10))
  pot2 <- as.polygons(ext(20, 30, 20, 30))
  pot1$Potential <- 1L
  pot2$Potential <- 2L
  pot <- rbind(pot1, pot2)
  
  # Lines in each potential
  line1 <- vect("LINESTRING (5 5, 7 7)", crs = crs(pot))
  line2 <- vect("LINESTRING (25 25, 27 27)", crs = crs(pot))
  Lay <- rbind(line1, line2)
  
  result <- createCropLayFinalYear1(
    Lay, pot, runClusteringInParallel = TRUE, clusterDistance = 5, studyAreaHash = "test_multi_potential"
  )
  
  # Should return two features with distinct Potential values
  expect_s4_class(result, "SpatVector")
  expect_equal(nrow(result), 2)
  expect_setequal(unique(result$Potential), c(1L, 2L))
})

# 2. clusterDistance = 0 (no clustering)
test_that("Zero clusterDistance prevents grouping", {
  reset_cache()
  pot <- vect("POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))", crs = "EPSG:4326")
  pot$Potential <- 1L
  line1 <- vect("LINESTRING (1 1, 2 2)", crs = "EPSG:4326")
  line2 <- vect("LINESTRING (3 3, 4 4)", crs = "EPSG:4326")
  Lay <- rbind(line1, line2)
  
  result <- createCropLayFinalYear1(
    Lay, pot, runClusteringInParallel = FALSE, clusterDistance = 0, studyAreaHash = "test_dist0"
  )
  
  # Both lines remain; no "cluster" column is added
  expect_s4_class(result, "SpatVector")
  expect_equal(nrow(result), 2)
  expect_false("cluster" %in% names(result))
})

# 3. Overlapping lines with 180° angle difference (not removed)
test_that("Collinear lines in opposite directions are kept", {
  reset_cache()
  pot <- vect("POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))", crs = "EPSG:32631")
  pot$Potential <- 1L
  line1 <- vect("LINESTRING (1 1, 5 1)", crs = "EPSG:32631")  # → 0°
  line2 <- vect("LINESTRING (5 1, 1 1)", crs = "EPSG:32631")  # ← 180°
  Lay <- rbind(line1, line2)
  
  result <- createCropLayFinalYear1(
    Lay, pot, runClusteringInParallel = FALSE, clusterDistance = 5, studyAreaHash = "test_opposite_angles"
  )
  
  # Both should be retained: angles differ by 180°
  expect_s4_class(result, "SpatVector")
  expect_equal(nrow(result), 2)
})

# 4. CRS mismatch handling
test_that("CRS mismatch throws error", {
  reset_cache()
  pot <- vect("POLYGON ((0 0, 0 10, 10 10, 10 0, 0 0))", crs = "EPSG:4326")
  line <- vect("LINESTRING (1 1, 5 5)", crs = "EPSG:32610")  # different CRS
  expect_error(
    createCropLayFinalYear1(line, pot, FALSE, 5, "test_crs_mismatch"),
    "CRS.*not match"
  )
})

