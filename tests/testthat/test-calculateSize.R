library(testthat)
library(data.table)
library(terra)
library(crayon)
library(sf)

test_that("non-wind missing layer is dropped", {
  # disturbanceParameters: one non-wind row
  dp <- data.table(
    dataName = "foo",
    dataClass = "potentialFoobar",
    disturbanceOrigin = "origin",
    disturbanceType = "Generating"
  )
  # disturbanceList has no foo/origin
  dl <- list(foo = list(origin = NULL))
  out <- calculateSize(disturbanceParameters = dp, disturbanceList = dl, whichToUpdate = 1)
  # row should be removed
  expect_equal(nrow(out), 0)
})

test_that("potentialWindTurbines missing layer yields 62500 size", {
  dp <- data.table(
    dataName = "energy",
    dataClass = "potentialWindTurbines",
    disturbanceOrigin = "wt",
    disturbanceType = "Generating"
  )
  dl <- list(energy = list(wt = NULL))
  out <- calculateSize(disturbanceParameters = dp, disturbanceList = dl, whichToUpdate = 1)
  print(out)
  expect_equal(nrow(out), 1)
  expect_equal(out$disturbanceSize, 62500)
})

test_that("vector layer yields rtnorm string with correct mean & sd", {
  # build a simple square polygon of area 100 m2
  sq <- vect(matrix(c(0,0, 10,0, 10,10, 0,10, 0,0), ncol=2, byrow=TRUE),
             type="polygons", crs="EPSG:3857")
  # perimeter = 40, expanse = area = 100
  dp <- data.table(
    dataName = "bar",
    dataClass = "potentialBar",
    disturbanceOrigin = "bar",
    disturbanceType = "Generating"
  )
  dl <- list(bar = list(bar = sq))
  out <- calculateSize(disturbanceParameters = dp, disturbanceList = dl, whichToUpdate = 1)
  expect_equal(nrow(out), 1)
  # disturbanceSize should be a string "rtnorm(1, mean, sd, lower = 0)"
  ds <- out$disturbanceSize
  expect_true(grepl("^rtnorm\\(1,\\s*100\\.?0*,\\s*0\\.?0*,", ds))
})

test_that("raster layer is converted to polygons then processed", {
  # create a 2×2 raster with one cell = 1 → area=cell_area
  r <- rast(nrows=1, ncols=1, xmin=0, xmax=10, ymin=0, ymax=10)
  values(r) <- 1
  dp <- data.table(
    dataName = "baz",
    dataClass = "potentialBaz",
    disturbanceOrigin = "baz",
    disturbanceType = "Generating"
  )
  dl <- list(baz = list(baz = r))
  out <- calculateSize(disturbanceParameters = dp, disturbanceList = dl, whichToUpdate = 1)
  expect_equal(nrow(out), 1)
  ds <- out$disturbanceSize
  # perimeter of square is 40 → mean=40, sd=0
  expect_true(grepl("^rtnorm\\(1,\\s*40\\.?0*,\\s*0\\.?0*,", ds))
})

test_that("calculateSize works on fixtures", {
  whichToUpdate <- seq_len(nrow(disturbanceParameters))
  out <- calculateSize(
    disturbanceParameters = disturbanceParameters,
    disturbanceList       = disturbanceList,
    whichToUpdate         = whichToUpdate
  )
  
  # 1) row count unchanged
  expect_equal(nrow(out), nrow(disturbanceParameters))
  
  # 2) All disturbanceSize are character
  expect_true(is.character(out$disturbanceSize))
  
  # 3) Wind-turbine default is exactly "62500"
  wind_sz <- out[dataClass == "potentialWindTurbines", disturbanceSize]
  expect_equal(length(wind_sz), 1L)
  expect_equal(wind_sz, "62500")
  
  # 4) Point-only layers (pipelines & seismic) → 0.00, 0.00
  zero_cases <- c("potentialPipelines","potentialSeismicLines")
  zero_vals <- out[dataClass %in% zero_cases, disturbanceSize]
  expect_true(all(grepl("^rtnorm\\(1,\\s*0\\.00,\\s*0\\.00, lower = 0\\)$", zero_vals)))
  
  # 5) Polygon layers (settlements, mining, forestry buffers) → positive means
  poly_cases <- c("potentialSettlements","potentialMining","potentialCutBlocks")
  poly_vals <- out[dataClass %in% poly_cases, disturbanceSize]
  means <- as.numeric(sub("^rtnorm\\(1,\\s*([0-9\\.]+),.*$", "\\1", poly_vals))
  expect_true(all(means > 0))
  
  # 6) Everything else matches rtnorm(...) pattern
  other_vals <- out[!dataClass %in% c("potentialWindTurbines", zero_cases, poly_cases),
                    disturbanceSize]
  expect_true(all(grepl("^rtnorm\\(1,\\s*\\d+\\.\\d{2},\\s*\\d+\\.\\d{2}, lower = 0\\)$",
                        other_vals)))
})
