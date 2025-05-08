library(testthat)
library(data.table)
library(terra)
library(sf)
library(crayon)

# source your original implementation
source("R/calculateSizeOriginal.R")  

test_that("non-wind missing layer is dropped", {
  dp <- data.table(
    dataName         = "foo",
    dataClass        = "potentialFoobar",
    disturbanceOrigin= "origin",
    disturbanceType  = "Generating"
  )
  dl <- list(foo = list(origin = NULL))
  out <- calculateSize(dp, dl, whichToUpdate = 1)
  expect_equal(nrow(out), 0L)
})

test_that("potentialWindTurbines missing layer yields numeric 62500", {
  dp <- data.table(
    dataName         = "wind",
    dataClass        = "potentialWindTurbines",
    disturbanceOrigin= "wt",
    disturbanceType  = "Generating"
  )
  dl <- list(wind = list(wt = NULL))
  expect_message(
    out <- calculateSize(dp, dl, whichToUpdate = 1),
    "potentialWindTurbines"
  )
  expect_equal(nrow(out), 1L)
  expect_true(is.numeric(out$disturbanceSize))
  expect_equal(out$disturbanceSize, 62500)
})

test_that("vector polygon produces correct rtnorm string", {
  # 10×10 square → area=100, perimeter=40
  sq <- vect(matrix(c(0,0, 10,0, 10,10, 0,10, 0,0), 
                    ncol=2, byrow=TRUE),
             type="polygons", crs="EPSG:3857")
  dp <- data.table(
    dataName         = "bar",
    dataClass        = "potentialBar",
    disturbanceOrigin= "bar",
    disturbanceType  = "Generating"
  )
  dl <- list(bar = list(bar = sq))
  expect_message(
    out <- calculateSize(dp, dl, whichToUpdate = 1),
    "trying to convert", all = FALSE
  )
  expect_equal(nrow(out), 1L)
  ds <- out$disturbanceSize
  # mean(area)=100, sd(area)=0
  expect_true(
    grepl("^rtnorm\\(1, 100\\.00, 0\\.00, lower = 0\\)$", ds)
  )
})

test_that("SpatRaster is converted and yields correct rtnorm", {
  # 1×1 cell 10×10 → area=100, perimeter=40
  r <- rast(nrows=1, ncols=1, xmin=0, xmax=10, ymin=0, ymax=10)
  values(r) <- 1
  dp <- data.table(
    dataName         = "baz",
    dataClass        = "potentialBaz",
    disturbanceOrigin= "baz",
    disturbanceType  = "Generating"
  )
  dl <- list(baz = list(baz = r))
  expect_message(
    out <- calculateSize(dp, dl, whichToUpdate = 1),
    "Trying to convert", all = FALSE
  )
  expect_equal(nrow(out), 1L)
  ds <- out$disturbanceSize
  expect_true(
    grepl("^rtnorm\\(1, 100\\.00, 0\\.00, lower = 0\\)$", ds)
  )
})

test_that("line geometries use perimeter", {
  ln <- vect(st_sfc(st_linestring(rbind(c(0,0),c(0,5))), crs="EPSG:3857"))
  dp <- data.table(
    dataName         = "road",
    dataClass        = "potentialRoad",
    disturbanceOrigin= "road",
    disturbanceType  = "Connecting"
  )
  dl <- list(road = list(road = ln))
  out <- calculateSize(dp, dl, whichToUpdate = 1)
  # perimeter of a 5-unit line = 5
  ds <- out$disturbanceSize
  expect_true(
    grepl("^rtnorm\\(1, 5\\.00, 0\\.00, lower = 0\\)$", ds)
  )
})

