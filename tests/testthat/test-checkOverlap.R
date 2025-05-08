library(testthat)
library(terra)

test_that("returns TRUE for overlapping lines", {
  # Create two lines that cross
  coords1 <- matrix(c(0, 0,
                      10, 10), ncol = 2, byrow = TRUE)
  coords2 <- matrix(c(0, 10,
                      10, 0), ncol = 2, byrow = TRUE)
  line1 <- vect(coords1, type = "lines")
  line2 <- vect(coords2, type = "lines")
  expect_true(checkOverlap(line1, line2))
})

test_that("returns FALSE for non-overlapping lines", {
  # Create two parallel lines that do not cross
  coords1 <- matrix(c(0, 0,
                      10, 0), ncol = 2, byrow = TRUE)
  coords2 <- matrix(c(0, 5,
                      10, 5), ncol = 2, byrow = TRUE)
  line1 <- vect(coords1, type = "lines")
  line2 <- vect(coords2, type = "lines")
  expect_false(checkOverlap(line1, line2))
})

test_that("matrix result is binary 0/1", {
  # Overlap returns 1, no overlap returns 0
  coords1 <- matrix(c(0, 0, 10, 10), ncol = 2, byrow = TRUE)
  coords2 <- matrix(c(20, 20, 30, 30), ncol = 2, byrow = TRUE)
  line1 <- vect(coords1, type = "lines")
  line2 <- vect(coords2, type = "lines")
  result <- checkOverlap(line1, line2)
  expect_true(result %in% c(0, 1))
})

test_that("endpoint touching only does not count", {
  coords1 <- matrix(c(0,0, 10,0), ncol=2, byrow=TRUE)
  coords2 <- matrix(c(10,0, 20,0), ncol=2, byrow=TRUE)
  l1 <- vect(coords1, "lines"); l2 <- vect(coords2, "lines")
  expect_false(checkOverlap(l1, l2))
})

test_that("buffer argument works as expected", {
  coords1 <- matrix(c(0,0, 10,0), ncol=2, byrow=TRUE)
  coords2 <- matrix(c(0,10, 10,10), ncol=2, byrow=TRUE)
  l1 <- vect(coords1, "lines"); l2 <- vect(coords2, "lines")
  
  expect_false(checkOverlap(l1, l2, buffer = 5))
  expect_true(checkOverlap(l1, l2, buffer = 8))
})

test_that("invalid inputs throw meaningful errors", {
  mf1 <- vect(matrix(c(0,0, 1,1), ncol=2, byrow=TRUE), "lines")
  mf2 <- vect(matrix(c(2,2, 3,3), ncol=2, byrow=TRUE), "lines")
  mf  <- rbind(mf1, mf2)
  expect_error(checkOverlap(mf, mf), "exactly one feature")
})



