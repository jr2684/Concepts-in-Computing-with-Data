source("functions.R")
context("test for range values")
test_that("range works as expected", {
  x <- c(1, 2, 3, 4, 5)
  y <- c(1, 2, 3, 4, NA)
  z <- c(TRUE, FALSE, TRUE)
  w <- letters[1:5]
  # for x
  expect_equal(stat_range(x), 4)
  expect_length(stat_range(x), 1)
  expect_type(stat_range(x), 'double')
  # for y
  expect_length(stat_range(y),1)
  expect_equal(stat_range(y),NA_real_)
  # for z
  expect_equal(stat_range(z),1)
  expect_type(stat_range(z), "integer")
  expect_equal(stat_range(z), 1L)
  # for w
  expect_error(stat_range(w))
})

context("Test the centers of a vector")
test_that("centers are as expected",{
  expect_equal(stat_centers(y),NA)
  
  
})