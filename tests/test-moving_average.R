library(testthat)

# load moving_average functions from this test folder
source(testthat::test_path("..", "codebase", "MA", "moving_average.R"))



test_that("ma errors gives vector", {
  # same random numbers every run
  set.seed(1)

  # make errors
  out <- generate_MA_errors(15, c(0.5, -0.2), 1, 10)

  # check basic output
  expect_true(is.numeric(out))
  expect_equal(length(out), 15)
})



test_that("ma data gives table", {
  # same random numbers every run
  set.seed(2)

  # make dataset
  out <- generate_MA_dataset(12, c(0.4), 1, 10, c(0.1, 2))

  # check basic shape
  expect_true(is.data.frame(out))
  expect_equal(nrow(out), 12)
  expect_equal(ncol(out), 3)
  expect_equal(names(out), c("t", "y", "e"))
  expect_equal(out$t, 1:12)
})



test_that("ma y math is right", {
  # same random numbers every run
  set.seed(3)

  # make dataset
  out <- generate_MA_dataset(20, c(0.2, -0.1), 1, 20, c(0.5, 1))

  # y should match formula
  y_check <- 0.5 * out$t + 1 + out$e
  expect_equal(out$y, y_check)
})



test_that("fit ma gives model", {
  # same random numbers every run
  set.seed(4)

  # make data and fit
  d <- generate_MA_dataset(40, c(0.3), 1, 20, c(0.1, 2))
  fit1 <- fit_MA(d, c(0.3))
  fit2 <- fit_MA(d$y, c(0.3))

  # check model class
  expect_true("Arima" %in% class(fit1))
  expect_true("Arima" %in% class(fit2))
})
