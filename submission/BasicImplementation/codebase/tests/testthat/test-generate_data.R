library(testthat)

# load generate_data from this test folder
source(testthat::test_path("..", "..", "LR", "generate_data.R"))



test_that("save false gives table", {
  # same random numbers every run
  set.seed(1)

  # make fake data
  out <- generate_data(2, c(1, 2, 3), 8, 0.1, FALSE)

  # check basic shape
  expect_true(is.data.frame(out))
  expect_equal(nrow(out), 8)
  expect_equal(ncol(out), 4)
  expect_equal(names(out), c("x1", "x2", "y", "noise"))

  # x values should be 0 to 1
  expect_true(all(out$x1 >= 0 & out$x1 <= 1))
  expect_true(all(out$x2 >= 0 & out$x2 <= 1))
})



test_that("y math is right", {
  # same random numbers every run
  set.seed(2)

  # run 
  out <- generate_data(2, c(1.5, -2, 0.5), 10, 0.1, FALSE)

  # y should match formula
  y_check <- 1.5 + out$x1 * (-2) + out$x2 * 0.5 + out$noise
  expect_equal(out$y, y_check)
})



test_that("bad b gives minus one", {
  # b size should be num_x plus one
  out <- generate_data(2, c(1, 2), 5, 0.1, FALSE)
  expect_equal(out, -1)
})



test_that("save true makes csv", {
  # use temp folder so project files stay clean
  tmp <- tempfile("gd-")
  dir.create(tmp)
  old <- setwd(tmp)
  on.exit(setwd(old), add = TRUE)

  # run and save csv
  out <- generate_data(1, c(1, 2), 6, 0.1, TRUE)

  # check output file
  expect_equal(out, 0)
  expect_true(file.exists("output.csv"))
})
