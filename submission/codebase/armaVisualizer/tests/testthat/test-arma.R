library(testthat)

N        <- 20
SIGMA    <- 1.5
B        <- c(0.2, 2)          
P_ARMA22 <- c(0.60, -0.30)
Q_ARMA22 <- c(0.45, -0.20)

test_that("generate_ARMA_errors returns correct structure", {
  result <- generate_ARMA_errors(n = N, p = P_ARMA22, q = Q_ARMA22, sigma = SIGMA)
  expect_s3_class(result, "data.frame")

  expect_named(result, c("t", "e", "p_order", "q_order", "arma_label"))
  expect_equal(nrow(result), N)
})

test_that("generate_ARMA_errors returns t sequence 1:n", {
  result <- generate_ARMA_errors(n = N, p = P_ARMA22, q = Q_ARMA22, sigma = SIGMA)
  expect_equal(result$t, seq_len(N))
})

test_that("generate_ARMA_errors records ARMA(2,2) p_order and q_order", {
  result <- generate_ARMA_errors(n = N, p = P_ARMA22, q = Q_ARMA22, sigma = SIGMA)
  expect_true(all(result$p_order == 2))
  expect_true(all(result$q_order == 2))
})

test_that("generate_ARMA_errors arma_label encodes ARMA(2,2) coefficients", {
  result <- generate_ARMA_errors(n = N, p = P_ARMA22, q = Q_ARMA22, sigma = SIGMA)
  expect_match(result$arma_label[1], "ARMA\\(2, 2\\)")
  expect_match(result$arma_label[1], "0.6")
  expect_match(result$arma_label[1], "-0.3")
  expect_match(result$arma_label[1], "0.45")
  expect_match(result$arma_label[1], "-0.2")
})


# Shared dataset: matches vignette set.seed(42) before generation
set.seed(42)
d_arma22 <- generate_ARMA_dataset(n = N, p = P_ARMA22, q = Q_ARMA22, sigma = SIGMA, b = B)

test_that("fit_ARMA LR baseline (0,0) returns Arima with correct metadata", {
  fit_lr <- fit_ARMA(d_arma22, p_order = 0, q_order = 0)
  expect_s3_class(fit_lr, "Arima")
  expect_equal(fit_lr$p_order, 0)
  expect_equal(fit_lr$q_order, 0)
  expect_match(fit_lr$fit_label, "ARMA\\(0, 0\\) fit")
})

test_that("fit_ARMA ARMA(1,1) returns Arima with correct metadata", {
  fit_arma11 <- fit_ARMA(d_arma22, p_order = 1, q_order = 1)
  expect_s3_class(fit_arma11, "Arima")
  expect_equal(fit_arma11$p_order, 1)
  expect_equal(fit_arma11$q_order, 1)
  expect_match(fit_arma11$fit_label, "ARMA\\(1, 1\\) fit")
})

test_that("fit_ARMA MA(2) returns Arima with correct metadata", {
  fit_ma2 <- fit_ARMA(d_arma22, p_order = 0, q_order = 2)
  expect_s3_class(fit_ma2, "Arima")
  expect_equal(fit_ma2$p_order, 0)
  expect_equal(fit_ma2$q_order, 2)
  expect_match(fit_ma2$fit_label, "ARMA\\(0, 2\\) fit")
})

set.seed(42)
e_arma22     <- generate_ARMA_errors( n = N, p = P_ARMA22, q = Q_ARMA22, sigma = SIGMA)
set.seed(42)
d_arma22_plt <- generate_ARMA_dataset(n = N, p = P_ARMA22, q = Q_ARMA22, sigma = SIGMA, b = B)

fit_lr_plt     <- fit_ARMA(d_arma22_plt, p_order = 0, q_order = 0)
fit_arma11_plt <- fit_ARMA(d_arma22_plt, p_order = 1, q_order = 1)
fit_ma2_plt    <- fit_ARMA(d_arma22_plt, p_order = 0, q_order = 2)
fit_ar2_plt    <- fit_ARMA(d_arma22_plt, p_order = 2, q_order = 0)
fit_arma22_plt <- fit_ARMA(d_arma22_plt, p_order = 2, q_order = 2)


test_that("plot_ARMA_series errors-only plot (vignette row 1) runs silently", {
  expect_silent(plot_ARMA_series(e_arma22, series = "e"))
})

test_that("plot_ARMA_series y-only plot (vignette row 1) runs silently", {
  expect_silent(plot_ARMA_series(d_arma22_plt, series = "y"))
})

test_that("plot_ARMA_series LR y and e overlays run silently", {
  expect_silent(plot_ARMA_series(d_arma22_plt, series = "y", fit = fit_lr_plt))
  expect_silent(plot_ARMA_series(d_arma22_plt, series = "e", fit = fit_lr_plt))
})