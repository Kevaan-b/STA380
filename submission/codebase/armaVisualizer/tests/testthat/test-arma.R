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

test_that("generate_ARMA_errors with empty p and q returns ARMA(0,0) metadata", {
  result <- generate_ARMA_errors(n = N, p = numeric(0), q = numeric(0), sigma = SIGMA)
  expect_true(all(result$p_order == 0))
  expect_true(all(result$q_order == 0))
  expect_match(result$arma_label[1], "ARMA\\(0, 0\\)")
  expect_match(result$arma_label[1], "p=\\[none\\]")
  expect_match(result$arma_label[1], "q=\\[none\\]")
})

test_that("generate_ARMA_errors seed argument is reproducible", {
  e1 <- generate_ARMA_errors(n = N, p = P_ARMA22, q = Q_ARMA22, sigma = SIGMA, seed = 123)
  e2 <- generate_ARMA_errors(n = N, p = P_ARMA22, q = Q_ARMA22, sigma = SIGMA, seed = 123)
  expect_equal(e1, e2)
})

test_that("generate_ARMA_dataset returns correct structure and trend formula", {
  d <- generate_ARMA_dataset(n = N, p = P_ARMA22, q = Q_ARMA22, sigma = SIGMA, b = B, seed = 123)
  expect_s3_class(d, "data.frame")
  expect_named(d, c("t", "y", "e", "p_order", "q_order", "arma_label"))
  expect_equal(nrow(d), N)
  expect_equal(d$t, seq_len(N))
  expect_equal(d$y, B[1] * d$t + B[2] + d$e)
  expect_true(all(d$p_order == 2))
  expect_true(all(d$q_order == 2))
})

test_that("generate_ARMA_dataset seed argument is reproducible", {
  d1 <- generate_ARMA_dataset(n = N, p = P_ARMA22, q = Q_ARMA22, sigma = SIGMA, b = B, seed = 999)
  d2 <- generate_ARMA_dataset(n = N, p = P_ARMA22, q = Q_ARMA22, sigma = SIGMA, b = B, seed = 999)
  expect_equal(d1, d2)
})

test_that("fit_ARMA uses trend regressor when t exists", {
  fit <- fit_ARMA(d_arma22, p_order = 1, q_order = 1)
  expect_true(any(names(fit$coef) %in% c("data$t", "xreg")))
})

test_that("fit_ARMA accepts numeric vector input", {
  fit_num <- fit_ARMA(d_arma22$y, p_order = 1, q_order = 1)
  expect_s3_class(fit_num, "Arima")
  expect_equal(fit_num$p_order, 1)
  expect_equal(fit_num$q_order, 1)
  expect_match(fit_num$fit_label, "ARMA\\(1, 1\\) fit")
})

test_that("fit_ARMA accepts data frame input without t column", {
  d_no_t <- data.frame(y = d_arma22$y)
  fit_no_t <- fit_ARMA(d_no_t, p_order = 1, q_order = 1)
  expect_s3_class(fit_no_t, "Arima")
  expect_equal(fit_no_t$p_order, 1)
  expect_equal(fit_no_t$q_order, 1)
  expect_match(fit_no_t$fit_label, "ARMA\\(1, 1\\) fit")
})

test_that("estimate_ARMA_errors computes y minus fitted deterministic part", {
  fit <- fit_ARMA(d_arma22_plt, p_order = 1, q_order = 1)
  est <- armaVisualizer:::estimate_ARMA_errors(d_arma22_plt, fit)

  intercept <- 0
  trend_coef <- 0
  if ("intercept" %in% names(fit$coef)) intercept <- fit$coef["intercept"]
  if ("data$t" %in% names(fit$coef)) trend_coef <- fit$coef["data$t"]
  if ("xreg" %in% names(fit$coef)) trend_coef <- fit$coef["xreg"]

  expected <- as.numeric(d_arma22_plt$y - (intercept + trend_coef * d_arma22_plt$t))
  expect_equal(est, expected)
})

test_that("estimate_ARMA_errors prioritizes xreg when both trend names exist", {
  fake_fit <- list(coef = c(intercept = 2, "data$t" = 0.1, xreg = 0.25))
  d_small <- data.frame(t = 1:4, y = c(10, 11, 12, 13))
  est <- armaVisualizer:::estimate_ARMA_errors(d_small, fake_fit)
  expect_equal(est, as.numeric(d_small$y - (2 + 0.25 * d_small$t)))
})

test_that("plot_ARMA_series supports numeric vector input with fit", {
  expect_silent(plot_ARMA_series(d_arma22_plt$y, series = "y", fit = fit_lr_plt))
  expect_silent(plot_ARMA_series(d_arma22_plt$e, series = "e", fit = fit_lr_plt))
})

test_that("plot_ARMA_series accepts explicit fitted_vals without fit object", {
  expect_silent(plot_ARMA_series(d_arma22_plt, series = "y", fitted_vals = d_arma22_plt$y - 0.5))
  expect_silent(plot_ARMA_series(d_arma22_plt, series = "e", fitted_vals = d_arma22_plt$e - 0.5))
})

test_that("plot_ARMA_series works for data frames without p_order and q_order", {
  d_min <- data.frame(t = d_arma22_plt$t, y = d_arma22_plt$y, e = d_arma22_plt$e)
  expect_silent(plot_ARMA_series(d_min, series = "y"))
  expect_silent(plot_ARMA_series(d_min, series = "e"))
})
