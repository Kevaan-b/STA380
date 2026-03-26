# Note, Eshan/Amin worked on this test file.

library(testthat)

N        <- 20
SIGMA    <- 1.5
B        <- c(0.2, 2)
P_ARMA22 <- c(0.60, -0.30)
Q_ARMA22 <- c(0.45, -0.20)

e_arma22 <- generate_ARMA_errors(n = N, p = P_ARMA22, q = Q_ARMA22, sigma = SIGMA, seed = 42)
d_arma22 <- generate_ARMA_dataset(n = N, p = P_ARMA22, q = Q_ARMA22, sigma = SIGMA, b = B, seed = 42)

fit_lr     <- fit_ARMA(d_arma22, p_order = 0, q_order = 0)
fit_arma11 <- fit_ARMA(d_arma22, p_order = 1, q_order = 1)
fit_ma2    <- fit_ARMA(d_arma22, p_order = 0, q_order = 2)

test_that("generate_ARMA_errors returns correct structure", {
  result <- generate_ARMA_errors(n = N, p = P_ARMA22, q = Q_ARMA22, sigma = SIGMA)
  expect_s3_class(result, "data.frame")
  expect_named(result, c("t", "e", "p_order", "q_order", "arma_label"))
  expect_equal(nrow(result), N)
  expect_equal(result$t, seq_len(N))
})

test_that("generate_ARMA_errors records ARMA order and label metadata", {
  result <- generate_ARMA_errors(n = N, p = P_ARMA22, q = Q_ARMA22, sigma = SIGMA)
  expect_true(all(result$p_order == 2))
  expect_true(all(result$q_order == 2))
  expect_match(result$arma_label[1], "ARMA\\(2, 2\\)")
  expect_match(result$arma_label[1], "0.6")
  expect_match(result$arma_label[1], "-0.3")
  expect_match(result$arma_label[1], "0.45")
  expect_match(result$arma_label[1], "-0.2")
})

test_that("generate_ARMA_errors is reproducible with seed and supports ARMA(0,0)", {
  a <- generate_ARMA_errors(n = N, p = P_ARMA22, q = Q_ARMA22, sigma = SIGMA, seed = 123)
  b <- generate_ARMA_errors(n = N, p = P_ARMA22, q = Q_ARMA22, sigma = SIGMA, seed = 123)
  c <- generate_ARMA_errors(n = N, p = P_ARMA22, q = Q_ARMA22, sigma = SIGMA, seed = 124)

  expect_equal(a, b)
  expect_false(isTRUE(all.equal(a$e, c$e)))

  arma00 <- generate_ARMA_errors(n = N, p = numeric(0), q = numeric(0), sigma = SIGMA, seed = 1)
  expect_true(all(arma00$p_order == 0))
  expect_true(all(arma00$q_order == 0))
  expect_match(arma00$arma_label[1], "ARMA\\(0, 0\\)")
  expect_match(arma00$arma_label[1], "p=\\[none\\]")
  expect_match(arma00$arma_label[1], "q=\\[none\\]")
})

test_that("generate_ARMA_dataset returns expected structure and trend relation", {
  d <- generate_ARMA_dataset(n = N, p = P_ARMA22, q = Q_ARMA22, sigma = SIGMA, b = B, seed = 777)
  expect_s3_class(d, "data.frame")
  expect_named(d, c("t", "y", "e", "p_order", "q_order", "arma_label"))
  expect_equal(nrow(d), N)
  expect_equal(d$y, B[1] * d$t + B[2] + d$e)

  d2 <- generate_ARMA_dataset(n = N, p = P_ARMA22, q = Q_ARMA22, sigma = SIGMA, b = B, seed = 777)
  expect_equal(d, d2)
})

test_that("fit_ARMA returns ARMA_CLS_fit object with expected fields", {
  expect_s3_class(fit_lr, "ARMA_CLS_fit")
  expect_s3_class(fit_arma11, "ARMA_CLS_fit")
  expect_s3_class(fit_ma2, "ARMA_CLS_fit")

  expect_named(fit_arma11, c("coef", "sigma2", "arma", "residuals", "fitted.values"))
  expect_equal(unname(fit_lr$arma), c(0, 0))
  expect_equal(unname(fit_arma11$arma), c(1, 1))
  expect_equal(unname(fit_ma2$arma), c(0, 2))
  expect_true(all(c("ar1", "ma1", "intercept", "t") %in% names(coef(fit_arma11))))
  expect_length(residuals(fit_arma11), N)
  expect_length(fit_arma11$fitted.values, N)
  expect_true(is.finite(fit_arma11$sigma2))
  expect_true(fit_arma11$sigma2 >= 0)
})

test_that("fit_ARMA handles numeric input, invalid orders, and missing y errors", {
  fit_numeric <- fit_ARMA(d_arma22$y, p_order = 1, q_order = 0)
  expect_s3_class(fit_numeric, "ARMA_CLS_fit")
  expect_equal(unname(fit_numeric$arma), c(1, 0))
  expect_true("ar1" %in% names(coef(fit_numeric)))

  fit_sanitized <- fit_ARMA(d_arma22, p_order = -2, q_order = 1.8)
  expect_equal(unname(fit_sanitized$arma), c(0, 1))

  expect_error(
    fit_ARMA(data.frame(t = seq_len(N)), p_order = 0, q_order = 0),
    "must include a `y` column"
  )
})

test_that("validate_arma_inputs and split_joint_par enforce dimensions", {
  meta <- armaVisualizer:::validate_arma_inputs(
    y = 1:5, t_vec = 1:5, p = -2, q = 2.8, par = c(0, 0, 0, 0)
  )
  expect_equal(meta$p, 0)
  expect_equal(meta$q, 2)
  expect_equal(meta$m, 2)

  parts <- armaVisualizer:::split_joint_par(par = c(1, 2, 0.3, -0.4), p = 1, q = 1)
  expect_equal(parts$intercept, 1)
  expect_equal(parts$slope, 2)
  expect_equal(parts$phi, 0.3)
  expect_equal(parts$theta, -0.4)

  expect_error(
    armaVisualizer:::validate_arma_inputs(y = numeric(0), t_vec = numeric(0), p = 0, q = 0),
    "positive length"
  )
  expect_error(
    armaVisualizer:::validate_arma_inputs(y = 1:4, t_vec = 1:3, p = 0, q = 0),
    "`t_vec` length must match `y` length"
  )
  expect_error(
    armaVisualizer:::split_joint_par(par = c(1, 2, 0.3), p = 1, q = 1),
    "Invalid parameter length"
  )
})

test_that("cls_loss and align_fitted_length handle indexing and boundaries", {
  expect_equal(
    armaVisualizer:::cls_loss(par = c(0, 0), y = 1:5, t_vec = 1:5, p = 0, q = 0),
    mean((1:5)^2)
  )

  expect_equal(
    armaVisualizer:::cls_loss(par = c(0, 0, 0, 0), y = 1:2, t_vec = 1:2, p = 2, q = 0),
    Inf
  )

  expect_equal(
    armaVisualizer:::cls_loss(par = c(0, 0), y = 1:5, t_vec = 1:5, p = 0, q = 0, idx = c(-1, 0)),
    Inf
  )

  expect_equal(armaVisualizer:::align_fitted_length(1:3, 5), c(NA, NA, 1, 2, 3))
  expect_equal(armaVisualizer:::align_fitted_length(1:7, 5), c(3, 4, 5, 6, 7))
})

test_that("fit extractors validate inputs and extracted values", {
  fake_fit <- structure(
    list(residuals = 1:4, fitted.values = 5:8),
    class = "ARMA_CLS_fit"
  )
  fake_fit_empty <- structure(
    list(residuals = numeric(0), fitted.values = numeric(0)),
    class = "ARMA_CLS_fit"
  )

  expect_equal(armaVisualizer:::extract_fit_residuals(fake_fit), 1:4)
  expect_equal(armaVisualizer:::extract_fit_fitted_values(fake_fit), 5:8)

  expect_error(armaVisualizer:::extract_fit_residuals(NULL), "`fit` must be provided")
  expect_error(armaVisualizer:::extract_fit_fitted_values(NULL), "`fit` must be provided")
  expect_error(
    armaVisualizer:::extract_fit_residuals(fake_fit_empty),
    "Could not extract residuals"
  )
  expect_error(
    armaVisualizer:::extract_fit_fitted_values(fake_fit_empty),
    "Could not extract `fitted.values`"
  )
})

test_that("plot_ARMA_series runs for valid use-cases", {
  expect_silent(plot_ARMA_series(e_arma22, series = "e"))
  expect_silent(plot_ARMA_series(d_arma22, series = "y"))
  expect_silent(plot_ARMA_series(d_arma22, series = "y", fit = fit_lr))
  expect_silent(plot_ARMA_series(d_arma22, series = "e", fit = fit_lr))
  expect_silent(plot_ARMA_series(d_arma22[, c("t", "y")], series = "e", fit = fit_lr))
  expect_silent(plot_ARMA_series(d_arma22$y, series = "y", fit = fit_arma11))
})

test_that("plot_ARMA_series errors clearly on missing required inputs", {
  expect_error(
    plot_ARMA_series(data.frame(t = seq_len(N)), series = "y"),
    "`series = 'y'` requires a `y` column"
  )

  expect_error(
    plot_ARMA_series(data.frame(t = seq_len(N), y = rnorm(N)), series = "e"),
    "`series = 'e'` requires an `e` column or a fitted model `fit`."
  )

  expect_error(
    plot_ARMA_series(d_arma22$y, series = "e"),
    "For numeric input with `series = 'e'`, provide a fitted model `fit`."
  )
})
