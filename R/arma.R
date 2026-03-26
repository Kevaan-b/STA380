#' Generate ARMA(p, q) Errors
#'
#' @description Simulates an ARMA error process of order `length(p)` and
#' `length(q)` using coefficient vectors `p` and `q` with Gaussian innovations.
#'
#' @param n Integer number of returned errors after burn-in.
#' @param p Numeric vector of AR coefficients.
#' @param q Numeric vector of MA coefficients.
#' @param sigma Numeric standard deviation of the white-noise innovations.
#' @param seed Optional integer random seed for reproducibility.
#'
#' @return Data frame with columns `t`, `e`, `p_order`, `q_order`, and
#' `arma_label`.
#'
#' @examples
#' e <- generate_ARMA_errors(n = 20, p = c(0.45), q = c(0.3, -0.1), sigma = 1)
#'
#' @importFrom stats rnorm
#' @importFrom utils tail
#' @export
generate_ARMA_errors <- function(n, p = numeric(0), q = numeric(0), sigma = 1, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  burnin <- 50
  total_n <- n + burnin
  p_order <- length(p)
  q_order <- length(q)

  z <- rnorm(total_n, mean = 0, sd = sigma)
  e <- z

  for (tt in seq_len(total_n)) {
    if (p_order > 0) {
      for (i in seq_len(p_order)) {
        if (tt - i > 0) e[tt] <- e[tt] + p[i] * e[tt - i]
      }
    }
    if (q_order > 0) {
      for (j in seq_len(q_order)) {
        if (tt - j > 0) e[tt] <- e[tt] + q[j] * z[tt - j]
      }
    }
  }

  p_text <- if (p_order > 0) paste(round(p, 4), collapse = ", ") else "none"
  q_text <- if (q_order > 0) paste(round(q, 4), collapse = ", ") else "none"
  arma_label <- paste0("ARMA(", p_order, ", ", q_order, ") | p=[", p_text, "] | q=[", q_text, "]")

  data.frame(
    t = seq_len(n),
    e = tail(e, n),
    p_order = rep(p_order, n),
    q_order = rep(q_order, n),
    arma_label = rep(arma_label, n),
    stringsAsFactors = FALSE
  )
}

#' Generate a synthetic ARMA dataset with linear trend
#'
#' @description Generate a synthetic ARMA dataset with linear trend of form:
#' `y_t = b1 * t + b0 + e_t`.
#'
#' @param n Integer number of returned observations.
#' @param p Numeric vector of AR coefficients.
#' @param q Numeric vector of MA coefficients.
#' @param sigma Numeric standard deviation of the white-noise innovations.
#' @param b Numeric vector of length 2 where `b[1] = b1` and `b[2] = b0`.
#' @param seed Optional integer random seed for reproducibility.
#'
#' @return Data frame with columns `t`, `y`, `e`, `p_order`, `q_order`,
#' and `arma_label`.
#'
#' @examples
#' d <- generate_ARMA_dataset(n = 300, p = c(0.4), q = c(0.25, -0.1), sigma = 1.5, b = c(0.04, 2))
#'
#' @export
generate_ARMA_dataset <- function(n, p = numeric(0), q = numeric(0), sigma = 1, b = c(0, 0), seed = NULL) {
  errors_df <- generate_ARMA_errors(n = n, p = p, q = q, sigma = sigma, seed = seed)
  t <- errors_df$t
  y_t <- b[1] * t + b[2] + errors_df$e

  data.frame(
    t = t,
    y = y_t,
    e = errors_df$e,
    p_order = errors_df$p_order,
    q_order = errors_df$q_order,
    arma_label = errors_df$arma_label,
    stringsAsFactors = FALSE
  )
}

#' Validate Core ARMA Inputs
#'
#' @param y Numeric response vector.
#' @param t_vec Numeric trend covariate vector.
#' @param p Integer AR order.
#' @param q Integer MA order.
#' @param par Optional numeric parameter vector.
#'
#' @return List with normalized input values and dimensions.
validate_arma_inputs <- function(y, t_vec, p, q, par = NULL) {
  y <- as.numeric(y)
  t_vec <- as.numeric(t_vec)
  p <- max(0, as.integer(p))
  q <- max(0, as.integer(q))

  if (length(y) == 0) stop("Input series must have positive length.")
  if (length(t_vec) != length(y)) stop("`t_vec` length must match `y` length.")
  if (!is.null(par) && length(par) != (2 + p + q)) stop("Invalid parameter length.")

  list(y = y, t_vec = t_vec, p = p, q = q, n = length(y), m = max(p, q))
}

#' Split Joint Trend + ARMA Parameter Vector
#'
#' @param par Numeric parameter vector.
#' @param p Integer AR order.
#' @param q Integer MA order.
#'
#' @return List with trend, AR, and MA components.
split_joint_par <- function(par, p, q) {
  validate_arma_inputs(y = 0, t_vec = 0, p = p, q = q, par = par)
  list(
    intercept = unname(par[1]),
    slope = unname(par[2]),
    phi = if (p > 0) unname(par[3:(2 + p)]) else numeric(0),
    theta = if (q > 0) unname(par[(3 + p):(2 + p + q)]) else numeric(0)
  )
}

#' Build ARMA Recursive State
#'
#' @param y Numeric response vector.
#' @param t_vec Numeric trend covariate vector.
#' @param par Numeric parameter vector.
#' @param p Integer AR order.
#' @param q Integer MA order.
#'
#' @return List with trend, residual, and fitted-value state.
build_arma_state <- function(y, t_vec, par, p, q) {
  meta <- validate_arma_inputs(y, t_vec, p, q, par = par)
  n <- meta$n
  m <- meta$m
  parts <- split_joint_par(par, p, q)

  trend_hat <- parts$intercept + parts$slope * t_vec
  x <- y - trend_hat
  ehat <- numeric(n)
  fitted_arma <- rep(NA_real_, n)

  if (n > m) {
    for (tt in (m + 1):n) {
      ar_part <- if (p > 0) sum(parts$phi * x[(tt - 1):(tt - p)]) else 0
      ma_part <- if (q > 0) sum(parts$theta * ehat[(tt - 1):(tt - q)]) else 0
      fitted_arma[tt] <- ar_part + ma_part
      ehat[tt] <- x[tt] - fitted_arma[tt]
    }
  }

  list(
    trend_hat = trend_hat,
    x = x,
    ehat = ehat,
    fitted_arma = fitted_arma,
    fitted_full = trend_hat + fitted_arma,
    resid_full = y - (trend_hat + fitted_arma),
    n = n,
    m = m
  )
}

arma_residuals <- function(x, phi, theta) {
  p <- length(phi)
  q <- length(theta)
  par <- c(0, 0, phi, theta)
  build_arma_state(y = as.numeric(x), t_vec = rep(0, length(x)), par = par, p = p, q = q)$ehat
}

#' Compute CLS Loss for Joint Trend + ARMA
#'
#' @param par Numeric parameter vector.
#' @param y Numeric response vector.
#' @param t_vec Numeric trend covariate vector.
#' @param p Integer AR order.
#' @param q Integer MA order.
#' @param idx Optional index vector to evaluate mini-batch loss.
#'
#' @return Scalar CLS loss.
cls_loss <- function(par, y, t_vec, p, q, idx = NULL) {
  state <- build_arma_state(y = y, t_vec = t_vec, par = par, p = p, q = q)
  n <- state$n
  m <- state$m

  if (n <= m) return(Inf)

  if (is.null(idx)) {
    use_idx <- (m + 1):n
  } else {
    use_idx <- as.integer(idx)
    use_idx <- use_idx[use_idx >= (m + 1) & use_idx <= n]
    if (length(use_idx) == 0) return(Inf)
  }

  resid <- state$ehat[use_idx]
  if (!all(is.finite(resid))) return(Inf)
  mean(resid^2)
}

cls_loss_arma <- function(par, y, t_vec, p, q) {
  cls_loss(par = par, y = y, t_vec = t_vec, p = p, q = q)
}

#' Compute One Mini-batch Gradient for Joint Trend + ARMA
#'
#' @param y Numeric response vector.
#' @param t_vec Numeric trend covariate vector.
#' @param par Numeric parameter vector.
#' @param p Integer AR order.
#' @param q Integer MA order.
#' @param batch_start Integer first batch index.
#' @param batch_end Integer last batch index.
#' @param warmup Integer warmup width for recursion.
#'
#' @return List with `grad`.
cls_grad_block_vec <- function(y, t_vec, par, p, q, batch_start, batch_end, warmup = 20) {
  parts <- split_joint_par(par, p, q)
  m <- max(p, q)
  start_t <- max(m + 1, batch_start - warmup)
  end_t <- batch_end
  t_idx <- start_t:end_t
  Tlen <- length(t_idx)

  trend_hat <- parts$intercept + parts$slope * t_vec
  x <- y - trend_hat

  ehat <- numeric(Tlen)
  d_intercept <- numeric(Tlen)
  d_slope <- numeric(Tlen)
  dphi <- matrix(0, nrow = Tlen, ncol = p)
  dtheta <- matrix(0, nrow = Tlen, ncol = q)

  for (s in seq_len(Tlen)) {
    tt <- t_idx[s]
    ar_lags <- if (p > 0) x[(tt - 1):(tt - p)] else numeric(0)
    ar_part <- if (p > 0) sum(parts$phi * ar_lags) else 0

    ma_lags <- if (q > 0) {
      vapply(seq_len(q), function(j) {
        prev <- s - j
        if (prev >= 1) ehat[prev] else 0
      }, numeric(1))
    } else {
      numeric(0)
    }
    ma_part <- if (q > 0) sum(parts$theta * ma_lags) else 0
    ehat[s] <- x[tt] - ar_part - ma_part

    row_intercept <- -1 + if (p > 0) sum(parts$phi) else 0
    if (q > 0) {
      for (j in seq_len(q)) {
        prev <- s - j
        if (prev >= 1) row_intercept <- row_intercept - parts$theta[j] * d_intercept[prev]
      }
    }
    d_intercept[s] <- row_intercept

    row_slope <- -t_vec[tt] + if (p > 0) sum(parts$phi * t_vec[(tt - 1):(tt - p)]) else 0
    if (q > 0) {
      for (j in seq_len(q)) {
        prev <- s - j
        if (prev >= 1) row_slope <- row_slope - parts$theta[j] * d_slope[prev]
      }
    }
    d_slope[s] <- row_slope

    if (p > 0) {
      row_phi <- -ar_lags
      if (q > 0) {
        for (j in seq_len(q)) {
          prev <- s - j
          if (prev >= 1) row_phi <- row_phi - parts$theta[j] * dphi[prev, ]
        }
      }
      dphi[s, ] <- row_phi
    }

    if (q > 0) {
      row_theta <- -ma_lags
      for (j in seq_len(q)) {
        prev <- s - j
        if (prev >= 1) row_theta <- row_theta - parts$theta[j] * dtheta[prev, ]
      }
      dtheta[s, ] <- row_theta
    }
  }

  idx <- (batch_start:batch_end) - start_t + 1
  resid <- ehat[idx]
  n_batch <- length(idx)

  grad <- c(
    (2 / n_batch) * sum(d_intercept[idx] * resid),
    (2 / n_batch) * sum(d_slope[idx] * resid),
    if (p > 0) (2 / n_batch) * colSums(dphi[idx, , drop = FALSE] * resid) else numeric(0),
    if (q > 0) (2 / n_batch) * colSums(dtheta[idx, , drop = FALSE] * resid) else numeric(0)
  )

  list(grad = grad)
}

#' Fit trend and ARMA Parameters by SGD with Momentum
#'
#' @param y Numeric response vector.
#' @param t_vec Numeric trend covariate vector.
#' @param p Integer AR order.
#' @param q Integer MA order.
#' @param init Numeric initial parameter vector.
#' @param lr Numeric learning rate.
#' @param momentum Numeric momentum coefficient.
#' @param batch_size Integer mini-batch length.
#' @param warmup Integer warmup width for recursion.
#' @param max_epochs Integer max training epochs.
#'
#' @return List with fitted parameters.
fit_cls_sgd_momentum <- function(y, t_vec, p, q,
                                 init = c(0, 0, rep(1.6/(p + q), p + q)),
                                 lr = 1e-3,
                                 momentum = 0.9,
                                 batch_size = 64,
                                 warmup = 10,
                                 max_epochs = 50) {
  meta <- validate_arma_inputs(y, t_vec, p, q, par = init)
  n <- meta$n
  m <- meta$m
  if (n <= m) stop("Series is too short for requested ARMA order.")

  par <- as.numeric(init)
  vel <- numeric(length(par))

  for (epoch in seq_len(max_epochs)) {
    block_starts <- seq(m + 1, n, by = batch_size)
    block_ends <- pmin(block_starts + batch_size - 1, n)

    for (z in seq_along(block_starts)) {
      out <- cls_grad_block_vec(
        y = y, t_vec = t_vec, par = par, p = p, q = q,
        batch_start = block_starts[z], batch_end = block_ends[z], warmup = warmup
      )
      grad <- out$grad

      vel <- momentum * vel - lr * grad
      par <- par + vel
    }
  }

  parts <- split_joint_par(par, p, q)

  list(
    par = par,
    trend_coef = c(intercept = parts$intercept, t = parts$slope),
    phi = parts$phi,
    theta = parts$theta
  )
}

#' Fit an ARMA Model to Data
#'
#' @description Fits an ARMA model of order `p_order` and `q_order` with a
#' linear trend `intercept + t * slope` estimated jointly by SGD.
#'
#' @param data Data frame containing at least `y` (and optionally `t`), or a
#'   numeric response vector.
#' @param p_order Integer AR order.
#' @param q_order Integer MA order.
#'
#' @return A fitted object with residuals/fitted values and ARMA metadata.
#'
#' @export
fit_ARMA <- function(data, p_order = 0, q_order = 0) {
  y <- if (is.data.frame(data)) as.numeric(data$y) else as.numeric(data)
  n <- length(y)

  if (is.data.frame(data)) {
    if (!"y" %in% names(data)) stop("For data-frame input, `data` must include a `y` column.")
    t_vec <- if ("t" %in% names(data)) as.numeric(data$t) else seq_len(n)
  } else {
    t_vec <- seq_len(n)
  }

  meta <- validate_arma_inputs(y, t_vec, p_order, q_order)
  p_order <- meta$p
  q_order <- meta$q

  t_mean <- mean(t_vec)
  t_sd <- sd(t_vec)
  if (!is.finite(t_sd) || t_sd <= 0) t_sd <- 1
  t_opt <- (t_vec - t_mean) / t_sd

  init_trend <- tryCatch(
    as.numeric(coef(lm(y ~ t_opt))),
    error = function(e) c(mean(y), 0)
  )

  opt <- fit_cls_sgd_momentum(
    y = y,
    t_vec = t_opt,
    p = p_order,
    q = q_order,
    init = c(init_trend[1], init_trend[2], rep(0.01, p_order + q_order))
  )

  trend_coef_opt <- c(
    intercept = unname(opt$trend_coef["intercept"]),
    t = unname(opt$trend_coef["t"])
  )
  trend_coef <- c(
    intercept = unname(trend_coef_opt["intercept"] - trend_coef_opt["t"] * t_mean / t_sd),
    t = unname(trend_coef_opt["t"] / t_sd)
  )

  par_original <- c(
    trend_coef["intercept"],
    trend_coef["t"],
    if (p_order > 0) opt$phi else numeric(0),
    if (q_order > 0) opt$theta else numeric(0)
  )

  state <- build_arma_state(y = y, t_vec = t_vec, par = par_original, p = p_order, q = q_order)
  resid_full <- state$resid_full
  fitted_full <- state$fitted_full

  sigma2 <- mean(resid_full[is.finite(resid_full)]^2)
  if (!is.finite(sigma2)) sigma2 <- 0

  ar_names <- if (p_order > 0) paste0("ar", seq_len(p_order)) else character(0)
  ma_names <- if (q_order > 0) paste0("ma", seq_len(q_order)) else character(0)

  coef_public <- c(
    setNames(opt$phi, ar_names),
    setNames(opt$theta, ma_names),
    c(intercept = unname(trend_coef["intercept"]), t = unname(trend_coef["t"]))
  )

  fit <- list(
    coef = coef_public,
    sigma2 = sigma2,
    arma = c(p_order, q_order),
    residuals = resid_full,
    fitted.values = fitted_full
  )

  class(fit) <- "ARMA_CLS_fit"
  fit
}

coef.ARMA_CLS_fit <- function(object, ...) {
  object$coef
}

residuals.ARMA_CLS_fit <- function(object, ...) {
  object$residuals
}

extract_fit_residuals <- function(fit) {
  if (is.null(fit)) stop("`fit` must be provided for residual overlays.")
  arma_errors <- as.numeric(residuals(fit))
  if (length(arma_errors) == 0) stop("Could not extract residuals from `fit`.")
  arma_errors
}

extract_fit_fitted_values <- function(fit) {
  if (is.null(fit)) stop("`fit` must be provided for fitted value overlays.")
  fitted_vals <- as.numeric(fit$fitted.values)
  if (length(fitted_vals) == 0) stop("Could not extract `fitted.values` from `fit`.")
  fitted_vals
}

resolve_series_input <- function(x, series, fit = NULL) {
  has_observed_errors <- FALSE

  if (is.data.frame(x)) {
    t <- if ("t" %in% names(x)) x$t else seq_len(nrow(x))

    if (series == "e") {
      if ("e" %in% names(x) && any(is.finite(x$e))) {
        y_obs <- x$e
        has_observed_errors <- TRUE
      } else if (!is.null(fit)) {
        y_obs <- align_fitted_length(extract_fit_residuals(fit), length(t))
      } else {
        stop("`series = 'e'` requires an `e` column or a fitted model `fit`.")
      }
    } else {
      if (!"y" %in% names(x)) stop("`series = 'y'` requires a `y` column for data-frame input.")
      y_obs <- x$y
    }
  } else {
    y_obs <- as.numeric(x)
    t <- seq_along(y_obs)

    if (series == "e") {
      if (is.null(fit)) stop("For numeric input with `series = 'e'`, provide a fitted model `fit`.")
      y_obs <- align_fitted_length(extract_fit_residuals(fit), length(t))
    }
  }

  list(t = t, y_obs = y_obs, has_observed_errors = has_observed_errors)
}

align_fitted_length <- function(fitted_vals, target_len) {
  fitted_vals <- as.numeric(fitted_vals)
  if (length(fitted_vals) > target_len) return(tail(fitted_vals, target_len))
  if (length(fitted_vals) < target_len) return(c(rep(NA_real_, target_len - length(fitted_vals)), fitted_vals))
  fitted_vals
}

#' Plot ARMA series over time
#'
#' @description Plots ARMA data or errors from either a generated ARMA data
#' frame or a numeric vector and overlays fit outputs if provided. For
#' `series = "y"` overlays come from `fit$fitted.values`; for `series = "e"`
#' overlays come from `residuals(fit)`.
#'
#' @param x Either a data frame returned by `generate_ARMA_dataset()` /
#'   `generate_ARMA_errors()`, or a numeric vector.
#' @param series Character string. Either `"y"` or `"e"`.
#' @param fitted_vals Optional numeric vector of fitted values.
#' @param fit Optional fitted model object from `fit_ARMA()`.
#'
#' @return No return value; called for side effects.
#'
#' @examples
#' d <- generate_ARMA_dataset(n = 300, p = c(0.4), q = c(0.25, -0.1), sigma = 1.5, b = c(0.04, 2))
#' fit <- fit_ARMA(d, p_order = 1, q_order = 2)
#' plot_ARMA_series(d, series = "y")
#' plot_ARMA_series(d, series = "e", fit = fit)
#' plot_ARMA_series(d$y, series = "y")
#'
#' @importFrom graphics legend lines plot
#' @importFrom stats residuals
#' @export
plot_ARMA_series <- function(x, series = c("y", "e"), fitted_vals = NULL, fit = NULL) {
  series <- match.arg(series)
  resolved <- resolve_series_input(x, series, fit)
  t <- resolved$t
  y_obs <- resolved$y_obs

  y_axis <- if (series == "e") "ARMA Errors" else "y"
  plot_type <- if (series == "e") "l" else "p"
  plot_pch <- if (series == "e") NA else 16
  fit_col <- if (series == "e") "red" else "blue"

  if (!is.null(fit) && is.null(fitted_vals)) {
    if (series == "e") {
      if (resolved$has_observed_errors) fitted_vals <- extract_fit_residuals(fit)
    } else {
      fitted_vals <- extract_fit_fitted_values(fit)
    }
  }

  if (!is.null(fitted_vals)) fitted_vals <- align_fitted_length(fitted_vals, length(t))

  dgp_label <- if (is.data.frame(x) && all(c("p_order", "q_order") %in% names(x))) {
    paste0("ARMA(", x$p_order[1], ", ", x$q_order[1], ") dgp")
  } else if (series == "e") {
    "ARMA errors"
  } else {
    "ARMA data"
  }

  fit_line_label <- if (!is.null(fitted_vals) || !is.null(fit)) {
    if (!is.null(fit) && !is.null(fit$arma) && length(fit$arma) >= 2) {
      paste0("ARMA(", fit$arma[1], ", ", fit$arma[2], ") fit")
    } else {
      "fit"
    }
  } else {
    NULL
  }

  plot_title <- if (!is.null(fit_line_label)) paste(dgp_label, "|", fit_line_label) else dgp_label

  plot(t, y_obs, type = plot_type, pch = plot_pch, cex = 0.7, xlab = "Time", ylab = y_axis, main = plot_title)

  if (!is.null(fitted_vals)) {
    lines(t, fitted_vals, col = fit_col, lwd = 2)
    legend_labels <- if (series == "e") c(expression(e), expression(hat(e))) else c(expression(y), expression(hat(y)))
    legend("topleft", legend = legend_labels, col = c("black", fit_col), pch = c(16, NA), lty = c(NA, 1), bty = "n")
  }
}
