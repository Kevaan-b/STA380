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
  if (!is.null(seed)) {
    set.seed(seed)
  }

  burnin <- 50
  total_n <- n + burnin
  p_order <- length(p)
  q_order <- length(q)
  z <- rnorm(total_n, mean = 0, sd = sigma)
  e <- z

  # Build e_t recursively using AR and MA lag terms.
  for (t in seq_len(total_n)) {
    for (i in seq_len(p_order)) {
      if (t - i > 0) {
        e[t] <- e[t] + p[i] * e[t - i]
      }
    }
    for (j in seq_len(q_order)) {
      if (t - j > 0) {
        e[t] <- e[t] + q[j] * z[t - j]
      }
    }
  }

  e_out <- tail(e, n)
  t_out <- seq_len(n)
  p_text <- if (p_order > 0) paste(round(p, 4), collapse = ", ") else "none"
  q_text <- if (q_order > 0) paste(round(q, 4), collapse = ", ") else "none"
  arma_label <- paste0("ARMA(", p_order, ", ", q_order, ") | p=[", p_text, "] | q=[", q_text, "]")

  data.frame(
    t = t_out,
    e = e_out,
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
  errors_df <- generate_ARMA_errors(
    n = n,
    p = p,
    q = q,
    sigma = sigma,
    seed = seed
  )

  t <- errors_df$t
  e_t <- errors_df$e
  b1 <- b[1]
  b0 <- b[2]
  y_t <- b1 * t + b0 + e_t

  data.frame(
    t = t,
    y = y_t,
    e = e_t,
    p_order = errors_df$p_order,
    q_order = errors_df$q_order,
    arma_label = errors_df$arma_label,
    stringsAsFactors = FALSE
  )
}



#' Create a lag matrix for recursive AR computations
#'
#' @description Builds a matrix where each row contains lagged values
#' `x_{t-1}, ..., x_{t-p}` for times `t = start_t, ..., end_t`.
#'
#' @param x Numeric input series.
#' @param p Integer number of lags.
#' @param start_t Integer first time index.
#' @param end_t Integer last time index.
#'
#' @return Numeric matrix with `p` lag columns (or zero columns when `p = 0`).
#'
#' @keywords internal
make_lag_matrix <- function(x, p, start_t, end_t) {
  if (p == 0L) {
    return(matrix(0, nrow = end_t - start_t + 1L, ncol = 0L))
  }
  
  # Build row-wise lag blocks used by AR terms.
  out <- matrix(0, nrow = end_t - start_t + 1L, ncol = p)
  for (s in seq_len(nrow(out))) {
    t <- start_t + s - 1L
    out[s, ] <- x[(t - 1L):(t - p)]
  }
  out
}

#' Compute ARMA residuals recursively
#'
#' @description Computes conditional residuals for an ARMA model by applying
#' AR and MA lag recursions across the series.
#'
#' @param x Numeric input series.
#' @param phi Numeric AR coefficient vector.
#' @param theta Numeric MA coefficient vector.
#'
#' @return Numeric vector of residuals with the same length as `x`.
#'
#' @keywords internal
arma_residuals <- function(x, phi, theta) {
  n <- length(x)
  p <- length(phi)
  q <- length(theta)
  m <- max(p, q)
  
  # Residual holder initialized to zero for early lag positions.
  ehat <- numeric(n)
  
  if (n <= m) return(ehat)
  
  # Recursive one-step residual update.
  for (t in (m + 1L):n) {
    ar_part <- if (p > 0L) sum(phi * x[(t - 1L):(t - p)]) else 0
    ma_part <- if (q > 0L) sum(theta * ehat[(t - 1L):(t - q)]) else 0
    ehat[t] <- x[t] - ar_part - ma_part
  }
  
  ehat
}

#' CLS objective for ARMA coefficients
#'
#' @description Evaluates conditional least squares loss for ARMA parameters.
#'
#' @param par Numeric parameter vector containing AR then MA coefficients.
#' @param x Numeric input series.
#' @param p Integer AR order.
#' @param q Integer MA order.
#'
#' @return Numeric scalar mean squared residual loss.
#'
#' @keywords internal
cls_loss_arma <- function(par, x, p, q) {
  phi <- if (p > 0L) par[1:p] else numeric(0)
  theta <- if (q > 0L) par[(p + 1L):(p + q)] else numeric(0)
  
  # Compute residual path and evaluate post-lag MSE.
  ehat <- arma_residuals(x, phi, theta)
  m <- max(p, q)
  
  if (length(x) <= m) return(Inf)
  mean(ehat[(m + 1L):length(x)]^2)
}

#' Compute block CLS loss and gradient
#'
#' @description Computes mini-batch loss and analytic gradient for ARMA
#' conditional least squares using a warmup window for recursion stability.
#'
#' @param x Numeric input series.
#' @param phi Numeric AR coefficient vector.
#' @param theta Numeric MA coefficient vector.
#' @param batch_start Integer first index of the optimization batch.
#' @param batch_end Integer last index of the optimization batch.
#' @param warmup Integer number of points used before `batch_start` for
#' recursion warmup.
#'
#' @return List with scalar `loss` and numeric vector `grad`.
#'
#' @keywords internal
cls_grad_block_vec <- function(x, phi, theta, batch_start, batch_end, warmup = 40L) {
  p <- length(phi)
  q <- length(theta)
  m <- max(p, q)
  
  if (batch_start < m + 1L) stop("batch_start must be at least max(p, q) + 1")
  if (batch_end > length(x)) stop("batch_end cannot exceed length(x)")
  if (batch_start > batch_end) stop("batch_start must be <= batch_end")
  
  start_t <- max(m + 1L, batch_start - warmup)
  end_t   <- batch_end
  Tlen    <- end_t - start_t + 1L
  
  # Local state for residual recursion and derivative recursion.
  ehat   <- numeric(Tlen)
  dphi   <- matrix(0, nrow = Tlen, ncol = p)
  dtheta <- matrix(0, nrow = Tlen, ncol = q)
  
  Xlag <- make_lag_matrix(x, p, start_t, end_t)
  
  # Forward pass over warmup + batch region.
  for (s in seq_len(Tlen)) {
    ar_part <- if (p > 0L) sum(phi * Xlag[s, ]) else 0
    
    ma_lags <- if (q > 0L) {
      vapply(seq_len(q), function(j) {
        prev_s <- s - j
        if (prev_s >= 1L) ehat[prev_s] else 0
      }, numeric(1))
    } else {
      numeric(0)
    }
    
    ma_part <- if (q > 0L) sum(theta * ma_lags) else 0
    ehat[s] <- x[start_t + s - 1L] - ar_part - ma_part
    
    if (p > 0L) {
      row_phi <- -Xlag[s, ]
      if (q > 0L) {
        for (j in seq_len(q)) {
          prev_s <- s - j
          if (prev_s >= 1L) row_phi <- row_phi - theta[j] * dphi[prev_s, ]
        }
      }
      dphi[s, ] <- row_phi
    }
    
    if (q > 0L) {
      row_theta <- -ma_lags
      for (j in seq_len(q)) {
        prev_s <- s - j
        if (prev_s >= 1L) row_theta <- row_theta - theta[j] * dtheta[prev_s, ]
      }
      dtheta[s, ] <- row_theta
    }
  }
  
  # Restrict loss/gradient to the requested batch indices.
  idx <- (batch_start:batch_end) - start_t + 1L
  resid <- ehat[idx]
  n_batch <- length(idx)
  
  grad_phi <- if (p > 0L) {
    (2 / n_batch) * colSums(dphi[idx, , drop = FALSE] * resid)
  } else {
    numeric(0)
  }
  
  grad_theta <- if (q > 0L) {
    (2 / n_batch) * colSums(dtheta[idx, , drop = FALSE] * resid)
  } else {
    numeric(0)
  }
  
  list(
    loss = mean(resid^2),
    grad = c(grad_phi, grad_theta)
  )
}

#' Fit ARMA parameters by SGD with momentum
#'
#' @description Optimizes ARMA conditional least squares parameters using
#' mini-batch SGD with momentum, gradient clipping, and early stopping.
#'
#' @param x Numeric input series.
#' @param p Integer AR order.
#' @param q Integer MA order.
#' @param init Numeric initial parameter vector.
#' @param lr Numeric learning rate.
#' @param momentum Numeric momentum factor.
#' @param batch_size Integer mini-batch size.
#' @param warmup Integer warmup length used for recursive state.
#' @param max_epochs Integer maximum training epochs.
#' @param clip Numeric gradient-norm clipping threshold.
#' @param shuffle_blocks Logical; whether to shuffle batches each epoch.
#' @param patience Integer early-stopping patience.
#' @param min_delta Numeric minimum improvement threshold.
#' @param verbose Logical; whether to print epoch loss.
#'
#' @return List containing fitted parameters, loss summary, and timing.
#'
#' @keywords internal
fit_cls_sgd_momentum <- function(x, p, q,
                                 init = rep(0, p + q),
                                 lr = 1e-3,
                                 momentum = 0.9,
                                 batch_size = 64L,
                                 warmup = 40L,
                                 max_epochs = 200L,
                                 clip = 5,
                                 shuffle_blocks = TRUE,
                                 patience = 30L,
                                 min_delta = 1e-8,
                                 verbose = FALSE) {
  n <- length(x)
  m <- max(p, q)
  
  if (n <= m) {
    stop("Series is too short for requested ARMA order.")
  }
  
  # Initialize optimizer state.
  par <- init
  vel <- numeric(length(par))
  
  best_par <- par
  best_loss <- Inf
  no_improve <- 0L
  loss_hist <- numeric(max_epochs)
  
  start_time <- proc.time()[3]
  
  # Epoch loop over mini-batches.
  for (epoch in seq_len(max_epochs)) {
    block_starts <- seq.int(m + 1L, n, by = batch_size)
    block_ends <- pmin(block_starts + batch_size - 1L, n)
    
    ord <- seq_along(block_starts)
    if (shuffle_blocks) ord <- sample(ord)
    
    for (z in ord) {
      batch_start <- block_starts[z]
      batch_end   <- block_ends[z]
      
      phi   <- if (p > 0L) par[1:p] else numeric(0)
      theta <- if (q > 0L) par[(p + 1L):(p + q)] else numeric(0)
      
      out <- cls_grad_block_vec(
        x = x,
        phi = phi,
        theta = theta,
        batch_start = batch_start,
        batch_end = batch_end,
        warmup = warmup
      )
      
      grad <- out$grad
      grad_norm <- sqrt(sum(grad^2))
      if (grad_norm > clip) grad <- grad * (clip / grad_norm)
      
      # Momentum parameter update.
      vel <- momentum * vel - lr * grad
      par <- par + vel
    }
    
    full_loss <- cls_loss_arma(par, x, p, q)
    loss_hist[epoch] <- full_loss
    
    if (full_loss < best_loss - min_delta) {
      best_loss <- full_loss
      best_par <- par
      no_improve <- 0L
    } else {
      no_improve <- no_improve + 1L
    }
    
    if (verbose) {
      cat(sprintf("Epoch %3d | CLS loss = %.6f\n", epoch, full_loss))
    }
    
    if (no_improve >= patience) {
      loss_hist <- loss_hist[seq_len(epoch)]
      break
    }
  }
  
  elapsed <- proc.time()[3] - start_time
  
  list(
    par = best_par,
    phi = if (p > 0L) best_par[1:p] else numeric(0),
    theta = if (q > 0L) best_par[(p + 1L):(p + q)] else numeric(0),
    loss = cls_loss_arma(best_par, x, p, q),
    loss_hist = loss_hist[loss_hist != 0],
    elapsed_sec = elapsed
  )
}

#' Reconstruct fitted ARMA component from residual recursion
#'
#' @description Rebuilds one-step fitted ARMA values from AR coefficients,
#' MA coefficients, and a residual sequence.
#'
#' @param x Numeric ARMA-target series.
#' @param phi Numeric AR coefficient vector.
#' @param theta Numeric MA coefficient vector.
#' @param resid Numeric residual vector.
#'
#' @return Numeric vector of fitted ARMA component values.
#'
#' @keywords internal
arma_fitted_from_resid <- function(x, phi, theta, resid) {
  n <- length(x)
  p <- length(phi)
  q <- length(theta)
  m <- max(p, q)
  
  # Keep pre-lag region as NA and fill valid fitted range recursively.
  fitted <- rep(NA_real_, n)
  if (n <= m) return(fitted)
  
  for (t in (m + 1L):n) {
    ar_part <- if (p > 0L) sum(phi * x[(t - 1L):(t - p)]) else 0
    ma_part <- if (q > 0L) sum(theta * resid[(t - 1L):(t - q)]) else 0
    fitted[t] <- ar_part + ma_part
  }
  
  fitted
}


#' Fit an ARMA Model to Data
#'
#' @description Fits an ARMA model of order `p_order` and `q_order`. If
#' `data` is a data frame with a `t` column, `t` is included as a linear trend
#' regressor and ARMA is fit to the detrended response using SGD with momentum.
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
  y <- if (is.data.frame(data)) data$y else as.numeric(data)
  n <- length(y)
  
  # Remove linear trend first, then fit ARMA on detrended response.
  if (is.data.frame(data) && "t" %in% names(data)) {
    t_vec <- data$t
    trend_fit <- lm(y ~ t_vec)
    trend_coef <- as.numeric(coef(trend_fit))   # intercept, slope
    trend_hat <- as.numeric(fitted(trend_fit))
    x_for_arma <- y - trend_hat
  } else {
    t_vec <- NULL
    trend_coef <- NULL
    trend_hat <- rep(0, n)
    x_for_arma <- y
  }
  
  # Optimize ARMA parameters via CLS-SGD.
  opt <- fit_cls_sgd_momentum(
    x = x_for_arma,
    p = p_order,
    q = q_order,
    init = rep(0.01, p_order + q_order),
    lr = 1e-3,
    momentum = 0.9,
    batch_size = 32L,
    warmup = 20L,
    max_epochs = 200L,
    clip = 5,
    shuffle_blocks = TRUE,
    patience = 30L,
    verbose = FALSE
  )
  
  # Rebuild fitted values and residuals on original y scale.
  resid_arma <- arma_residuals(x_for_arma, opt$phi, opt$theta)
  fitted_arma <- arma_fitted_from_resid(x_for_arma, opt$phi, opt$theta, resid_arma)
  fitted_full <- trend_hat + fitted_arma
  resid_full <- y - fitted_full
  
  fit <- list(
    coefficients = c(
      if (!is.null(trend_coef)) c(intercept = trend_coef[1], t = trend_coef[2]) else numeric(0),
      stats::setNames(opt$phi, paste0("ar", seq_along(opt$phi))),
      stats::setNames(opt$theta, paste0("ma", seq_along(opt$theta)))
    ),
    residuals = resid_full,
    arma_residuals = resid_arma,
    fitted.values = fitted_full,
    trend_fitted = trend_hat,
    trend_coef = trend_coef,
    phi = opt$phi,
    theta = opt$theta,
    loss = opt$loss,
    loss_hist = opt$loss_hist,
    elapsed_sec = opt$elapsed_sec,
    p_order = p_order,
    q_order = q_order,
    fit_label = paste0("ARMA(", p_order, ", ", q_order, ") fit"),
    x = y,
    x_detrended = x_for_arma
  )
  
  class(fit) <- "ARMA_SGD_fit"
  fit
}

#' Extract residuals from an ARMA_SGD_fit object
#'
#' @description Returns full-series residuals stored in a fitted
#' `ARMA_SGD_fit` object.
#'
#' @param object An object returned by `fit_ARMA()`.
#' @param ... Additional arguments (unused).
#'
#' @return Numeric residual vector.
#' @export
residuals.ARMA_SGD_fit <- function(object, ...) {
  object$residuals
}

#' Estimate ARMA error series from fitted model output
#'
#' @description Extracts and length-aligns model residuals so they can be used
#' as ARMA error estimates for plotting or diagnostics.
#'
#' @param data Data frame or numeric series used as target alignment reference.
#' @param fit Fitted model object supplying residuals.
#'
#' @return Numeric error vector aligned to `data` length.
estimate_ARMA_errors <- function(data, fit) {
  if (is.null(fit)) {
    stop("`fit` must be provided to estimate ARMA errors.")
  }

  if (is.data.frame(data)) {
    if (!"y" %in% names(data)) {
      stop("For data-frame input, `data` must include a `y` column.")
    }
    target_len <- nrow(data)
  } else {
    target_len <- length(as.numeric(data))
  }

  # Extract residuals and align to target length.
  arma_errors <- as.numeric(residuals(fit))
  if (length(arma_errors) == 0) {
    stop("Could not extract residuals from `fit`.")
  }

  if (length(arma_errors) == target_len) {
    return(arma_errors)
  }

  if (length(arma_errors) > target_len) {
    return(tail(arma_errors, target_len))
  }

  c(rep(NA_real_, target_len - length(arma_errors)), arma_errors)
}

#' Plot ARMA series over time
#'
#' @description Plots ARMA data or errors from either a generated ARMA data
#' frame or a numeric vector and overlays fitted values if provided.
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
  has_observed_errors <- FALSE

  if (is.data.frame(x)) {
    t <- if ("t" %in% names(x)) x$t else seq_len(nrow(x))
    if (series == "e") {
      if ("e" %in% names(x) && any(is.finite(x$e))) {
        y_obs <- x$e
        has_observed_errors <- TRUE
      } else if (!is.null(fit)) {
        y_obs <- estimate_ARMA_errors(x, fit)
      } else {
        stop("`series = 'e'` requires an `e` column or a fitted model `fit`.")
      }
    } else {
      if (!"y" %in% names(x)) {
        stop("`series = 'y'` requires a `y` column for data-frame input.")
      }
      y_obs <- x$y
    }
  } else {
    y_obs <- as.numeric(x)
    t <- seq_along(y_obs)
    if (series == "e") {
      if (is.null(fit)) {
        stop("For numeric input with `series = 'e'`, provide a fitted model `fit`.")
      }
      y_obs <- estimate_ARMA_errors(y_obs, fit)
    }
  }

  y_axis <- if (series == "e") "ARMA Errors" else "y"
  plot_type <- if (series == "e") "l" else "p"
  plot_pch <- if (series == "e") NA else 16
  fit_col <- if (series == "e") "red" else "blue"

  if (!is.null(fit) && is.null(fitted_vals)) {
    if (series == "e") {
      if (has_observed_errors) {
        fitted_vals <- estimate_ARMA_errors(x, fit)
      }
    } else {
      fitted_vals <- as.numeric(y_obs - estimate_ARMA_errors(y_obs, fit))
    }
  }

  if (!is.null(fitted_vals)) {
    fitted_vals <- as.numeric(fitted_vals)
    if (length(fitted_vals) > length(t)) {
      fitted_vals <- tail(fitted_vals, length(t))
    } else if (length(fitted_vals) < length(t)) {
      fitted_vals <- c(rep(NA_real_, length(t) - length(fitted_vals)), fitted_vals)
    }
  }

  dgp_label <- if (is.data.frame(x) && all(c("p_order", "q_order") %in% names(x))) {
    paste0("ARMA(", x$p_order[1], ", ", x$q_order[1], ") dgp")
  } else if (series == "e") {
    "ARMA errors"
  } else {
    "ARMA data"
  }

  fit_line_label <- if (!is.null(fitted_vals) || !is.null(fit)) {
    if (!is.null(fit) && !is.null(fit$fit_label)) fit$fit_label else "fit"
  } else {
    NULL
  }

  plot_title <- if (!is.null(fit_line_label)) {
    paste(dgp_label, "|", fit_line_label)
  } else {
    dgp_label
  }

  plot(t, y_obs, type = plot_type, pch = plot_pch, cex = 0.7, xlab = "Time", ylab = y_axis, main = plot_title)
  if (!is.null(fitted_vals)) {
    lines(t, fitted_vals, col = fit_col, lwd = 2)
    legend_labels <- if (series == "e") {
      c(expression(e), expression(hat(e)))
    } else {
      c(expression(y), expression(hat(y)))
    }
    legend("topleft", legend = legend_labels, col = c("black", fit_col), pch = c(16, NA), lty = c(NA, 1), bty = "n")
  }
}
