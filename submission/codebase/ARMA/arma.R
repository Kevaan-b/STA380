#' Generate ARMA(p, q) Errors
#'
#' @description Simulates an ARMA error process of order `length(p)` and
#' `length(q)` using coefficient vectors `p` and `q` with Gaussian innovations.
#'
#' @param n Integer number of returned errors after burn-in.
#' @param p Numeric vector of AR coefficients.
#' @param q Numeric vector of MA coefficients.
#' @param sigma Numeric standard deviation of the white-noise innovations.
#' @param burnin Integer number of initial simulated points to discard.
#' @param seed Optional integer random seed for reproducibility.
#'
#' @return Data frame with columns `t`, `e`, `p_order`, `q_order`, and
#' `arma_label`.
#'
#' @examples
#' e <- generate_ARMA_errors(n = 20, p = c(0.45), q = c(0.3, -0.1), sigma = 1, burnin = 50)
#'
#' @importFrom stats rnorm
#' @export
generate_ARMA_errors <- function(n, p = numeric(0), q = numeric(0), sigma = 1, burnin = 100, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

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
#' @param burnin Integer number of initial simulated points to discard.
#' @param b Numeric vector of length 2 where `b[1] = b1` and `b[2] = b0`.
#' @param seed Optional integer random seed for reproducibility.
#'
#' @return Data frame with columns `t`, `y`, `e`, `p_order`, `q_order`,
#' and `arma_label`.
#'
#' @examples
#' d <- generate_ARMA_dataset(n = 300, p = c(0.4), q = c(0.25, -0.1), sigma = 1.5, burnin = 120, b = c(0.04, 2))
#'
#' @export
generate_ARMA_dataset <- function(n, p = numeric(0), q = numeric(0), sigma = 1, burnin = 100, b = c(0, 0), seed = NULL) {
  errors_df <- generate_ARMA_errors(
    n = n,
    p = p,
    q = q,
    sigma = sigma,
    burnin = burnin,
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

#' Fit an ARMA Model to Data
#'
#' @description Fits an ARMA model of order `p` and `q_order`. If
#' `data` is a data frame with a `t` column, `t` is included as an external regressor.
#'
#' @param data Data frame containing at least `y` (and optionally `t`), or a
#'   numeric response vector.
#' @param p_order Integer AR order.
#' @param q_order Integer MA order.
#'
#' @return An object of class Arima.
#'
#' @examples
#' fit <- fit_ARMA(d, p_order = 1, q_order = 2)
#'
#' @importFrom stats arima
#' @export
fit_ARMA <- function(data, p_order = 0, q_order = 0) {
  y <- if (is.data.frame(data)) data$y else as.numeric(data)

  if (is.data.frame(data) && "t" %in% names(data)) {
    fit <- arima(y, order = c(p_order, 0, q_order), xreg = data$t)
  } 
  
  else {
    fit <- arima(y, order = c(p_order, 0, q_order))
  }

  # Keep lightweight model info for plotting labels.
  fit$fit_label <- paste0("ARMA(", p_order, ", ", q_order, ") fit")
  fit$p_order <- p_order
  fit$q_order <- q_order
  fit
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
#' plot_ARMA_series(d, series = "y")
#' plot_ARMA_series(d, series = "e", fit = fit)
#' plot_ARMA_series(d$y, series = "y")
#'
#' @importFrom graphics legend lines plot
#' @export
plot_ARMA_series <- function(x, series = c("y", "e"), fitted_vals = NULL, fit = NULL) {
  series <- match.arg(series)

  if (is.data.frame(x)) {
    t <- x$t
    y_obs <- if (series == "e") x$e else x$y
  } else {
    y_obs <- as.numeric(x)
    t <- seq_along(y_obs)
  }

  y_axis <- if (series == "e") "ARMA Errors" else "y"
  plot_type <- if (series == "e") "l" else "p"
  plot_pch <- if (series == "e") NA else 16
  fit_col <- if (series == "e") "red" else "blue"

  if (!is.null(fit) && is.null(fitted_vals)) {
    if (series == "e") {
      fitted_vals <- as.numeric(residuals(fit))
    } else {
      fitted_vals <- as.numeric(y_obs - residuals(fit))
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

