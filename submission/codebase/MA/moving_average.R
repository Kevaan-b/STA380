#' Generate MA(q) Errors
#'
#' @description Simulates a moving-average error process of order `length(q)`
#' using coefficient vector `q` and Gaussian white noise.
#'
#' @param n Integer number of returned errors after burn-in.
#' @param q Numeric vector of MA coefficients.
#' @param sigma Numeric standard deviation of the white-noise innovations.
#' @param burnin Integer number of initial simulated points to discard.
#'
#' @return Numeric vector of length `n` containing simulated MA errors.
#'
#' @examples
#' e <- generate_MA_errors(n = 20, q = c(0.5, -0.2), sigma = 10, burnin = 50)
#'
#' @importFrom stats rnorm
#' @export
generate_MA_errors <- function(n, q, sigma, burnin) {
  total_n <- n + burnin
  noise <- rnorm(total_n, mean = 0, sd = sigma)
  theta <- q
  q_order <- length(theta)

  e <- noise
  if (q_order > 0) {
    for (t in seq_len(total_n)) {
      for (j in seq_len(q_order)) {
        if (t - j > 0) {
          e[t] <- e[t] + theta[j] * noise[t - j]
        }
      }
    }
  }

  tail(e, n)
}

#' Generate a synthetic MA dataset with linear trend
#'
#' @description Generate a synthetic MA dataset with linear trend of form:
#' `y_t = b1 * t + b0 + e_t`.
#'
#' @param n Integer number of returned observations.
#' @param q Numeric vector of MA coefficients.
#' @param sigma Numeric standard deviation of the white-noise innovations.
#' @param burnin Integer number of initial simulated points to discard.
#' @param b Numeric vector of length 2 where `b[1] = b1` and `b[2] = b0`.
#'
#' @return Data frame with columns `t`, `y`, and `e`.
#'
#' @examples
#' d <- generate_MA_dataset(n = 300, q = c(0.55, -0.25), sigma = 1.5, burnin = 120, b = c(0.04, 2))
#'
#' @export
generate_MA_dataset <- function(n, q, sigma, burnin, b) {
  e_t <- generate_MA_errors(n, q, sigma, burnin)
  t <- seq_len(n)
  b1 <- b[1]
  b0 <- b[2]
  y_t <- b1 * t + b0 + e_t
  data.frame(t = t, y = y_t, e = e_t)
}

#' Fit an MA Model to Data
#'
#' @description Fits an MA model of order `length(q)`. If `data` is a
#' data frame with a `t` column, `t` is included as an external regressor.
#'
#' @param data Data frame containing at least `y` (and optionally `t`), or a
#'   numeric response vector.
#' @param q Numeric vector whose length defines MA order.
#'
#' @return An object of class Arima.
#'
#' @examples
#' fit <- fit_MA(d, q = c(0.55, -0.25))
#'
#' @importFrom stats arima
#' @export
fit_MA <- function(data, q) {
  y <- if (is.data.frame(data)) data$y else as.numeric(data)
  if (is.data.frame(data) && "t" %in% names(data)) {
    arima(y, order = c(0, 0, length(q)), xreg = data$t)
  } else {
    arima(y, order = c(0, 0, length(q)))
  }
}

#' Plot MA errors over time
#'
#' @description Plots observed MA errors as a line and overlays
#' fitted errors if provided.
#'
#' @param e Numeric vector of observed errors.
#' @param e_hat Optional numeric vector of fitted/estimated errors.
#'
#' @return No return value; called for side effects.
#'
#' @examples
#' plot_MA_errors(e)
#' plot_MA_errors(e, e_hat = e + rnorm(length(e), sd = 0.1))
#'
#' @importFrom graphics legend lines plot
#' @importFrom stats rnorm
#' @export
plot_MA_errors <- function(e, e_hat = NULL) {
  t <- seq_along(e)
  plot(t, e, type = "l", xlab = "Time", ylab = "MA Errors", main = "MA Errors vs Time")
  if (!is.null(e_hat)) {
    lines(t, e_hat, col = "red", lwd = 2)
    legend("topleft", legend = c("Observed e", "Fitted e"), col = c("black", "red"), lty = 1, bty = "n")
  }
}

#' Plot dataset values over time
#'
#' @description Plots observed `y` as points and overlays fitted
#' values as a line if provided.
#'
#' @param y Either a data frame with columns `t` and `y`, or a numeric vector
#'   of observed values.
#' @param y_hat Optional numeric vector of fitted values.
#'
#' @return No return value; called for side effects.
#'
#' @examples
#' plot_MA_dataset(d)
#' plot_MA_dataset(d, y_hat = fitted(fit))
#'
#' @importFrom graphics legend lines plot
#' @export
plot_MA_dataset <- function(y, y_hat = NULL) {
  if (is.data.frame(y)) {
    t <- y$t
    y_obs <- y$y
  } else {
    y_obs <- as.numeric(y)
    t <- seq_along(y_obs)
  }
  plot(t, y_obs, type = "p", pch = 16, cex = 0.7, xlab = "Time", ylab = "y", main = "MA Dataset vs Time")
  if (!is.null(y_hat)) {
    lines(t, y_hat, col = "blue", lwd = 2)
    legend("topleft", legend = c("Observed y", "Fitted y"), col = c("black", "blue"), pch = c(16, NA), lty = c(NA, 1), bty = "n")
  }
}
