
#' Generate an AR(m) Data Generating Process
#' @description Simulates a stationary AR(m) time series. If phi is not supplied,
#' stationary AR coefficients are found via rejection sampling. If phi is supplied,
#' it is used directly without a stationarity check, allowing explosive processes.
#' @param m integer order of the AR process. Default is 2.
#' @param variance numeric variance of the Gaussian noise. Default is 2.
#' @param ar_mean numeric mean of the Gaussian noise. Default is 0.
#' @param n integer number of observations to generate. Default is 50.
#' @param seed integer random seed for reproducibility. Default is 1.
#' @param phi optional numeric vector of AR coefficients of length m. If NULL,
#' coefficients are sampled via rejection sampling until stationarity is achieved.
#' @return a list with components \code{x} (the simulated time series as a ts object)
#' and \code{phi} (the AR coefficients used).
#' @examples
#' # Random stationary AR(2)
#' dgp <- generate_ar_dgp(m = 2, variance = 2, n = 50, seed = 1)
#'
#' # User-supplied coefficients
#' dgp <- generate_ar_dgp(m = 2, variance = 2, n = 50, seed = 1, phi = c(0.5, 0.3))
#' @importFrom stats rnorm filter
#' @export
generate_ar_dgp <- function(m = 2, variance = 2, ar_mean = 0, n = 50, 
                            seed = 1, phi = NULL) {
  if (!is.null(phi)) {
    if (length(phi) != m) {
      stop(paste("phi has length", length(phi), "but m =", m,
                 "-- please ensure length(phi) == m"))
    }
  } else {
    stationary <- FALSE
    while (!stationary) {
      phi <- runif(m, -1, 1)
      roots <- polyroot(c(1, -phi))
      stationary <- all(Mod(roots) > 1)
    }
  }
  initialize <- rep(0, m)
  set.seed(seed)
  noise <- rnorm(n, ar_mean, variance)
  x <- filter(noise, filter = phi, method = "recursive", init = initialize)
  plot(x, type = "l", xlab = "Time", ylab = "x",
       main = paste("AR(", m, ") time series", sep = ""))
  return(list(x = x, phi = phi))
}

#' Generate a User-Specified AR(p) Time Series
#' @description Simulates an AR(p) time series using user-supplied AR coefficients.
#' No stationarity check is performed, so explosive processes are permitted.
#' @param phi numeric vector of AR coefficients. The order p is inferred as
#' \code{length(phi)}.
#' @param variance numeric variance of the Gaussian noise. Default is 2.
#' @param ar_mean numeric mean of the Gaussian noise. Default is 0.
#' @param n integer number of observations to generate. Default is 50.
#' @param seed integer random seed for reproducibility. Default is 9999.
#' @return a ts object of length \code{n} containing the simulated series.
#' @examples
#' x_user <- generate_user_ar(phi = c(0.5, 0.4), variance = 2, n = 50, seed = 9999)
#' @importFrom stats rnorm filter
#' @export
generate_user_ar <- function(phi, variance = 2, ar_mean = 0, n = 50, seed = 9999) {
  p <- length(phi)
  initialize <- rep(0, p)
  set.seed(seed)
  noise <- rnorm(n, ar_mean, variance)
  x_user <- filter(noise, filter = phi, method = "recursive", init = initialize)
  return(x_user)
}

#' Fit an AR(p) Model to a DGP Series
#' @description Fits an AR(p) model via maximum likelihood using \code{arima()}
#' to the series stored in a DGP list object, without estimating a mean term.
#' @param dgp a list with at least a component \code{x} containing the time series
#' to fit, as returned by \code{generate_ar_dgp()}.
#' @param p integer order of the AR model to fit.
#' @return a list with components \code{fit} (the fitted \code{arima} object) and
#' \code{fitted_vals} (the fitted values of the series).
#' @examples
#' dgp <- generate_ar_dgp(m = 2, variance = 2, n = 50, seed = 1)
#' result <- fit_user_ar(dgp, p = 2)
#' @importFrom stats arima fitted
#' @export
fit_user_ar <- function(dgp, p) {
  fit_user <- arima(dgp$x, order = c(p, 0, 0), include.mean = FALSE)
  fitted_user <- fitted(fit_user)
  return(list(fit = fit_user, fitted_vals = fitted_user))
}
