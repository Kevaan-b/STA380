plot_fit_1d <- function(x, y, yhat, xlab = "x", ylab = "y", ...) {
  ord <- order(x)
  plot(x, y, xlab = xlab, ylab = ylab)
  lines(x[ord], yhat[ord], col = "red", lwd = 2)
}

plot_observed_vs_fitted <- function(y, yhat, ...) {p
  plot(y, yhat, xlab = "Observed", ylab = "Fitted")
  abline(0, 1, col = "gray", lty = 2)
}
