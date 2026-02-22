Model <- function(meta = list()) {
  obj <- list(
    coefficients = NULL,
    fitted = FALSE,
    formula = NULL,
    meta = meta
  )
  class(obj) <- "Model"
  obj
}

print.Model <- function(x, ...) {
  cat("<Model>\n")
  cat("  fitted:", x$fitted, "\n")
  cat("  formula:", x$formula, "\n")
  cat("  coefficients:\n")
  print(x$coefficients)

  invisible(x)
}

generate_data.Model <- function(model, formula, ...) {
  invisible(NULL)
}

fit.Model <- function(model, formula, data, ...) {
  model
}

plot.Model <- function(x, data = NULL, formula = NULL, ...) {
  invisible(NULL)
}
