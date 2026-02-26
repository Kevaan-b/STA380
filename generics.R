generate_data <- function(model, formula, ...) {
  UseMethod("generate_data")
}

fit <- function(model, formula, data, ...) {
  UseMethod("fit")
}
