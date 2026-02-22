LinearModel <- function(meta = list()) {
  obj <- Model(meta = meta)
  obj$model_type <- "linear"
  class(obj) <- c("LinearModel", "Model")
  obj
}

generate_data.LinearModel <- function(model, formula, ...) {
  stop("not implemented", call. = FALSE)
}

fit.LinearModel <- function(model, formula, data, ...) {
  stop("not implemented", call. = FALSE)
}

plot.LinearModel <- function(x, data = NULL, ...) {
  stop("not implemented", call. = FALSE)
}
