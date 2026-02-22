ARModel <- function(p = 1, meta = list()) {
  obj <- Model(meta = meta)
  obj$p <- p
  obj$model_type <- "ar"
  class(obj) <- c("ARModel", "Model")
  obj
}

generate_data.ARModel <- function(model, formula, ...) {
  stop("not implemented", call. = FALSE)
}

fit.ARModel <- function(model, formula, data, ...) {
  stop("not implemented", call. = FALSE)
}

plot.ARModel <- function(x, data = NULL, ...) {
  stop("not implemented", call. = FALSE)
}