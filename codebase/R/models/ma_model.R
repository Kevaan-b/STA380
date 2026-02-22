MAModel <- function(q = 1, meta = list()) {
  obj <- Model(meta = meta)
  obj$q <- q
  obj$model_type <- "ma"
  class(obj) <- c("MAModel", "Model")
  obj
}

generate_data.MAModel <- function(model, formula, ...) {
  stop("not implemented", call. = FALSE)
}

fit.MAModel <- function(model, formula, data, ...) {
  stop("not implemented", call. = FALSE)
}

plot.MAModel <- function(x, data = NULL, ...) {
  stop("not implemented", call. = FALSE)
}
