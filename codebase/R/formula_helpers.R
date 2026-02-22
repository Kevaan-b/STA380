make_xy <- function(formula, data) {
  mf <- model.frame(formula, data = data)
  y <- model.response(mf)
  X <- model.matrix(formula, data = mf)

  list(X = X, y = y, data = df, model_frame = mf)
}

make_x <- function(formula, data) {
  rhs_terms <- delete.response(terms(formula))
  X <- model.matrix(rhs_terms, data = data)

  list(X = X, data = data)
}
