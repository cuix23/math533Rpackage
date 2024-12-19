setup_regression <- function(X, y) {
  list(
    covariates = X,
    response = y,
    dimension = ncol(X)
  )
}