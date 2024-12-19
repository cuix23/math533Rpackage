
#' OLS Estimator
#'
#' Computes the OLS estimator for beta.
#' @param X A matrix of covariates.
#' @param y A vector of responses.
#' @return A vector of estimated coefficients.
ols_est <- function(X, y) {
  solve(t(X) %*% X) %*% t(X) %*% y
}
