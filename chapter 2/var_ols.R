#' Variance of OLS Coefficients
#'
#' Computes the variance-covariance matrix of OLS coefficients.
#' @param X A matrix of covariates.
#' @param sigma2 The variance of residuals.
#' @return The variance-covariance matrix.
var_ols <- function(X, sigma2) {
  sigma2 * solve(t(X) %*% X)
}