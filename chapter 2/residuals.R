
#' Residuals
#'
#' Computes residuals from the OLS fit.
#' @param X A matrix of covariates.
#' @param y A vector of responses.
#' @param beta A vector of coefficients.
#' @return A vector of residuals.
residuals_ols <- function(X, y, beta) {
  y - X %*% beta
}

#' RSS (Residual Sum of Squares)
#'
#' Computes the Residual Sum of Squares (RSS).
#' @param res A vector of residuals.
#' @return The RSS value.
rss <- function(res) {
  sum(res^2)
}