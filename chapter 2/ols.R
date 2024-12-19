#' OLS Loss Function
#'
#' Computes the OLS loss for a given beta.
#' @param X A matrix of covariates.
#' @param y A vector of responses.
#' @param beta A vector of coefficients.
#' @return The OLS loss value.
ols_loss <- function(X, y, beta) {
  sum((y - X %*% beta)^2)
}