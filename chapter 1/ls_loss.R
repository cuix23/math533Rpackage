#' Computes the least squares loss function for regression.
#' @param X A matrix of covariates.
#' @param y A vector of responses.
#' @param beta Coefficients for the model.
#' @return The least squares loss value.
ls_loss <- function(X, y, beta) {
  residuals <- y - X %*% beta
  loss <- sum(residuals^2) / length(y)
  loss
}