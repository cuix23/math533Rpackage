#' Ordinary Least Squares (OLS) Estimation
#'
#' @param X A numeric matrix of predictors.
#' @param y A numeric vector of responses.
#' @return A list containing coefficients, fitted values, and residuals.
ols_fit <- function(X, y) {
  beta <- solve(t(X) %*% X) %*% t(X) %*% y
  fitted <- X %*% beta
  residuals <- y - fitted
  
  list(coefficients = beta, fitted = fitted, residuals = residuals)
}
