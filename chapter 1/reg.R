#' @param X A matrix of covariates.
#' @param y A vector of responses.
#' @return A list with the regression function and residuals.
reg_func <- function(X, y) {
  beta <- solve(t(X) %*% X) %*% t(X) %*% y
  fitted <- X %*% beta
  residuals <- y - fitted
  list(
    coefficients = beta,
    fitted = fitted,
    residuals = residuals
  )
}