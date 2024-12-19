#' Theorem 1: Best Prediction under L2 Loss
#' 
#' Verifies that m(x) minimizes the L2 risk.
#' @param X A matrix of covariates.
#' @param y A vector of responses.
#' @return The orthogonality check (residuals orthogonal to predictors).
check_best_pred <- function(X, y) {
  model <- reg_func(X, y)
  residuals <- model$residuals
  orthogonality <- t(X) %*% residuals
  list(
    residuals = residuals,
    orthogonality = orthogonality  # Should be close to zero
  )
}