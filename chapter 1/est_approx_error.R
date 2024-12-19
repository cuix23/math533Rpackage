#' Estimation and Approximation Errors
#' 
#' Demonstrates estimation and approximation errors in regression.
#' @param X A matrix of covariates.
#' @param y A vector of responses.
#' @param candidates A list of candidate models.
#' @return A list with estimation and approximation errors.
est_approx_error <- function(X, y, candidates) {
  model <- reg_func(X, y)
  m_hat <- model$fitted
  estimation_error <- sapply(candidates, function(f) {
    mean((y - f(X))^2) - mean((y - m_hat)^2)
  })
  approximation_error <- sapply(candidates, function(f) {
    mean((m_hat - f(X))^2)
  })
  list(
    estimation_error = estimation_error,
    approximation_error = approximation_error
  )
}