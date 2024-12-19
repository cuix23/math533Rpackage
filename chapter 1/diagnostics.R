#' Plot Residuals vs Fitted Values
#'
#' @param model A list returned by ols_fit.
plot_residuals <- function(model) {
  plot(model$fitted, model$residuals,
       xlab = "Fitted Values", ylab = "Residuals",
       main = "Residuals vs Fitted")
  abline(h = 0, col = "red", lty = 2)
}

#' Check Orthogonality of Residuals
#'
#' @param X Design matrix.
#' @param residuals Residuals from the model.
check_orthogonality <- function(X, residuals) {
  ortho <- t(X) %*% residuals
  return(ortho)  # Should be close to zero
}
