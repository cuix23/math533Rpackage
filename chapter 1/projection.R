#' Compute Hat Matrix
#'
#' @param X Design matrix.
hat_matrix <- function(X) {
  H <- X %*% solve(t(X) %*% X) %*% t(X)
  return(H)
}

#' Verify Projection Theorem
#'
#' @param y Response vector.
#' @param fitted Fitted values from the model.
verify_projection <- function(y, fitted) {
  residuals <- y - fitted
  projection <- fitted + residuals
  
  all.equal(y, projection)  # Should return TRUE
}
