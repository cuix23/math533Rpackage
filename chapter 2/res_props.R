#' Properties of Residuals
#'
#' Verifies key properties of residuals (orthogonality and centering).
#' @param X A matrix of covariates.
#' @param res A vector of residuals.
#' @return A list verifying orthogonality and centering properties.
res_props <- function(X, res) {
  list(
    orthogonality = all(abs(colSums(X * res)) < 1e-6),
    centered = abs(mean(res)) < 1e-6
  )
}