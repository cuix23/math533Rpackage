#' Design Matrix
#'
#' Constructs the design matrix from given covariates.
#' @param covariates A matrix of covariates.
#' @return The design matrix including an intercept column.
design_mat <- function(covariates) {
  intercept <- matrix(1, nrow = nrow(covariates), ncol = 1)
  cbind(intercept, covariates)
}