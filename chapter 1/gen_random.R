#' Random Design Realizations
#' 
#' Generates random design data realizations.
#' @param n Number of samples.
#' @param beta True coefficients.
#' @param noise_sd Standard deviation of noise.
#' @return A data frame with covariates and responses.
gen_random <- function(n, beta, noise_sd) {
  x <- runif(n, 0, 1)
  X <- cbind(1, x)
  y <- X %*% beta + rnorm(n, mean = 0, sd = noise_sd)
  data.frame(X1 = X[, 2], y = y)
}