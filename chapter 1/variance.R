#' @param y Response vector.
#' @param fitted Fitted values from the regression function.
#' @return A list with total, signal, and noise variances.
var_decomp <- function(y, fitted) {
  total_var <- var(y)
  signal_var <- var(fitted)
  noise_var <- total_var - signal_var
  
  list(
    total_variance = total_var,
    signal_variance = signal_var,
    noise_variance = noise_var
  )
}