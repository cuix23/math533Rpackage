#' Proposition 3: Semi-parametric and Parametric Models
#'
#' Classifies the regression model based on design and residuals.
#' @param design_type A string specifying "random" or "fixed" design.
#' @param residual_type A string specifying "Gaussian" or "non-Gaussian" residuals.
#' @return A string classifying the model as "parametric" or "semi-parametric".
class_model <- function(design_type, residual_type) {
  if (design_type == "fixed" && residual_type == "Gaussian") {
    return("parametric")
  } else if (design_type == "random") {
    return("semi-parametric")
  } else {
    return("non-parametric")
  }
}