#' Hat Matrix
#'
#' Computes the hat matrix for a given design matrix.
#' @param X A matrix of covariates.
#' @return The hat matrix.
hat_mat <- function(X) {
  X %*% solve(t(X) %*% X) %*% t(X)
}

#' Annihilator Matrix
#'
#' Computes the annihilator matrix (I - H).
#' @param H The hat matrix.
#' @return The annihilator matrix.
annih_mat <- function(H) {
  diag(nrow(H)) - H
}

#' Leverage Values
#'
#' Computes the leverage values (diagonal of the hat matrix).
#' @param H The hat matrix.
#' @return A vector of leverage values.
leverage <- function(H) {
  diag(H)
}