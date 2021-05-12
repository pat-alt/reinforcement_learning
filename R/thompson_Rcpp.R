library(Rcpp)
sourceCpp("R/thompson.cpp")
#' Thompson Sampling (Rcpp comiled)
#'
#' @param mab An object of class `mab`.
#' @param cpp_fun The compiled C++ code that runs the Thompson Sampling algorithm.
#' @param successes Initial success counts (optional).
#' @param failures Initial failure counts (optional).
#'
#' @author Patrick Altmeyer
thompson_Rcpp <- function(
  mab,
  cpp_fun=thompson,
  successes = NULL,
  failures = NULL
) {

  # Parameters: ----
  unpack(mab) # unpack bandit problem

  # Execute C++ function: ----
  output <- cpp_fun(
    horizon = horizon,
    v_star = v_star,
    K = K,
    prob=prob,
    successes = successes,
    failures = failures
  )

  # Output: ----
  output <- c(output, mab=mab)
  class(output) <- "policy"
  return(output)
}
