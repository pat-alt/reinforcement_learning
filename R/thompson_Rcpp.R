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
  update_every = 1,
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
    update_every = update_every,
    successes = successes,
    failures = failures
  )

  # Output: ----
  output[["mab"]] <- mab
  class(output) <- "policy"
  return(output)
}
