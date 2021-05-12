library(Rcpp)
sourceCpp("R/ucb.cpp")
#' UCB (Rcpp comiled)
#'
#' @param mab An object of class `mab`.
#' @param cpp_fun The compiled C++ code that runs the UCB algorithm.
#' @param action_values Initial action value estimates (optional).
#'
#' @author Patrick Altmeyer
ucb_Rcpp <- function(
  mab,
  cpp_fun=ucb,
  action_values = NULL
) {

  # Parameters: ----
  unpack(mab) # unpack bandit problem

  # Execute C++ function: ----
  output <- cpp_fun(
    horizon = horizon,
    v_star = v_star,
    K = K,
    prob=prob,
    action_values_ = action_values
  )

  # Output: ----
  output <- c(output, mab=mab)
  class(output) <- "policy"
  return(output)
}
