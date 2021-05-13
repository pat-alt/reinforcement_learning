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
  update_every=1,
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
    update_every=update_every,
    action_values_ = action_values
  )

  # Output: ----
  output[["mab"]] <- mab
  class(output) <- "policy"
  return(output)
}
