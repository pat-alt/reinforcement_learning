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
  failures = NULL,
  discount_factor = 1
) {

  # Parameters: ----
  unpack(mab) # unpack bandit problem

  # Execute C++ function: ----
  if (stationary_probs) {
    output <- cpp_fun(
      horizon = horizon,
      v_star = v_star,
      K = K,
      prob = prob,
      update_every = update_every,
      successes_ = successes,
      failures_ = failures
    )
  } else {
    ccp_fun <- thompson_discounted
    output <- ccp_fun(
      horizon = horizon,
      K = K,
      prob = prob,
      discount_factor = discount_factor,
      update_every = update_every,
      method = method,
      successes_ = successes,
      failures_ = failures
    )
  }


  # Output: ----
  output[["mab"]] <- mab
  class(output) <- "policy"
  return(output)
}
