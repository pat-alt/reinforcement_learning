library(Rcpp)
sourceCpp("R/ucb.cpp")
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
