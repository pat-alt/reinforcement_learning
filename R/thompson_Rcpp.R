library(Rcpp)
sourceCpp("R/thompson.cpp")
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
