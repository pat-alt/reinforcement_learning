#' Multi-Armed Bandit
#'
#' @param prob Bernoulli probabilities.
#' @param horizon Number of trials.
#' @param method Method to use. Only Bernoulli is implemented.
#'
#' @author Patrick Altmeyer
mab <- function(prob, horizon=1000, method="bernoulli") {

  # Parameters: ----
  K <- length(prob)
  v_star <- max(prob)

  # Reward generating function: ----
  if (method=="bernoulli") {
    generate_rewards <- function(prop) {
      sapply(
        prob,
        function(p) {
          rbinom(1,1,p)
        }
      )
    }
  }

  # Output: ----
  output <- list(
    prob = prob,
    generate_rewards = generate_rewards,
    v_star = v_star,
    K = K,
    horizon = horizon,
    method = method
  )
  class(output) <- "mab"
  return(output)
}

unpack.mab <- function(mab, envir=parent.env) {
  list2env(mab, envir = envir(environment()))
}

unpack <- function(mab, envir=parent.env) {
  UseMethod("unpack")
}
