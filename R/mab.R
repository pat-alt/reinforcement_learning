#' Multi-Armed Bandit
#'
#' @param prob Bernoulli probabilities.
#' @param horizon Number of trials.
#' @param method Method to use. Only Bernoulli is implemented.
#'
#' @author Patrick Altmeyer
mab <- function(prob, horizon=1000, method="bernoulli") {

  # Parameters: ----
  stationary_probs <- !(is.matrix(prob) & length(dim(prob)) > 1)
  if (stationary_probs) {
    K <- length(prob)
    v_star <- max(prob)
  } else {
    K <- ncol(prob)
    horizon <- nrow(prob)
    v_star <- NULL
  }

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
    method = method,
    stationary_probs = stationary_probs
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
