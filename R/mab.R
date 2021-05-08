library(data.table)
simulate_mab <- function(prob, horizon=1000) {
  # Generate rewards: ----
  K <- length(prob)
  v_star <- max(prob)
  rewards <- data.table(
    sapply(
      prob,
      function(p) {
        rbinom(horizon,1,p)
      }
    )
  )

  # Output: ----
  output <- list(
    prob = prob,
    v_star = v_star,
    K = K,
    horizon = horizon,
    rewards = rewards
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
