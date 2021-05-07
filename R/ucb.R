ucb <- function(
  mab,
  upper_confidence_bounds = NULL
) {

  # Initialization: ----
  unpack(mab) # unpack variables
  if (is.null(upper_confidence_bounds)) {
    upper_confidence_bounds <- rep(0,K) # initial upper confidence bounds
  }
  policy <- rep(0, horizon) # vector to keep track of choices
  times_chosen <- rep(0,K) # count of times that actions were chosen
  T_ <- 1 # iteration counter

  # Recursion: ----
  while (T_ <= horizon) {
    delta <- sqrt(1/T_)
    # Select arm:
    if (sum(upper_confidence_bounds==max(upper_confidence_bounds))==1) {
      arm <- which.max(upper_confidence_bounds) # select maximal value
    } else {
      # should there be no maximum, draw uniformly:
      arm <- sample(which(upper_confidence_bounds==max(upper_confidence_bounds)),1)
    }
    policy[T_] <- arm # update policy
    times_chosen[arm] <- times_chosen[arm] + 1 # update counter
    # Observe reward:
    r <- rewards[T_,get(colnames(rewards)[arm])]
    # Update upper confidence bound:
    q <- upper_confidence_bounds[arm]
    m <- times_chosen[arm]
    upper_confidence_bounds[arm] <- q +
      1/m * (r - q) + # action value estimate
      sqrt((2/m * log(1/delta)) / m) + (2*log(1/delta)) # uncertainty
    T_ <- T_ + 1
  }

  # Output: ----
  output <- list(
    policy = policy,
    algo = list(
      upper_confidence_bounds = upper_confidence_bounds,
      times_chosen = times_chosen
    )
  )
  class(output) <- "policy"
  return(output)
}
