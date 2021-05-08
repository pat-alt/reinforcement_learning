ucb <- function(
  mab,
  action_values = NULL
) {

  # Parameters: ----
  uncertainty <- function(action_value,m,delta) {
    ifelse(m==0, Inf, sqrt((2 * action_value * log(1/delta)) / m) + (2*log(1/delta)) / m) # uncertainty
  }

  # Initialization: ----
  unpack(mab) # unpack bandit problem
  delta <- sqrt(1/T_)
  policy <- rep(0, horizon) # vector to keep track of choices
  times_chosen <- rep(0,K) # count of times that actions were chosen
  T_ <- 1 # iteration counter
  if (is.null(action_values)) {
    action_values <- rep(0,K)
  }
  ucb <- action_values + uncertainty(action_values, times_chosen, delta) # initial upper confidence bounds

  # Recursion: ----
  while (T_ <= horizon) {

    # Select arm:
    if (sum(ucb==max(ucb))==1) {
      arm <- which.max(ucb) # select maximal value
    } else {
      # should there be no maximum, draw uniformly:
      arm <- sample(which(ucb==max(ucb)),1)
    }
    policy[T_] <- arm # update policy
    times_chosen[arm] <- times_chosen[arm] + 1 # update counter

    # Observe reward:
    r <- rewards[T_,get(colnames(rewards)[arm])]

    # Update parameters and estimates for T+1:
    T_ <- T_ + 1
    delta <- sqrt(1/T_)
    q <- action_values[arm] # action value at T
    m <- times_chosen[arm] # times chose at T
    action_values[arm] <- q + 1/m * (r - q) # action value increment
    ucb[arm] <- action_values[arm] + uncertainty(action_values[arm], m, delta) # UCB increment
  }

  # Output: ----
  output <- list(
    policy = policy,
    algo = list(
      upper_confidence_bounds = ucb,
      times_chosen = times_chosen
    ),
    mab = mab
  )
  class(output) <- "policy"
  return(output)
}
