#' UCB
#'
#' @param mab An object of class `mab`.
#' @param update_every Integer specifying length of interval after which parameters will be updated.
#' @param action_values Initial action value estimates (optional).
#'
#' @author Patrick Altmeyer
ucb_R <- function(
  mab,
  update_every = 1,
  action_values = NULL
) {

  # Parameters: ----
  unpack(mab) # unpack bandit problem
  # Uncertainty implemented according to Chapelle et al. (2011):
  uncertainty <- function(action_value,m,delta) {
    ifelse(m==0, Inf, sqrt((2 * action_value * log(1/delta)) / m) + (2*log(1/delta)) / m)
  }
  # If unambiguous, select arm with maximum UCB, else draw uniformly:
  select_arm <- function(ucb) {
    ifelse(sum(ucb==max(ucb))==1, which.max(ucb), sample(which(ucb==max(ucb)),1))
  }

  # Initialization: ----
  T_ <- 1 # iteration counter
  delta <- sqrt(1/T_)
  policy <- rep(0, horizon) # vector to keep track of choices
  regret <- rep(v_star, horizon) # initialize regret as v_star (maximal true action value)
  times_chosen <- rep(0,K) # count of times that actions were chosen
  if (is.null(action_values)) {
    action_values <- rep(0,K)
  }
  ucb <- action_values + uncertainty(action_values, times_chosen, delta) # initial upper confidence bounds

  # Recursion: ----
  while (T_ <= horizon) {

    # Select arm:
    arm <- select_arm(ucb)

    # Observe reward:
    r <- generate_rewards(prob)[arm]

    # Update choices in T:
    policy[T_] <- arm # update policy
    regret[T_] <- regret[T_] - prob[arm] # compute regret in this period
    times_chosen[arm] <- times_chosen[arm] + 1 # update counter
    q <- action_values[arm] # action value at T
    m <- times_chosen[arm] # times chose at T

    # Update parameters and estimates for T+1:
    T_ <- T_ + 1
    update_this_round <- T_ %/% update_every == T_ / update_every # update only if modulus equal to ratio
    if (update_this_round) {
      delta <- sqrt(1/T_)
      action_values[arm] <- q + 1/m * (r - q) # action value increment
      ucb[arm] <- action_values[arm] + uncertainty(action_values[arm], m, delta) # UCB increment
    }
  }

  # Output: ----
  output <- list(
    policy = policy,
    regret = regret,
    times_chosen = times_chosen,
    action_values = action_values,
    algo = list(
      type = "ucb",
      upper_confidence_bounds = ucb
    ),
    mab = mab
  )
  class(output) <- "policy"
  return(output)
}
