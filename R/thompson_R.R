#' Thompson Sampling
#'
#' @param mab An object of class `mab`.
#' @param successes Initial success counts (optional).
#' @param failures Initial failure counts (optional).
#'
#' @author Patrick Altmeyer
thompson_R <- function(
  mab,
  successes = NULL,
  failures = NULL
) {

  # Parameters: ----
  unpack(mab) # unpack bandit problem

  # If unambiguous, select arm with maximum value, else draw uniformly:
  select_arm <- function(successes, failures, method="bernoulli") {
    K <- length(successes)
    if (method=="bernoulli") {
      theta <- rbeta(K,successes,failures)
      arm <- which.max(theta)
    }
    return(arm)
  }

  posteriour_means <- function(successes, failures, method="bernoulli") {
    K <- length(successes)
    if (method=="bernoulli") {
      theta <- rbeta(K,successes,failures)
    }
    return(theta)
  }

  # Initialization: ----
  T_ <- 1 # iteration counter
  policy <- rep(0, horizon) # vector to keep track of choices
  regret <- rep(v_star, horizon) # initialize regret as v_star (maximal true action value)
  times_chosen <- rep(0,K) # count of times that actions were chosen
  if (is.null(successes)) {
    successes <- rep(1,K) # initialize success count
  }
  if (is.null(failures)) {
    failures <- rep(1,K) # initialize failure count
  }

  # Recursion: ----
  while (T_ <= horizon) {

    # Select arm:
    arm <- select_arm(successes = successes, failures = failures)

    # Observe reward:
    r <- generate_rewards(prob)[arm]

    # Update choices in T:
    policy[T_] <- arm # update policy
    regret[T_] <- regret[T_] - prob[arm] # compute regret in this period
    times_chosen[arm] <- times_chosen[arm] + 1 # update counter

    # Update parameters and estimates for T+1:
    T_ <- T_ + 1
    if (r==1) {
      successes[arm] <- successes[arm] + 1
    } else {
      failures[arm] <- failures[arm] + 1
    }

  }

  # Output: ----
  output <- list(
    policy = policy,
    regret = regret,
    times_chosen = times_chosen,
    action_values = posteriour_means(successes, failures),
    algo = list(
      type = "thompson",
      successes = successes,
      failures = failures
    ),
    mab = mab
  )
  class(output) <- "policy"
  return(output)
}
