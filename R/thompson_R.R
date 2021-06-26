#' Thompson Sampling
#'
#' @param mab An object of class `mab`.
#' @param update_every Integer specifying length of interval after which parameters will be updated.
#' @param successes Initial success counts (optional).
#' @param failures Initial failure counts (optional).
#'
#' @author Patrick Altmeyer
thompson_R <- function(
  mab,
  update_every = 1,
  successes = NULL,
  failures = NULL,
  discount_factor = 1
) {

  # Parameters: ----
  unpack(mab) # unpack bandit problem

  # Function to compute posterior means (action values):
  posterior_means <- function(successes, failures, method=method) {
    K <- length(successes)
    if (method == "bernoulli") {
      theta <- rbeta(K,successes,failures)
    }
    return(theta)
  }
  # Function to select based on action values:
  select_arm <- function(theta) {
    arm <- which.max(theta)
    return(arm)
  }

  # Initialization: ----
  T_ <- 1 # iteration counter
  policy <- rep(0, horizon) # vector to keep track of choices
  regret <- rep(0, horizon) # initialize regret as zero
  times_chosen <- rep(0,K) # count of times that actions were chosen
  if (is.null(successes)) {
    successes <- rep(1,K) # initialize success count
  }
  if (is.null(failures)) {
    failures <- rep(1,K) # initialize failure count
  }
  theta <- posterior_means(successes = successes, failures = failures, method = method) # initilize action values

  # Recursion: ----
  while (T_ <= horizon) {

    # Get probabilities:
    if (stationary_probs) {
      prob_t <- prob
    } else {
      prob_t <- prob[T_,]
      v_star <- max(prob_t)
    }

    # Select arm:
    arm <- select_arm(theta = theta)

    # Observe reward:
    r <- generate_rewards(prob_t)[arm]

    # Update according to choices in T:
    policy[T_] <- arm # update policy
    regret[T_] <- v_star - prob_t[arm] # compute regret in this period
    times_chosen[arm] <- times_chosen[arm] + 1 # update counter

    # Update success/failure counts for chosen arm:
    if (r == 1) {
      successes[arm] <- discount_factor * successes[arm] + 1
    } else {
      failures[arm] <- discount_factor * failures[arm] + 1
    }
    # Update success/failure counts for remaining arms:
    successes[-arm] <- discount_factor * successes[-arm]
    failures[-arm] <- discount_factor * failures[-arm]

    # Update parameters and estimates for T+1:
    T_ <- T_ + 1
    update_this_round <- T_ %% update_every == 0 # update only if modulus equal to zero
    if (update_this_round) {
      theta <- posterior_means(successes = successes, failures = failures, method = method)
    }

  }

  # Output: ----
  output <- list(
    policy = policy,
    regret = regret,
    times_chosen = times_chosen,
    action_values = theta,
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
