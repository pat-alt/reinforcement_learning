# Markov Decision Process:
define_mdp <- function(
  state_space,
  action_space,
  reward_fun,
  transition_fun,
  discount_factor
) {

  # Compile:
  mdp <- list(
    state_space = state_space,
    action_space = action_space,
    reward_fun = reward_fun,
    transition_fun = transition_fun,
    discount_factor = discount_factor
  )

  # Assign class:
  class(mdp) <- "mdp"

  # Return:
  return(mdp)
}

evaluate_policy.mdp <- function(mdp, policy_fun) {

  list2env(mdp, envir = environment())

  # Policy:
  pi <- policy_fun(state=mdp$state_space, action_space = mdp$action_space)

  # Reward vector:
  rewards_pi <- mdp$reward_fun(
    state = mdp$state_space,
    action = pi
  )

  # Transition matrix:
  P <- sapply(
    state_space,
    function(state) {
      sapply(
        rewards_pi,
        function(reward) {
          NULL
        }
      )
    }
  )

}

