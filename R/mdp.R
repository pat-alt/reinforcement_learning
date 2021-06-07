library(data.table)
# Markov Decision Process:
define_mdp <- function(
  state_space,
  action_space,
  reward_fun,
  transition_fun,
  discount_factor,
  ...
) {

  # Compile:
  mdp <- list(
    state_space = state_space,
    action_space = action_space,
    reward_fun = reward_fun,
    transition_fun = transition_fun,
    discount_factor = discount_factor,
    ...
  )

  # Assign class:
  class(mdp) <- "mdp"

  # Return:
  return(mdp)
}

evaluate_policy.mdp <- function(mdp, policy_fun) {

  list2env(mdp, envir = environment())
  n_states <- length(state_space)


  # Policy:
  state <- state_space
  action <- policy_fun(state=state, action_space = action_space)

  # Reward vector:
  reward_fun_args <- lapply(formalArgs(reward_fun), function(i) get(i))
  names(reward_fun_args) <- formalArgs(reward_fun)
  r_pi <- do.call(reward_fun, args = reward_fun_args)

  # Transition matrix:
  new_state <- state_space
  state_action <- data.table(state = state, action = action)
  grid <- state_action[,.(CJ(new_state=mdp$state_space, state),action)]
  grid[,prob:={
    transition_fun_args <- lapply(formalArgs(transition_fun), function(i) get(i))
    names(transition_fun_args) <- formalArgs(transition_fun)
    do.call(transition_fun, args = transition_fun_args)
  }]
  P_pi <- matrix(grid$prob, nrow=n_states)

  # Value function:
  V_pi <- solve((diag(n_states) - discount_factor * P_pi), r_pi)

  return(V_pi)

}

evaluate_policy <- function(mdp, policy_fun) {
  UseMethod("evaluate_policy", mdp)
}

