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

reward_pi.mdp <- function(mdp, policy) {

  # Setup: ----
  state <- mdp$state_space
  action_space <- mdp$action_space

  # Reward vector: ----
  reward_fun_arg_names <- formalArgs(mdp$reward_fun) # all arguments
  # Default arguments:
  reward_fun_args_default <- list(
    state = state,
    action = policy
  )
  # Additional arguments:
  add_arg_names <- reward_fun_arg_names[
    !reward_fun_arg_names  %in% names(reward_fun_args_default)
  ]
  tryCatch(
    reward_fun_args_additional <- lapply(add_arg_names, function(i) mdp[[i]]),
    error = function(e) {
      stop(
        "Any additional user-defined arguments used by reward function need to be
        passed at instatiation of MDP."
      )
    }
  )
  names(reward_fun_args_additional) <- add_arg_names
  # Bring together:
  reward_fun_args <- c(reward_fun_args_default, reward_fun_args_additional)
  r_pi <- do.call(mdp$reward_fun, args = reward_fun_args)

  # Return: ----
  return(r_pi)

}

reward_pi <- function(mdp, policy) {
  UseMethod("reward_pi", mdp)
}

transition_pi.mdp <- function(mdp, policy) {

  # Setup: ----
  list2env(mdp, envir = environment())
  n_states <- length(state_space)
  state <- mdp$state_space

  # Transition matrix: ----
  state_action <- data.table(state = state, action = policy)
  grid <- state_action[,.(CJ(new_state=state, state),action)]
  grid[,prob:={
    transition_fun_args <- lapply(formalArgs(transition_fun), function(i) get(i))
    names(transition_fun_args) <- formalArgs(transition_fun)
    do.call(transition_fun, args = transition_fun_args)
  }]
  P_pi <- matrix(grid$prob, nrow=n_states)

  # Return: ----
  return(P_pi)

}

transition_pi <- function(mdp, policy) {
  UseMethod("transition_pi", mdp)
}

evaluate_policy.mdp <- function(mdp, policy_fun) {

  list2env(mdp, envir = environment())
  n_states <- length(state_space)

  # Policy:
  policy <- policy_fun(state=state_space, action_space = action_space)

  # Reward vector:
  r_pi <- reward_pi(mdp, policy)

  # Transition matrix:
  P_pi <- transition_pi(mdp, policy)

  # Value function:
  V_pi <- solve((diag(n_states) - discount_factor * P_pi), r_pi)

  return(V_pi)

}

evaluate_policy <- function(mdp, policy_fun) {
  UseMethod("evaluate_policy", mdp)
}

