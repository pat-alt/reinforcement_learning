library(data.table)
# Markov Decision Process: ----
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

# Reward function: ----
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

# Transition function: ----
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

# Policy evaluation: ----
evaluate_policy.mdp <- function(mdp, policy) {

  # Reward vector:
  r_pi <- reward_pi(mdp, policy)

  # Transition matrix:
  P_pi <- transition_pi(mdp, policy)

  # Value function:
  n_states <- length(mdp$state_space)
  V_pi <- solve((diag(n_states) - mdp$discount_factor * P_pi), r_pi)

  return(V_pi)

}

evaluate_policy <- function(mdp, policy) {
  UseMethod("evaluate_policy", mdp)
}

policy_improvement.mpd <- function(mdp, policy, V) {

  n_states <- length(mdp$state_space)

  # Initialize:
  policy_stable <- rep(TRUE, length(policy))
  # Reward vector:
  r_pi <- reward_pi(mdp, policy)
  # Transition matrix:
  P_pi <- transition_pi(mdp, policy)

  # # Recursion:
  # grid <- sapply(
  #   1:n_states,
  #   function(i) {
  #     pi_s <- policy[i]
  #     sapply(
  #       mdp$action_space,
  #       function(a) {
  #         # Reward vector:
  #         r_pi <- reward_pi(mdp, a)
  #         # Transition matrix:
  #         P_pi <- transition_pi(mdp, a)
  #         r_pi_s <- r_pi[i]
  #         P_pi_s <- P_pi[i,]
  #         crossprod(P_pi_s,(r_pi_s + mdp$discount_factor * V))
  #       }
  #     )
  #   }
  # )

  grid <- sapply(
    mdp$action_space,
    function(a) {
      evaluate_policy(mdp, a)
    }
  )
}

# Policy iteration:
policy_iteration.mdp <- function(mdp, policy_init) {

  # Initialization:
  V_k <- evaluate_policy(mdp, policy_init)

}



