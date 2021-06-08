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

policy_improvement.mdp <- function(mdp, policy, V) {

  n_states <- length(mdp$state_space)

  # Initialize:
  policy_stable <- rep(TRUE, length(policy))

  # Recursion:
  grid <- sapply(
    mdp$action_space,
    function(a) {
      # Reward vector:
      r_pi <- reward_pi(mdp, a)
      # Transition matrix:
      P_pi <- transition_pi(mdp, a)
      P_pi %*% (r_pi + mdp$discount_factor * V)
    }
  )

  proposed_policy <- sapply(
    1:nrow(grid),
    function(i) {
      best_action_value <- max(grid[i,])
      best_action <- which(grid[i,] == best_action_value)
      # Break ties:
      if(length(best_action)>1) {
        best_action <- sample(best_action, 1)
      }
      return(best_action)
    }
  )

  return(proposed_policy)

}

policy_improvement <- function(mdp, policy, V) {
  UseMethod("policy_improvement", mdp)
}

# Policy iteration:
policy_iteration.mdp <- function(mdp, policy, max_iter=100) {

  policy_stable <- rep(FALSE, length(mdp$state_space))
  iter <- 1

  while (!all(policy_stable) & iter <= max_iter) {

    # Policy evaluation:
    V <- evaluate_policy(mdp, policy)

    # Policy improvement:
    policy_proposed <- policy_improvement(mdp, policy, V)

    # Check if stable:
    policy_stable <- policy == policy_proposed
    policy <- policy_proposed
    iter <- iter + 1

    print(data.table(pol=policy, stable=policy_stable))

  }

  optimal_policy <- list(
    policy = policy,
    value = evaluate_policy(mpd, policy),
    mdp = mdp
  )

  return(optimal_policy)

}

policy_iteration <- function(mdp, policy, max_iter=100) {
  UseMethod("policy_iteration", mdp)
}



