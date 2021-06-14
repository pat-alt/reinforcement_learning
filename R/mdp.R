#' ---
#' title: "Code used for Markov Decision Process problems"
#' author: Patrick Altmeyer
#' output:
#'  html_document:
#'    default
#' ---
#+ setup
library(data.table)
#' Markov decision process:
#+ mdp
define_mdp <- function(
  reward_fun,
  discount_factor,
  transition_fun=NULL,
  transition_prob_fun=NULL,
  state_space=NULL,
  action_space=NULL,
  ...
) {

  # Compile:
  mdp <- list(
    reward_fun = reward_fun,
    discount_factor = discount_factor,
    transition_fun = transition_fun,
    transition_prob_fun = transition_prob_fun,
    state_space = state_space,
    action_space = action_space,
    ...
  )

  # Assign class:
  class(mdp) <- "mdp"

  # Return:
  return(mdp)
}

#' Reward function:
#+ reward-fun
reward_pi.mdp <- function(mdp, policy=NULL, state=NULL) {

  # Setup:
  if (is.null(state)) {
    state <- mdp$state_space
    action <- policy
  }

  # Reward vector:
  reward_fun_arg_names <- formalArgs(mdp$reward_fun) # all arguments
  # Default arguments:
  reward_fun_args_default <- list(
    state = state,
    action = action
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

  # Return:
  return(r_pi)

}

reward_pi <- function(mdp, policy=NULL, state=NULL) {
  UseMethod("reward_pi", mdp)
}

#' Transition function:
#+ trans-fun
transit_pi.mdp <- function(mdp, policy=NULL, state=NULL) {

  # Setup:
  if (is.null(state)) {
    state <- mdp$state_space
    action <- policy
  }

  # transition vector:
  transition_fun_arg_names <- formalArgs(mdp$transition_fun) # all arguments
  # Default arguments:
  transition_fun_args_default <- list(
    state = state,
    action = action
  )
  # Additional arguments:
  add_arg_names <- transition_fun_arg_names[
    !transition_fun_arg_names  %in% names(transition_fun_args_default)
  ]
  tryCatch(
    transition_fun_args_additional <- lapply(add_arg_names, function(i) mdp[[i]]),
    error = function(e) {
      stop(
        "Any additional user-defined arguments used by transition function need to be
        passed at instatiation of MDP."
      )
    }
  )
  names(transition_fun_args_additional) <- add_arg_names
  # Bring together:
  transition_fun_args <- c(
    transition_fun_args_default,
    transition_fun_args_additional
  )
  new_state <- do.call(mdp$transition_fun, args = transition_fun_args)

  # Return:
  return(new_state)

}

transit_pi <- function(mdp, policy=NULL, state=NULL) {
  UseMethod("transit_pi", mdp)
}

#' Transition probability function:
#+ trans-prob-fun
transition_prob_pi.mdp <- function(mdp, policy) {

  # Setup:
  list2env(mdp, envir = environment())
  n_states <- length(state_space)
  state <- mdp$state_space

  # Transition matrix:
  state_action <- data.table(state = state, action = policy)
  grid <- state_action[,.(CJ(new_state=state, state),action)]
  grid[,prob:={
    transition_prob_fun_args <- lapply(formalArgs(transition_prob_fun), function(i) get(i))
    names(transition_prob_fun_args) <- formalArgs(transition_prob_fun)
    do.call(transition_prob_fun, args = transition_prob_fun_args)
  }]
  P_pi <- matrix(grid$prob, nrow=n_states)

  # Return:
  return(P_pi)

}

transition_prob_pi <- function(mdp, policy) {
  UseMethod("transition_prob_pi", mdp)
}

#' Evaluate policy:
#+ eval-pol
evaluate_policy.mdp <- function(mdp, policy) {

  # Reward vector:
  r_pi <- reward_pi(mdp, policy)

  # Transition matrix:
  P_pi <- transition_prob_pi(mdp, policy)

  # Value function:
  n_states <- length(mdp$state_space)
  V_pi <- solve((diag(n_states) - mdp$discount_factor * P_pi), r_pi)

  return(V_pi)

}

evaluate_policy <- function(mdp, policy) {
  UseMethod("evaluate_policy", mdp)
}

#' Power iteration:
#+ power-it
power_iteration.mdp <- function(mdp, policy, V, accuracy=1e-0, max_iter=1000) {

  # Setup:
  delta <- Inf
  finished <- FALSE
  iter <- 1

  while(!finished) {

    # Reward vector:
    r_pi <- reward_pi(mdp, policy)

    # Transition matrix:
    P_pi <- transition_prob_pi(mdp, policy)

    # Value function:
    V_new <- r_pi + P_pi %*% (mdp$discount_factor * V)

    # Observe and update:
    delta <- min(delta, max(abs(V_new-V)))
    V <- V_new
    iter <- iter + 1
    finished <- delta < accuracy | iter == max_iter

  }

  return(V)

}

power_iteration <- function(mdp, policy, V, accuracy=1e-0, max_iter=200) {
  UseMethod("power_iteration", mdp)
}

#' Policy improvement:
#+ pol-imp
policy_improvement.mdp <- function(
  mdp,
  V,
  output_type="policy"
) {

  n_states <- length(mdp$state_space)

  # Recursion:
  grid <- sapply(
    mdp$action_space,
    function(a) {
      # Reward vector:
      r_pi <- as.matrix(reward_pi(mdp, a))
      # Transition matrix:
      P_pi <- transition_prob_pi(mdp, a)
      r_pi + P_pi %*% (mdp$discount_factor * V)
    }
  )

  V_star <- apply(grid, 1, max)

  proposed_policy <- sapply(
    1:nrow(grid),
    function(i) {
      best_action_value <- max(grid[i,])
      best_action <- which(grid[i,] == best_action_value)
      # Break ties:
      if(length(best_action)>1) {
        best_action <- sample(best_action, 1)
      }
      return(action_space[best_action])
    }
  )

  output <- switch(
    output_type,
    policy = proposed_policy,
    value = V_star
  )

  return(output)

}

policy_improvement <- function(
  mdp,
  V,
  output_type="policy"
) {
  UseMethod("policy_improvement", mdp)
}

#' Policy iteration:
#+ pol-it
policy_iteration.mdp <- function(
  mdp,
  policy=NULL,
  accuracy=1e-5,
  max_iter=100,
  verbose=0,
  V=NULL,
  use_conv_crit=TRUE
) {

  # Setup:
  if (is.null(policy)) {
    policy <- sample(mdp$action_space, length(mdp$state_space), replace = TRUE)
  }
  policy_stable <- rep(FALSE, length(mdp$state_space))
  iter <- 1
  if (is.null(V)) {
    V <- rep(-10, length(mdp$state_space)) # initialize V
  }
  finished <- FALSE
  policy_path <- data.table()

  if (verbose == 1) {
    plot(
      x=mdp$state_space,
      y=V,
      t="l",
      xlab="State",
      ylab="Improvement",
      ylim=c(min(V),0),
      main = sprintf("Iterations: %i; Accuracy: %0.2e", max_iter, accuracy)
    )
  }

  while (!finished) {

    # 1.) Policy evaluation:
    if (use_conv_crit) {
      V <- power_iteration(mdp, policy, V, accuracy = accuracy)
    } else {
      V <- power_iteration(mdp, policy, V, max_iter = 1)
    }

    # 2.) Policy improvement:
    policy_proposed <- policy_improvement(mdp, V)

    # 3.) Check if stable:
    policy_stable <- policy == policy_proposed
    policy_path <- rbind(
      policy_path,
      data.table(policy = policy, proposed = policy_proposed, iter=iter)
    )
    policy <- policy_proposed
    iter <- iter + 1
    if (use_conv_crit) {
      finished <- all(policy_stable) | iter == max_iter
    } else {
      finished <- iter == max_iter
    }

    if (verbose==1) {
      if (finished) {
        points(
          x=mdp$state_space,
          y=V,
          t="l",
          col="blue",
          lwd=2
        )
      } else {
        points(
          x=mdp$state_space,
          y=V,
          t="l",
          col=alpha("black",0.5),
          lty="dotted"
        )
      }
    }

  }

  optimal_policy <- list(
    policy = policy,
    value = evaluate_policy(mdp, policy),
    mdp = mdp,
    policy_path = policy_path,
    max_iter = max_iter,
    converged_after = iter
  )

  return(optimal_policy)

}

policy_iteration <- function(
  mdp,
  policy=NULL,
  accuracy=1e-5,
  max_iter=100,
  verbose=0,
  V=NULL,
  use_conv_crit=TRUE
) {
  UseMethod("policy_iteration", mdp)
}

#' Value iteration:
#+ val-it
value_iteration.mdp <- function(
  mdp,
  max_iter=100,
  accuracy=1e-5,
  verbose=0,
  V = NULL,
  use_conv_crit=TRUE
) {

  # Setup:
  if (is.null(V)) {
    V <- rep(-10, length(mdp$state_space)) # initialize V
  }
  # Following Sutton (2018), set terminal state to 0. Not found to be useful,
  # so commented out.
  # V[length(mdp$state_space)] <- 0
  finished <- FALSE
  delta <- Inf
  iter <- 1

  if (verbose == 1) {
    plot(
      x=mdp$state_space,
      y=V,
      t="l",
      xlab="State",
      ylab="Improvement",
      ylim=c(min(V),0),
      main = sprintf("Iterations: %i; Accuracy: %0.2e", max_iter, accuracy)
    )
  }

  while(!finished) {

    # 1.) Value function:
    V_new <- policy_improvement(mdp, V, output_type = "value")

    # 2.) Observe and update:
    delta <- min(delta, max(abs(V_new-V)))
    V <- V_new
    iter <- iter + 1
    if (use_conv_crit) {
      finished <- (delta < accuracy) | (iter == max_iter)
    } else {
      finished <- iter == max_iter
    }

    if (verbose==1) {
      if (finished) {
        points(
          x=mdp$state_space,
          y=V,
          t="l",
          col="blue",
          lwd=2
        )
      } else {
        points(
          x=mdp$state_space,
          y=V,
          t="l",
          col=alpha("black",0.5),
          lty="dotted"
        )
      }
    }

  }

  # Compute implied deterministic policy:
  implied_policy <- policy_improvement(mdp, V)
  # Evaluate corresponding value:
  implied_value <- evaluate_policy(mdp, implied_policy)

  optimal_policy <- list(
    policy = implied_policy,
    value = implied_value,
    mdp = mdp,
    max_iter = max_iter,
    converged_after = iter
  )

}

value_iteration <- function(
  mdp,
  max_iter=100,
  accuracy=1e-5,
  verbose=0,
  V = NULL,
  use_conv_crit=TRUE
) {
  UseMethod("value_iteration", mdp)
}



