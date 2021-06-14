#' ---
#' title: "Approximate dynamic programming"
#' author: Patrick Altmeyer
#' output:
#'  html_document:
#'    default
#' ---
#+ setup
library(data.table)
#' Feature map:
#+ extract-features
extract_features.mdp <- function(mdp, x) {

  # Feature vector: ----
  feature_map_fun_arg_names <- formalArgs(mdp$feature_map_fun) # all arguments
  # Default arguments:
  feature_map_fun_args_default <- list(
    x = x,
    state_space = mdp$state_space
  )
  # Additional arguments:
  add_arg_names <- feature_map_fun_arg_names[
    !feature_map_fun_arg_names  %in% names(feature_map_fun_args_default)
  ]
  tryCatch(
    feature_map_fun_args_additional <- lapply(add_arg_names, function(i) mdp[[i]]),
    error = function(e) {
      stop(
        "Any additional user-defined arguments used by feature_map function need to be
        passed at instatiation of MDP."
      )
    }
  )
  names(feature_map_fun_args_additional) <- add_arg_names
  # Bring together:
  feature_map_fun_args <- c(feature_map_fun_args_default, feature_map_fun_args_additional)
  phi <- do.call(mdp$feature_map_fun, args = feature_map_fun_args)

  # Return: ----
  return(phi)

}

extract_features <- function(mdp, x) {
  UseMethod("extract_features", mdp)
}

#' Temporal Difference Learning:
#+ temp-diff
td.mdp <- function(
  mdp,
  policy,
  state_init=tail(mdp$state_space,1),
  theta_init=NULL,
  n_iter=1e5,
  alpha=function(a,b,t) a/(b+t),
  a=1e5,
  b=1e5
) {

  # Setup:
  finished <- FALSE
  iter <- 1
  state <- state_init
  if (is.null(theta_init)) {
    n_features <- length(extract_features(mdp, mdp$state_space[1]))
    theta <- rep(0, n_features)
  }
  phi <- extract_features(mdp, x=state)

  while(!finished) {

    # Choose action:
    action <- policy[which(mdp$state_space == state)]

    # Observe reward:
    r_pi <- reward_pi(mdp, action, state)

    # New state:
    new_state <- transit_pi(mdp, action, state)

    # Update:
    new_phi <- extract_features(mdp, x=new_state)
    delta <- r_pi +
      mdp$discount_factor * crossprod(theta, new_phi) -
      crossprod(theta, phi)
    theta <- theta + alpha(a,b,iter) * as.numeric(delta) * phi
    phi <- new_phi
    state <- new_state
    iter <- iter + 1
    finished <- iter == n_iter

  }

  V <- sapply(
    mdp$state_space,
    function(state) {
      phi <- extract_features(mdp, x=state)
      crossprod(theta, phi)
    }
  )

  return(V)
}

td <- function(
  mdp,
  policy,
  state_init=tail(mdp$state_space,1),
  theta_init=NULL,
  n_iter=1e5,
  alpha=function(a,b,t) a/(b+t),
  a=1e5,
  b=1e5
) {
  UseMethod("td", mdp)
}

#' Trajectory:
#+ traj
sim_trajectory.mdp <- function(
  mdp,
  policy,
  state_init=tail(mdp$state_space,1),
  n_iter=1e5
) {

  # Setup:
  finished <- FALSE
  iter <- 1
  state_trajectory <- rep(0,n_iter)
  state_trajectory[1] <- state_init
  action_trajectory <- rep(0,n_iter)
  reward_trajectory <- rep(0,n_iter)

  for (i in 1:n_iter) {

    # Choose action:
    action_trajectory[i] <- policy[which(mdp$state_space == state_trajectory[i])]

    # Observe reward:
    reward_trajectory[i] <- reward_pi(
      mdp,
      action_trajectory[i],
      state_trajectory[i]
    )

    # New state:
    if (i < n_iter) {
      state_trajectory[i+1] <- transit_pi(
        mdp,
        action_trajectory[i],
        state_trajectory[i]
      )
    }

  }

  trajectory <- data.table(
    action = action_trajectory,
    reward = reward_trajectory,
    state = state_trajectory
  )

  return(trajectory)

}

sim_trajectory <- function(
  mdp,
  policy,
  state_init=tail(mdp$state_space,1),
  n_iter=1e5
) {
  UseMethod("sim_trajectory", mdp)
}

#' Least-squares temporal difference learning:
#+ lstd
lstd.mdp <- function(
  mdp,
  trajectory,
  sigma=1e-5
) {

  # Matrix A:
  n_features <- length(extract_features(mdp, 1))
  A <- matrix(rep(0,(n_features)^2),n_features)
  T_ <- nrow(trajectory)
  for (i in 1:(T_-1)) {
    phi <- extract_features(mdp, trajectory[i,state])
    new_phi <- extract_features(mdp, trajectory[i+1,state])
    A <- A + 1/(T_-1) * (phi %*% t(phi - mdp$discount_factor * new_phi))
  }

  # Vector b:
  b <- matrix(rep(0,n_features),n_features)
  for (i in 1:(T_-1)) {
    phi <- extract_features(mdp, trajectory[i,state])
    r_t <- trajectory[i,reward]
    b <- b + 1/(T_-1) * r_t * phi
  }

  # Solve:
  theta <- qr.solve(A + sigma * diag(nrow(A)), b)

  V <- sapply(
    mdp$state_space,
    function(state) {
      phi <- extract_features(mdp, x=state)
      crossprod(theta, phi)
    }
  )

  return(V)

}

lstd <- function(
  mdp,
  trajectory,
  sigma=1e-5
) {
  UseMethod("lstd", mdp)
}

#' Approximate policy iteration:
#+ appr-pol-it
appr_policy_iteration.mdp <- function(
  mdp,
  policy=NULL,
  n_iter=100,
  n_trans=1e4,
  verbose=0
) {

  # Setup:
  if (is.null(policy)) {
    policy <- sample(mdp$action_space, length(mdp$state_space), replace = TRUE)
  }
  iter <- 1
  finished <- FALSE

  while (!finished) {

    # 1.) Policy evaluation:
    trajectory <- sim_trajectory(mdp, policy, n_iter = n_trans)
    V <- lstd(mdp, trajectory)

    # 2.) Policy improvement:
    policy <- policy_improvement(mdp, V)

    # 3.) Update:
    iter <- iter + 1
    finished <- iter == n_iter

  }

  optimal_policy <- list(
    policy = policy,
    value = evaluate_policy(mdp, policy),
    mdp = mdp,
    n_iter = n_iter
  )

  return(optimal_policy)

}

appr_policy_iteration <- function(
  mdp,
  policy=NULL,
  n_iter=100,
  n_trans=1e4,
  verbose=0
) {
  UseMethod("aggr_policy_iteration", mdp)
}
