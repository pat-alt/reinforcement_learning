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

#' Temporal Difference Learning:
#+ temp-diff
td.mdp <- function(
  mdp,
  trajectory,
  theta_init=NULL,
  alpha=function(a,b,t) a/(b+t),
  a=1e5,
  b=1e5
) {

  # Setup:
  finished <- FALSE
  iter <- 1
  if (is.null(theta_init)) {
    n_features <- length(extract_features(mdp, mdp$state_space[1]))
    theta <- rep(0, n_features)
  }
  T_ <- nrow(trajectory)

  for (t in 1:(T_-1)) {

    phi <- extract_features(mdp, trajectory[t,state])
    new_phi <- extract_features(mdp, trajectory[t+1,state])
    r <- trajectory[t,reward]

    delta <- r +
      mdp$discount_factor * crossprod(theta, new_phi) -
      crossprod(theta, phi)
    theta <- theta + alpha(a,b,t) * as.numeric(delta) * phi

  }

  V <- sapply(
    mdp$state_space,
    function(state) {
      phi <- extract_features(mdp, x=state)
      crossprod(theta, phi)
    }
  )

  output <- list(
    V = V,
    theta = theta
  )

  return(output)
}

td <- function(
  mdp,
  trajectory,
  theta_init=NULL,
  alpha=function(a,b,t) a/(b+t),
  a=1e5,
  b=1e5
) {
  UseMethod("td", mdp)
}

#' Least-squares temporal difference learning:
#+ lstd
lstd.mdp <- function(
  mdp,
  trajectory,
  sigma=1e-20
) {

  # Matrix A:
  n_features <- length(extract_features(mdp, 1))
  A <- matrix(rep(0,(n_features)^2),n_features)
  T_ <- nrow(trajectory)
  for (t in 1:(T_-1)) {
    phi <- extract_features(mdp, trajectory[t,state])
    new_phi <- extract_features(mdp, trajectory[t+1,state])
    A <- A + 1/(T_-1) * (phi %*% t(phi - mdp$discount_factor * new_phi))
  }

  # Vector b:
  b <- matrix(rep(0,n_features),n_features)
  for (t in 1:(T_-1)) {
    phi <- extract_features(mdp, trajectory[t,state])
    r_t <- trajectory[t,reward]
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

  output <- list(
    V = V,
    theta = theta
  )

  return(output)

}

lstd <- function(
  mdp,
  trajectory,
  sigma=1e-20
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
  verbose=0,
  sigma=1e-20
) {

  # Setup:
  if (is.null(policy)) {
    policy <- sample(mdp$action_space, length(mdp$state_space), replace = TRUE)
  }
  iter <- 1
  finished <- FALSE

  while (!finished) {

    if (verbose==1) {
      message(sprintf("Iteration: %i", iter))
      print(policy)
    }

    # 1.) Policy evaluation:
    trajectory <- sim_trajectory(mdp, policy, n_iter = n_trans)
    out <- lstd(mdp, trajectory, sigma=sigma)
    V <- out$V
    theta <- out$theta

    if (verbose == 1 & iter==1) {
      plot(
        x=mdp$state_space,
        y=V,
        t="l",
        xlab="State",
        ylab="Improvement",
        ylim=c(-10,0),
        main = sprintf("Iterations: %i", n_iter)
      )
    }

    # 2.) Policy improvement:
    policy <- policy_improvement(mdp, V)

    # 3.) Update:
    iter <- iter + 1
    finished <- iter == n_iter

    if (verbose==1 & iter>1) {
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
    n_iter = n_iter,
    theta = theta
  )

  return(optimal_policy)

}

appr_policy_iteration <- function(
  mdp,
  policy=NULL,
  n_iter=100,
  n_trans=1e4,
  verbose=0,
  sigma=1e-20
) {
  UseMethod("appr_policy_iteration", mdp)
}
