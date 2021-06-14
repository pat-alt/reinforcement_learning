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
  policy_fun,
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
    action <- policy_fun(state, mdp$action_space)

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
