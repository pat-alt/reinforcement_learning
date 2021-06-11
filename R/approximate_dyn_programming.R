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

reward_pi <- function(mdp, policy) {
  UseMethod("reward_pi", mdp)
}
