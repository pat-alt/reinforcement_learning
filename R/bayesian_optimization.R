bayesian_optimization <- function(
  objective_fun=branin,
  X,
  X_test,
  n_iter = 100,
  hyper_params = list(
    length_scale = 1,
    signal_sd = 1,
    noise_sd = 0.1
  ),
  acquisition_fun = ucb,
  verbose = 1,
  ...
) {

  # Setup: ----
  # Initial hyper parameters:
  list2env(hyper_params, envir = environment())
  counter <- 1
  y <- objective_fun(X) # function to learn
  n_init_train <- nrow(X) # initial number of training points

  # Bayesian optimization: ----
  while (counter <= n_iter) {

    if(verbose %in% c(1,2)) {
      message(sprintf("Iteration %i:", counter))
    }

    # Run GP regression:
    gp_reg <- gaussian_process_regression(
      X=X, y=y, X_test = X_test,
      signal_sd=signal_sd,
      length_scale=length_scale,
      noise_sd = noise_sd
    )

    # Run acquisition function:
    X_star <- acquisition_fun(gp_reg, ...)

    # Update:
    X <- rbind(X, X_star)
    y <- objective_fun(X) # update function
    counter <- counter + 1

    if(verbose==2) {
      print("New point:")
      print(X_star)
      print("Function value:")
      print(y[n_init_train+counter-1])
    }
  }

  optimum <- list(
    X_star = X_star,
    X = X,
    y = y
  )

  return(optimum)
}
