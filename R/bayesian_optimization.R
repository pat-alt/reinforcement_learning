bayesian_optimization <- function(
  X,
  y,
  X_test,
  n_iter = 100,
  hyper_params = list(
    length_scale = 1,
    signal_sd = 1,
    noise_sd = 0.1
  ),
  acquisition_fun = ucb,
  verbose = 1
) {

  # Setup: ----
  # Initial hyper parameters:
  list2env(hyper_params, envir = environment())
  counter <- 1

  # Bayesian optimization: ----
  while (counter <= n_iter) {

    if(verbose %in% c(1,2)) {
      message(sprintf("Iteration %i:", counter))
    }

    y <- gaussian_process(
      X=X,
      signal_sd=signal_sd,
      length_scale=length_scale,
      noise_sd = noise_sd
    )

    # Run GP regression:
    gp_reg <- gaussian_process_regression(
      X=X, y=y, X_test = X_test,
      signal_sd=signal_sd,
      length_scale=length_scale,
      noise_sd = noise_sd
    )

    # Run acquisition function:
    acq_output <- acquisition_fun(gp_reg)
    X_star <- acq_output$X_t
    y_t <- acq_output$y

    # Update:
    X <- rbind(X, X_star)
    # y <- as.matrix(c(y,y_t))
    counter <- counter + 1

    if(verbose==2) {
      print("New point:")
      print(X_star)
      print("Function value:")
      print(y_t)
    }
  }

  optimum <- list(
    X_star = X_star,
    X = X,
    y = y
  )

  return(optimum)
}
