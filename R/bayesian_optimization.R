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
  kernel_fun = kernel_se,
  acquisition_fun = acqui_ucb,
  exploring_rate = 0.5,
  verbose = 1,
  fn_scale = -1
) {

  # Setup: ----
  # Initial hyper parameters:
  list2env(hyper_params, envir = environment())
  y <- fn_scale * objective_fun(X) # initialize targets
  counter <- 1

  # Bayesian optimization: ----
  while (counter <= n_iter) {

    if(verbose %in% c(1,2,3)) {
      message(sprintf("Iteration %i:", counter))
    }

    # Update hyperparameters:
    hyper_params <- optim_hyper(hyper_params, X=X, y=y, kernel_fun = kernel_fun)

    if (verbose==3) {
      # Plot true function values for X_test:
      plot(
        objective_fun(X_test),
        cex=0.8, pch=18, t="l",
        xlab="Point", ylab="output, f(x)"
      )
      points(
        x=which.min(objective_fun(X_test)), y=min(objective_fun(X_test)),
        pch=18,
        cex=2
      )
    }

    # Run GP regression:
    args <- c(
      list(X=X, y=y, X_test = X_test, kernel_fun = kernel_fun),
      hyper_params
    )
    gp_reg <- do.call(gaussian_process_regression, args = args)

    # Run acquisition function:
    X_star <- acquisition_fun(
      gp_reg, exploring_rate=exploring_rate, fn_scale=fn_scale,
      verbose=verbose
    )
    X <- rbind(X, X_star) # store new data
    y_t <- fn_scale * objective_fun(X_star)
    y <- as.matrix(c(y,y_t)) # update target vector
    counter <- counter + 1 # update counter

    if(verbose==2) {
      print("New point:")
      print(X_star)
      print("Function value:")
      print(fn_scale * y_t)
    }
  }

  y <- fn_scale * y

  optimum <- list(
    X_star = X_star,
    X = X,
    y = y
  )

  return(optimum)
}
