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
  fn_scale = -1,
  store_path = FALSE
) {

  # Setup: ----
  # Initial hyper parameters:
  list2env(hyper_params, envir = environment())
  y <- fn_scale * objective_fun(X) # initialize targets
  counter <- 1
  posterior <- NULL # to store posterior values
  optima <- NULL # to store proposed optima
  true_optimum <- fn_scale * max(fn_scale * objective_fun(X_test))
  true_fun <- objective_fun(X_test) # true function values
  loss <- as.matrix(rep(0, n_iter))

  # Bayesian optimization: ----
  while (counter <= n_iter) {

    if(verbose %in% c(1,2,3)) {
      message(sprintf("Iteration %i:", counter))
    }

    # Update hyperparameters:
    hyper_params <- tryCatch(
      optim_hyper(hyper_params, X=X, y=y, kernel_fun = kernel_fun),
      error = function(e) {
        return(hyper_params)
      }
    )

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
    loss_t <- (true_optimum - y_t)^2
    loss[counter] <- loss_t # update loss

    # Store posterior:
    if (store_path) {

      # Posterior
      posterior_t <- data.table(gp_reg$predictions)
      setnames(posterior_t, names(posterior_t), c("value", "uncertainty"))
      posterior_t[,t:=counter]
      posterior_t[,point:=1:.N]
      posterior <- rbind(
        posterior,
        posterior_t
      )

      # Minimum
      point <- which(true_fun==fn_scale * y_t)
      optima_t <- data.table(point=point, value=fn_scale * y_t)
      optima_t[,t:=counter]
      optima <- rbind(optima, optima_t)

    }

    # Print results:
    if(verbose==2) {
      print("New point:")
      print(X_star)
      print("Function value:")
      print(fn_scale * y_t)
    }

    counter <- counter + 1 # update counter

  }

  y <- fn_scale * y
  path <- list(
    posterior = posterior,
    optima = optima
  )

  bayes_optimum <- list(
    X_star = X_star,
    X = X,
    y = y,
    X_test = X_test,
    true_optimum = true_optimum,
    loss = loss,
    objective_fun = objective_fun,
    path = path,
    fn_scale = fn_scale
  )
  class(bayes_optimum) <- "bayes_optimum"

  return(bayes_optimum)
}

library(ggplot2)
plot.bayes_optimum <- function(bayes_optimum) {
  loss <- data.table(loss=c(bayes_optimum$loss))
  loss[,t:=.I]
  p <- ggplot(data=loss, aes(x=t, y=loss)) +
    geom_line() +
    labs(
      x="T",
      y="Loss"
    )
  p
  return(p)
}

library(gganimate)
plot_path.bayes_optimum <- function(bayes_optimum) {

  if (is.null(bayes_optimum$path$posterior)) {
    stop("Path of posterior was not stored.")
  }

  list2env(bayes_optimum$path, envir = environment())

  # Function path:
  path <- copy(posterior)
  path[,value:=value * bayes_optimum$fn_scale]
  setnames(path, "value", "estimate")
  true_y <- bayes_optimum$objective_fun(bayes_optimum$X_test)
  ylim <- range(true_y)
  ylim[1] <- ylim[1] * 0.9
  ylim[2] <- ylim[2] * 1.1
  path[,true:=rep.int(true_y,max(t))]
  path[,point:=1:.N,by=.(t)]
  path[,lb:=estimate-2*uncertainty]
  path[,ub:=estimate+2*uncertainty]
  path <- melt(path, id.vars = c("t", "point", "uncertainty"))
  path[,uncertainty:=NULL]
  point_estimate <- copy(path[variable %in% c("estimate", "true")])

  # Confidence bands:
  ci <- copy(path[variable %in% c("lb", "ub")])
  ci[,id:=1]
  ci[variable=="lb",point:=rev(point),by=.(t)]
  ci[variable=="lb",value:=rev(value),by=.(t)]

  # Minimum path:
  y_star <- bayes_optimum$true_optimum
  X_star <- which(bayes_optimum$objective_fun(bayes_optimum$X_test)==y_star)
  true_optima <- data.table(
    point=X_star,
    value=y_star,
    t=1:optima[,.N],
    variable="true"
  )
  optima[,variable:="estimate"]
  optima_path <- rbind(
    true_optima,
    optima
  )

  p <- ggplot() +
    geom_polygon(
      data=ci,
      aes(x=point, y=value, group=id),
      fill="blue",
      alpha=0.1
    ) +
    geom_line(
      data=point_estimate,
      aes(x=point, y=value, colour=variable)
    ) +
    geom_point(
      data=optima_path,
      aes(x=point, y=value, colour=variable),
      cex=2,
      pch=18
    ) +
    scale_colour_manual(
      name="Value:",
      values = c("blue", "black")
    ) +
    transition_states(
      t,
      transition_length = 2,
      state_length = 1
    )+
    labs(
      title="Iteration: {closest_state}"
    ) +
    coord_cartesian(ylim=ylim)

  animate(p, width = 400, height = 300)

}

plot_path <- function(bayes_optimum) {
  UseMethod("plot_path", bayes_optimum)
}
