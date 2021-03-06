gaussian_process_regression <- function(
  X,
  y,
  kernel_fun=kernel_se,
  noise_sd=0.1,
  X_test,
  inv_tol=1e-30,
  ...
) {

  # Set up: ----
  # Training data:
  if (is.null(dim(X))) {
    X <- matrix(X)
  }
  n <- nrow(X)
  # Test data:
  if (is.null(dim(X_test))) {
    X_test <- matrix(X_test)
  }
  n_test <- nrow(X_test)

  # Derive gram matrix:
  K <- kernel_matrix(X, kernel_fun = kernel_fun, ...) # Training
  L <- t(chol(K + noise_sd^2 * diag(n))) # Cholesky decompose
  alpha <- as.matrix(solve(crossprod(t(L)),y,tol=inv_tol))

  # Iterate over test cases:
  predictions <- t(
    sapply(
      1:n_test,
      function(i) {
        x_star <- matrix(X_test[i,],nrow = 1)
        k_star <- kernel_matrix(X, x_star, kernel_fun = kernel_fun, ...)
        predictive_mean <- crossprod(k_star, alpha)
        v <- solve(L, k_star)
        predictive_variance <- kernel_matrix(x_star, x_star, kernel_fun = kernel_fun, ...) - crossprod(v)
        return(cbind(mean=predictive_mean, var=predictive_variance))
      }
    )
  )

  # Output:
  gp_regression <- list(
    predictions = predictions,
    X=X,
    y=y,
    X_test=X_test
  )
  class(gp_regression) <- "gp_regression"
  return(gp_regression)

}

# Methods: ----
# Plot: ----
plot.gp_regression <- function(gp_regression) {

  list2env(gp_regression, envir = environment())
  if (dim(X)[2]>1) {
    stop("Your input is multi-dimensional, which cannot be visualized.")
  }

  # Predictions:
  pred <- data.table(x=X_test[,1], y=predictions[,1])
  # Confidence bands:
  ci <- data.table(
    x=X_test[,1],
    lb=predictions[,1]-2*sqrt(predictions[,2]),
    ub=predictions[,1]+2*sqrt(predictions[,2])
  )
  ci <- melt(ci, id.vars = "x", value.name = "y")
  ci[,id:=1]
  ci[variable=="lb",x:=rev(x)]
  ci[variable=="lb",y:=rev(y)]
  # Training points:
  train <- data.table(x=c(X), y=c(y))

  p <- ggplot() +
    geom_polygon(data=ci, aes(x=x, y=y, group=id), fill="blue", alpha=0.1) +
    geom_line(data=pred, aes(x=x, y=y), colour="blue") +
    geom_point(data=train, aes(x=x, y=y), shape=3, colour="blue") +
    labs(
      x="input, x",
      y="output, f(x)"
    ) +
    scale_x_continuous(expand=c(0,0))
  p
  return(p)

}

# UCB: ----
acqui_ucb.gp_regression <- function(
  gp_regression, exploring_rate=0.5, fn_scale=1, verbose=1
) {

  list2env(gp_regression, envir = environment())

  # Apply UCB
  value_estimate <- predictions[,1] + exploring_rate * predictions[,2]
  idx_max <- which.max(value_estimate)
  if (verbose==3) {
    points(fn_scale * predictions[,1], col=alpha("blue",0.5), cex=0.8, t="l")
    abline(v=idx_max, lty=2)
    legend(
      "topright",
      legend=c("True value", "Estimated value"),
      lty=c(1,1), col=c("black", "blue")
    )
  }
  X_t <- matrix(X_test[idx_max,], ncol = ncol(gp_regression$X))

  # Return:
  return(X_t)

}

acqui_ucb <- function(
  gp_regression, exploring_rate=0.5, fn_scale=1, verbose=1
) {
  UseMethod("acqui_ucb", gp_regression)
}

# PI: ----
acqui_pi.gp_regression <- function(
  gp_regression, exploring_rate=0.5, fn_scale=1, verbose=1
) {

  list2env(gp_regression, envir = environment())

  # Apply PI:
  mu_star <- max(y)
  z_score <- (predictions[,1] - mu_star - exploring_rate)/sqrt(predictions[,2])
  value_estimate <- pnorm(z_score)
  idx_max <- which.max(value_estimate)
  if (verbose==3) {
    points(fn_scale * predictions[,1], col=alpha("blue",0.5), cex=0.8, t="l")
    abline(v=idx_max, lty=2)
    legend(
      "topright",
      legend=c("True value", "Estimated value"),
      lty=c(1,1), col=c("black", "blue")
    )
  }
  X_t <- matrix(X_test[idx_max,], ncol = ncol(gp_regression$X))

  # Return:
  return(X_t)

}

acqui_pi <- function(
  gp_regression, exploring_rate=0.5, fn_scale=1, verbose=1
) {
  UseMethod("acqui_pi", gp_regression)
}

# EI: ----
acqui_ei.gp_regression <- function(
  gp_regression, exploring_rate=0.5, fn_scale=1, verbose=1
) {

  list2env(gp_regression, envir = environment())

  # Apply EI:
  mu_star <- max(y)
  z_score <- (predictions[,1] - mu_star - exploring_rate)/sqrt(predictions[,2])
  value_estimate <- (predictions[,1] - mu_star - exploring_rate) * pnorm(z_score) +
    sqrt(predictions[,2]) * dnorm(z_score)

  idx_max <- which.max(value_estimate)
  if (verbose==3) {
    points(fn_scale * predictions[,1], col=alpha("blue",0.5), cex=0.8, t="l")
    abline(v=idx_max, lty=2)
    # points(x=idx_max, y=fn_scale * predictions[idx_max,1], col="blue", cex=1.5, pch=16)
    legend(
      "topright",
      legend=c("True value", "Estimated value"),
      lty=c(1,1), col=c("black", "blue")
    )
  }
  X_t <- matrix(X_test[idx_max,], ncol = ncol(gp_regression$X))

  # Return:
  return(X_t)

}

acqui_ei <- function(
  gp_regression, exploring_rate=0.5, fn_scale=1, verbose=1
) {
  UseMethod("acqui_ei", gp_regression)
}

