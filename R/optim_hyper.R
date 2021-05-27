# Hyper parameters: ----
optim_hyper <- function(
  theta,
  X,
  y,
  kernel_fun,
  control = list(
    maxit=1000,
    fnscale=-1
  ),
  tol=1e-30,
  min_par=1e-5,
  eps=1e-30
) {

  # Log likelihood:
  lml <- function(theta, X, y, kernel_fun) {

    # Unpack parameters:
    theta <- as.list(theta)
    list2env(theta, envir = environment())
    n <- nrow(as.matrix(X))

    # Covariance matrix:
    K <- kernel_matrix(X, kernel_fun = kernel_fun, signal_sd=signal_sd, length_scale=length_scale)
    K_y <- K + noise_sd^2 * diag(n)
    L <- t(chol(K_y + eps)) # Cholesky decompose
    alpha <- as.matrix(solve(crossprod(t(L)),y,tol=tol))

    # Compute log marginal likelihood:
    lml <- (- (crossprod(y,alpha))/2 - sum(log(diag(L))) - (n/2) * log(2*pi))
    return(lml)

  }

  # Multi-started optimization: ----
  multi_starts <- list(
    theta,
    lapply(theta, function(i) 1), # try all ones
    lapply(theta, function(i) 10), # try all 10s
    lapply(theta, function(i) runif(1, min = min_par, max=1)), # try all uniform [min,1]
    lapply(theta, function(i) runif(1, min = min_par, max=10)), # try all uniform [min,10]
    lapply(theta, function(i) runif(1, min = min_par, max=100)) # try all uniform [min,100]
  )
  # Run:
  multi_optim <- lapply(
    multi_starts,
    function(theta) {
      # Optimal values
      optim_output <- optim(
        par=theta,
        X=X, y=y, kernel_fun=kernel_fun,
        fn=lml,
        control=control,
        method="L-BFGS-B",
        lower=rep(1e-5,length(theta)),
        upper=rep(Inf,length(theta))
      )
      optim_value <- optim_output$value
      optim_theta <- optim_output$par
      return(
        list(
          value = optim_value,
          theta = optim_theta
        )
      )
    }
  )

  # Output:
  idx_max <- which.max(sapply(multi_optim, function(i) i$value))
  theta <- multi_optim[[idx_max]]$theta

  return(theta)
}
