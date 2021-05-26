# Hyper parameters: ----
lml <- function(theta, X, y, kernel_fun) {

  # Unpack parameters:
  theta <- as.list(theta)
  list2env(theta, envir = environment())
  n <- nrow(as.matrix(y))

  # Covariance matrix:
  K <- kernel_matrix(X, kernel_fun = kernel_fun, signal_sd=signal_sd, length_scale=length_scale)
  K_y <- K + noise_sd^2 * diag(n)

  lml <- (-1) * (-(1/2) * t(y) %*% solve(K_y) %*% y - (1/2) * log(det(K_y)) - (n/2) * log(2*pi))
  return(lml)

}

optim(
  par=hyper_params,
  X=X, y=y, kernel_fun=kernel_se,
  fn=lml,
  control=list(maxit=500)
)
