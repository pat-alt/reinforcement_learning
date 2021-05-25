gaussian_process <- function(
  X,
  mu = rep(0,nrow(X)),
  kernel_fun=kernel_se,
  noise_sd=0.1,
  ...
) {

  if (is.null(dim(X))) {
    X <- matrix(X)
  }
  n <- nrow(X)

  # Derive gram matrix:
  K <- kernel_matrix(X, kernel_fun = kernel_fun, ...)
  L <- t(chol(K + noise_sd^2 * diag(n))) # Cholesky decompose

  # Compute Gaussian Process:
  gp <- mu + L %*% matrix(rnorm(n),nrow=n)

  # Output:
  return(gp)

}
