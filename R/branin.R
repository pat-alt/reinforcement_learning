library(data.table)
branin <- function(
  X,
  a=1,
  b=5.1/(4*pi^2),
  c=5/pi,
  r=6,
  s=10,
  t=1/(8*pi)
) {

  if(is.null(dim(X))) {
    X <- as.matrix(X)
  }
  if (dim(X)[2]!=2) {
    stop("Input needs to be 2-dimensional.")
  }

  # Compute output:
  x <- X[,1]
  y <- X[,2]
  f <- a * (y - b*x^2 + c*x - r)^2 + s * (1-t) * cos(x) + s

  return(f)
}
