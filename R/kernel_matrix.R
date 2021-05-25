kernel_matrix <- function(X,Y=NULL,kernel_fun, ...) {

  # Setup:
  if (is.null(dim(X))) {
    X <- matrix(X)
  }
  n_x <- nrow(X)
  if (is.null(Y)) {
    Y <- X
  }
  Y <- as.matrix(Y)
  n_y <- nrow(Y)

  vapply(
    1:n_x,
    function(i) {
      sapply(
        1:n_y,
        function(j) {
          kernel_fun(X[i,],Y[j,],...)
        }
      )
    },
    FUN.VALUE = rep(0,n_y)
  )
}
