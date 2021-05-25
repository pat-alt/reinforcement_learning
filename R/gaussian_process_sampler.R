gaussian_process_sampler <- function(
  X_test=seq(-7,7,length.out=100),
  n_samples=5,
  gaussian_process_fun=gaussian_process,
  kernel_fun=kernel_se,
  ...
) {

  # Set up:
  if (is.null(dim(X_test))) {
    X_test <- matrix(X_test)
  }
  n <- nrow(X_test)

  samples <- vapply(
    1:n_samples,
    function(i) {
      gp <- gaussian_process_fun(X=X_test,kernel_fun=kernel_fun,...)
      return(gp)
    },
    FUN.VALUE = rep(0,n)
  )

  # Output:
  gp_samples <- list(
    X_test = X_test,
    y = samples,
    kernel_fun = kernel_fun,
    params = ...
  )
  class(gp_samples) <- "gp_samples"
  return(gp_samples)

}

# Methods: ----
library(ggplot2)
library(data.table)
plot.gp_samples <- function(gp_samples) {

  list2env(gp_samples, envir = environment())
  if (dim(X_test)[2]>1) {
    stop("Your input is multi-dimensional, which cannot be visualized.")
  }

  # Samples:
  dt_plot <- data.table(x=c(X_test),y)
  setnames(dt_plot, colnames(dt_plot[,-1]), sprintf("sample_%i", 1:ncol(y)))
  dt_plot <- melt(dt_plot, id.vars = "x")

  # Shaded area:
  y_lims <- data.frame(ymin=-2*signal_sd,ymax=2*signal_sd)

  p <- ggplot() +
    geom_rect(data=y_lims, aes(xmin=-Inf,xmax=Inf,ymin=ymin, ymax=ymax), alpha=0.1) +
    geom_line(data=dt_plot, aes(x=x, y=value, colour=variable)) +
    labs(
      x="input, x",
      y="output, f(x)"
    ) +
    scale_x_continuous(expand=c(0,0))
  p
  return(p)

}
