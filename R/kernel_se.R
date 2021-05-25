kernel_se <- function(
  x,
  y,
  signal_sd = 1,
  length_scale = 1
) {
  sqared_distance <- crossprod(x-y) # squared euclidean distance
  kernel <- signal_sd^2 * exp(-(1/(2*length_scale^2)) * sqared_distance)
  return(kernel)
}
