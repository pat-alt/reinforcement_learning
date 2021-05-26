library(data.table)
branin <- function(
  x,
  y,
  a=1,
  b=5.1/(4*pi^2),
  c=5/pi,
  r=6,
  s=10,
  t=1/(8*pi)
) {

  f <- a * (y - b*x^2 + c*x - r)^2 + s * (1-t) * cos(x) + s

  function_output <- list(
    output = f,
    x = x,
    y = y
  )

  return(function_output)
}
