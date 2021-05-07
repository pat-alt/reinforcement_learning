kl_bernoulli <- function(p,q) {
  p * log(p/q) + (1-p) * log((1-p)/(1-q))
}
