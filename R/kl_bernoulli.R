#' KL-Divergence for Bernoulli
#'
#' @param p Probability of first Bernoulli distribution.
#' @param q Probability of second Bernoulli distribution.
#'
#' @author Patrick Altmeyer
kl_bernoulli <- function(p,q) {
  p * log(p/q) + (1-p) * log((1-p)/(1-q))
}
