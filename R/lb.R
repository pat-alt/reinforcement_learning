#' Lower bound
#'
#' @param t Time T.
#' @param prob Action values.
#' @param kl Function to compute KL-Divergence.
#'
#' @author Patrick Altmeyer
lb <- function(t, prob, kl) {
  p_star <- max(prob)
  log(t) * sum(
    sapply(
      prob,
      function(p) {
        ifelse(p_star-p>0,(p_star-p)/kl(p,p_star),0)
      }
    )
  )
}
