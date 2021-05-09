# Methods: ----
print.policy <- function(policy) {
  print(sprintf("Loss after %i trials: %0.2f", policy$mab.horizon, sum(policy$regret)))
}

plot.policy <- function(policy,...) {
  plot(x=log10(1:length(policy$regret)),y=cumsum(policy$regret),t="l",xlab="Trials",ylab="Cumulative regret",...)
}
