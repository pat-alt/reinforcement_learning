#' Simulation
#'
#' @param n_sim Number of simulations.
#' @param horizon Number of trials.
#' @param K Number of arms.
#' @param eps Difference between best and worst action values.
#' @param algos Compiled simulation functions for different algorithms.
#'
#' @author Patrick Altmeyer
run_simulation <- function(
  n_sim=5, horizon=100000, update_every=1,
  K=c(10,100),
  eps=c(0.02,0.1),
  algos=list(
    ucb = sim_ucb,
    thompson = sim_thompson
  ),
  file=NULL,
  n_start=100
) {

  # Setup: ----
  t_0 <- 1
  T_ <- horizon
  param_grid <- CJ(K=K,eps=eps,algo=names(algos))
  simulation <- CJ(K=K,eps=eps,T_=t_0:T_)
  # Lower bound:
  simulation[
    ,
    value:=lb(T_,c(0.5,rep(0.5-eps,K-1)),kl_bernoulli) - lb(100,c(0.5,rep(0.5-eps,K-1)),kl_bernoulli),
    by=.(K,eps)
  ]
  simulation[,variable:="lower_bound"]

  # Run simulation: ----
  start <- Sys.time()
  simulation <- rbind(
    simulation,
    rbindlist(
      lapply(
        1:nrow(param_grid),
        function(param_spec) {
          list2env(c(param_grid[param_spec,]), envir = environment()) # unpack variables
          prob <- c(0.5,rep(0.5-eps,K-1)) # compute corresponding probabilities
          bernoulli_mab <- mab(prob, horizon = T_) # generate MAB
          unpack(bernoulli_mab) # unpack objects
          # initialize table to store output for parameter specification:
          simulation_spec <- data.table(K=K, eps=eps, T_=t_0:T_, value=0, variable=algo)
          # run simulation:
          message(sprintf("Starting with: %s, %i arms, eps=%0.2f, T=%i, n_sim=%i", algo, K, eps, T_, n_sim))
          regret <- algos[[algo]](
            n = n_sim,
            horizon = horizon,
            v_star = v_star,
            K = K,
            prob = prob,
            update_every = update_every
          )
          message(sprintf("Done with: %s, %i arms, eps=%0.2f, T=%i, n_sim=%i", algo, K, eps, T_, n_sim))
          simulation_spec[,value:=cumsum(regret)] # store results
          return(simulation_spec)
        }
      )
    )
  )
  time_passed <- Sys.time() - start
  print(time_passed)

  # Plot: ----
  bin_size <- ifelse(T_/1000>1,T_/1000,1)
  dt_plot <- copy(simulation)
  dt_plot <- dt_plot[,bin:=(((T_) %/% bin_size))*bin_size]
  dt_plot[,bin_idx:=1:.N,by=.(K,eps,variable,bin)]
  dt_plot <- dt_plot[bin_idx==1 | (bin==0 & bin_idx %in% round(seq(100,bin_size-bin_size/n_start,length.out=n_start)))]
  dt_plot[,bin_idx:=NULL]
  dt_plot[,bin:=NULL]
  dt_plot[,K:=sprintf("K==%i",K)]
  dt_plot[,eps:=sprintf("epsilon==%0.2f",eps)]
  dt_plot[,variable:=as.factor(variable)]
  levels(dt_plot$variable) <- c("Lower bound", "Thompson", "UCB")
  p <- ggplot(data=dt_plot[T_>=100]) +
    geom_line(aes(x=T_, y=value, colour=variable)) +
    scale_x_log10(
      breaks = trans_breaks("log10", function(x) 10^x),
      labels = trans_format("log10", math_format(10^.x))
    ) +
    scale_color_discrete(name="Variable:") +
    facet_wrap(eps ~ K, scales="free", labeller = label_parsed) +
    labs(
      x="T",
      y="Regret",
      title=sprintf("Number of simulations: %i", n_sim),
      subtitle = sprintf("Updated every %i trials", update_every)
    )

  # Save chart:
  if (is.null(file)) {
    ggsave("www/user_sim.png", width = 6, height = 4)
  } else {
    file_path <- file.path("www", file)
    ggsave(file_path, width = 6, height = 4)
  }

}
