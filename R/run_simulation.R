run_simulation <- function(
  n_sim, horizon,
  K=c(10,100),
  eps=c(0.02,0.1),
  algos=list(
    ucb = sim_ucb,
    thompson = sim_thompson
  )
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
          regret <- algos[[algo]](
            n = n_sim,
            horizon = horizon,
            v_star = v_star,
            K = K,
            prob = prob
          )
          simulation_spec[,value:=cumsum(regret)] # store results
          return(simulation_spec)
        }
      )
    )
  )

  # Plot: ----
  bin_size <- ifelse(T_/1000>1,T_/1000,1)
  dt_plot <- copy(simulation)
  dt_plot <- dt_plot[,bin:=(((T_) %/% bin_size))*bin_size]
  dt_plot[,bin_idx:=1:.N,by=.(K,eps,variable,bin)]
  dt_plot <- dt_plot[bin_idx==1 | (bin==0 & bin_idx %in% round(seq(100,bin_size-bin_size/4,length.out=4)))]
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
      y="Regret"
    )
  p

  # Save chart:
  path <- file.path(system("echo $HOME"), "sim_output.png")
  ggsave(path, plot=p)
}
