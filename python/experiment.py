# Experiment: ----
def experiment(n_trials,n_simulations,k_arms,policies, dis_factor):
    # We will use dictionary objects to record the cumulative regrets
    
    results = {k: {'regrets': np.zeros(n_trials)} for k in policies}
    for policy in policies:
                
        for s in tqdm(range(n_simulations)):

            # Vectorize/initialize:
            m = zeros(k_arms)
            n_1 = zeros(k_arms)
            n_0 = zeros(k_arms)
            n_1_delta = zeros(k_arms)
            n_0_delta = zeros(k_arms)
            cum_regrets = zeros(n_trials)
            regret_count = 0
            r_matrix = np.zeros((n_trials,k_arms))
            a_matrix = np.zeros((n_trials,k_arms))
        
            # Run simulation:
            for t in range(n_trials):
                
                bandit_probs = bandits(t)
                
                if policy == 'ts':
                    action = thompson(k_arms, n_1, n_0)
                
                elif policy == 'first':
                    action = epsilon_first(k_arms, t, n_trials, 0.6, n_1, m)

                elif policy == 'delta':
                    action = thompson(k_arms, n_1_delta, n_0_delta)
                    
                elif policy == 'softmax':
                    action = soft_max(k_arms,n_1, m, t, 0.5)
                
                elif policy == 'sw_ucb':
                    action, r_matrix, a_matrix = sliding_window(1/2,t, k_arms, m, n_1, r_matrix, a_matrix,dis_factor)
                    
                elif policy == 'ucb':
                    action  = ucb(t, m, n_1,1/2)
                
                elif policy == 'discounted_ucb':
                    action, r_matrix, a_matrix = discounted_ucb(1/2,t, k_arms, m, n_1, r_matrix,a_matrix,dis_factor)

                reward = random.binomial(1, bandit_probs[action])

                if (policy == 'discounted_ucb') | (policy == 'sw_ucb'):
                    r_matrix[t,action] = reward

                regret = max(bandit_probs) - bandit_probs[action]
                regret_count += regret 
                cum_regrets[t] = regret_count
                m[action] += 1
                # success/failure
                n_1[action] += reward
                n_0 = m - n_1
                # discounted success/failure
                n_1_delta = [dis_factor*n_1_delta[n] + reward if n == action else dis_factor*n_1_delta[n] for n in range(k_arms)]
                n_0_delta = [dis_factor*n_0_delta[n] + (1-reward) if n == action else dis_factor*n_0_delta[n] for n in range(k_arms)]
            
            results[policy]['regrets'] += cum_regrets
        
    return results
