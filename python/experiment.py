def experiment(n_trials,n_simulations,k_arms,epsilon, policies):
    
    # We will use dictionary objects to record the cumulative regrets
    results = {k: {'regrets': np.zeros((1,int(n_trials/100)))[0]} for k in policies}
    
    for policy in policies:
      
      for s in tqdm(range(n_simulations)):
        
        # We keep track of the chosen actions, the number of successes and the number of failures:
        m = zeros(k_arms)
        n_1 = zeros(k_arms)
        cum_regrets = zeros((1,int(n_trials/100)))[0]
        n_0 = m - n_1
        regret_count = 0

        # loop for each round
        
        for t in range(n_trials):
          
          bandit_probs = bandits(k_arms,epsilon)
          
          if t % 100 == 0:
            # Based on method, choose action:
            if policy == 'ts':
              action = thompson(k_arms, n_1, n_0)
            elif policy == 'ucb':
              action = ucb(t, m, n_1)
            # Update regret:
            regret = 0.5 - bandit_probs[action]
            regret_count += regret 
            cum_regrets[int(t/100)] = regret_count
          else:
            # We add this to the regret count, so that when keeping track of the cumulative regret
            # we actually do not lose any information in the meanwhile
            regret_count += regret
             
          # We compute the reward, so that we can update the number of successes. 
          reward = random.binomial(1, bandit_probs[action])  
                  
        # We record all the information related to this round
        m[action] += 1
        n_1[action] += reward
        n_0 = m - n_1

      # We add the results of this given simulation
      results[policy]['regrets'] += cum_regrets
        
    return results
