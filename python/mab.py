# Epsilon first: ----
def epsilon_first(k_arms, t, T, eps, n_1):
    # Pure exploration phase:
    if (t/T <= eps):
        action = random.choice(range(k_arms))
    else:
        action = argmax(n_1)
    return action

# Epsilon greedy: ----
def epsilon_greedy(k_arms,eps, n_1):
    explore = random.binomial(1, eps)
    if explore:
        action = random.choice(range(k_arms))
    else:
        action = argmax(n_1)
    return action

# Softmax: ----
def soft_max(k_arms,n_1, m, t, etha):
    mean_rewards = [n_1[arm]/m[arm] for arm in range(k_arms)]
    numerator = [np.exp((mu * (t-1))/etha) for mu in mean_rewards]
    denominator = sum(numerator)
    arm_samples = [n/denominator for n in numerator]
    action = argmax(arm_samples)
    return action

# Thompson: ----
def thompson(k_arms, n_1, n_0):
    # Sample a probability for each action
    arm_samples = [random.beta(1 + n_1[arm], 1 + n_0[arm]) for arm in range(k_arms)]
    # We take the action with the higher sample drawn
    action = argmax(arm_samples)
    return action

# UCB: ----
def ucb(t, m, n_1, alpha):
    # We compute all the components of the upper bound, for all the actions
    delta = [sqrt((alpha*log(t))/(m[arm])) for arm in range(3)]
    average = n_1/m
    # We take the action that maximized the upper bound
    action = argmax(average + delta)
    return action

# Discounted UCB: ----
def discounted_ucb(alpha,t, k_arms, m, n_1, r_matrix, a_matrix,dis_factor):
    if t <= k_arms -1:
        action = t
    else:
        denominator = [sum([(dis_factor**(t-s))*a_matrix[s,arm] for s in range(t)]) for arm in range(k_arms)]
        delta = 2*[sqrt(alpha*log(sum(denominator))/(denominator[arm])) for arm in range(k_arms)]
        average = [sum([(dis_factor**(t-s))*r_matrix[s,arm] for s in range(t)])/denominator[arm] for arm in range(k_arms)]
        action = argmax([sum(x) for x in zip(*[delta, average])])
    a_matrix[t,action] = 1
    return action, r_matrix, a_matrix

# Sliding window: ----
def sliding_window(alpha,t, k_arms, m, n_1, r_matrix, a_matrix, dis_factor):
    tau = 16
    if t <= k_arms -1:
        action = t
    else:
        denominator = [sum([(dis_factor**(t-s))*a_matrix[s,arm] for s in range(t)]) for arm in range(k_arms)]
        delta = 2*[sqrt(alpha*log(sum(denominator))/(denominator[arm])) for arm in range(k_arms)]
        if t-tau+1 < 0:
            average = [sum([(dis_factor**(t-s))*r_matrix[s,arm] for s in np.arange(0,t+1,1)])/denominator[arm] for arm in range(k_arms)]
        else:    
            average = [sum([(dis_factor**(t-s))*r_matrix[s,arm] for s in np.arange(t-tau+1,t+1,1)])/denominator[arm] for arm in range(k_arms)]
        action = argmax([sum(x) for x in zip(*[delta, average])])
    a_matrix[t,action] = 1  
    return action, r_matrix, a_matrix
