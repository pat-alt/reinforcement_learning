# Epsilon first: ----
def epsilon_first(k_arms, t, T, eps, n_1):
    # Pure exploration phase:
    if (t/T <= eps):
        action = random.choice(range(k_arms))
    else:
        action = argmax(n_1)
    return action

# Epsilon greedy: ----
def epsilon_greedy(eps, n_1):
    explore = random.binomial(1, eps)
    if explore:
        action = random.sample(range(k_arms), 1)[0]
    else:
        action = argmax(n_1)
    return action

# UCB: ----
def ucb(t, m, n_1):
    # We compute all the components of the upper bound, for all the actions
    delta = sqrt(1/(t+1))
    first_term = n_1/m
    second_term = (2*log(1/delta))/m
    third_term = sqrt((2*first_term*log(1/delta))/m)
    # We take the action that maximized the upper bound
    action = argmax(first_term + second_term + third_term)
    return(action)

# Thompson: ----
def thompson(k_arms, n_1, n_0):
    # Sample a probability for each action
    arm_samples = [random.beta(1 + n_1[arm], 1 + n_0[arm]) for arm in range(k_arms)]
    # We take the action with the higher sample drawn
    action = argmax(arm_samples)
    return action

# Delta Thompson: ----
def delta_thompson(k_arms, n_1, n_0, t, delta):
    # Sample a probability for each action
    arm_samples = [random.beta(1 + delta**t * n_1[arm], 1 + delta**t * n_0[arm]) for arm in range(k_arms)]
    # We take the action with the higher sample drawn
    action = argmax(arm_samples)
    return action
