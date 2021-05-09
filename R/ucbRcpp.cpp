#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List ucbRcpp(
    int horizon,
    double v_star,
    int K,
    Function uncertainty,
    Function select_arm,
    NumericVector prob,
    Function generate_rewards,
    Nullable<NumericVector> action_values_ = R_NilValue
) {

  // Initialize:
  int T = 1;
  double delta = sqrt(1/T);
  NumericVector policy (horizon); // vector to keep track of choices
  NumericVector regret (horizon, v_star); // initialize regret as v_star (maximal true action value)
  NumericVector times_chosen (K); // counts of times selected
  NumericVector action_values (K); // action_values initialized to zeros
  // If initial action values have been supplied, then overwrite:
  if (action_values_.isNotNull()) {
    NumericVector action_values(action_values_); // casting to underlying type
  }
  NumericVector u = uncertainty(action_values, times_chosen, delta); // uncertainty
  NumericVector ucb = action_values + u; // upper confidence bound

  // Recursion:
  while(T <= horizon) {

    // Select arm:
    IntegerVector arm_v = select_arm(ucb);
    int arm = arm_v[0]-1;

    // Observe reward:
    IntegerVector rewards = generate_rewards(prob);
    int r = rewards[arm];

    // Update choices in T:
    policy[T-1] = arm+1; //update policy
    regret[T-1] = regret[T-1] - prob[arm]; // compute regret in this period
    times_chosen[arm] = times_chosen[arm] + 1; // update counter
    double q = action_values[arm]; // action value at T
    int m = times_chosen[arm]; // times chose at T

    // Update:
    T++;
    double delta = sqrt(1.0/T);
    action_values[arm] = q + 1.0/m * (r - q); // action value increment
    NumericVector u_arm_v;
    u_arm_v = uncertainty(action_values[arm], m, delta);
    double u_arm = u_arm_v[0];
    ucb[arm] = action_values[arm] + u_arm; // UCB increment
  }

  // Output
  List output = List::create(
    Named("policy") = policy,
    Named("regret") = regret,
    Named("times_chosen") = times_chosen,
    Named("action_values") = action_values,
    Named("ucb") = ucb
  );

  return output;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
prob <- c(0.5,rep(0.4,9))
mab <- simulate_mab(prob)
unpack(mab)
ucbRcpp(
  horizon = 1000,
  v_star = 0.5,
  K = 10,
  uncertainty = uncertainty,
  select_arm = select_arm,
  prob=prob,
  generate_rewards=generate_rewards,
  action_values_ = rep(0.5,10)
)
*/
