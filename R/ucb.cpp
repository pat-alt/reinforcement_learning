#include <Rcpp.h>
#include <random>
using namespace Rcpp;

// Helper function to generate rewards:
// [[Rcpp::export]]
NumericVector generate_rewards(NumericVector prob, String method="bernoulli") {
  int n = prob.size();
  NumericVector v = no_init(n);
  if (method=="bernoulli") {
    for (int i=0; i<n; i++) {v[i] = as<double>(rbinom(1, 1, prob[i]));}
  }
  return(v);
}

// Helper function to select arm:
// [[Rcpp::export]]
int select_arm(NumericVector ucb) {
  int out;
  if (sum(ucb==max(ucb))==1) {
    // if there is an unambiguous maximal value, choose that one
    out = which_max(ucb);
  } else {
    // else draw uniformly from maximal values
    IntegerVector indices = seq_along(ucb)-1; // indices of values
    LogicalVector is_max = ucb==max(ucb); // boolean vector identifying maximal values
    IntegerVector candidates = indices[is_max]; // indices of maximal values
    IntegerVector out_v = sample(candidates, 1); // sample from those
    out = out_v[0];
  }
  return out;
}

// Helper function to quantify uncertainty:
// [[Rcpp::export]]
NumericVector uncertainty(NumericVector action_values, NumericVector times_chosen, double delta) {
  NumericVector out=ifelse(
    times_chosen==0,
    INFINITY, // if arm has never been selected, set to infinity
    sqrt(2 * log(1/delta) *  action_values / times_chosen) + (2*log(1/delta)) / times_chosen
  );
  return out;
}

// Main UCB function:
// [[Rcpp::export]]
List ucb(
    int horizon,
    double v_star,
    int K,
    NumericVector prob,
    String method="bernoulli",
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

    // Rcout << "Round: " << T << "\n";

    // Select arm:
    int arm=select_arm(ucb);

    // Observe reward:
    NumericVector rewards = generate_rewards(prob, method);
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
    action_values[arm] = q + 1.0/m * (r - q); // action value increment for selected arm
    u = uncertainty(action_values, times_chosen, delta); // update uncertainty
    ucb = action_values + u; // update UCB
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
//
// /*** R
// prob <- c(0.5,rep(0.4,9))
// bernoulli_mab <- mab(prob, horizon = 10000)
// unpack(bernoulli_mab)
// ucb(
//   horizon = horizon,
//   v_star = v_star,
//   K = K,
//   prob = prob,
//   method = method,
//   action_values_ = NULL
// )
// */
