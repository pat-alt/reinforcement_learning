#include <Rcpp.h>
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

// Helper function to compute posterior means (action values):
// [[Rcpp::export]]
NumericVector posterior_means(IntegerVector successes, IntegerVector failures, String method="bernoulli") {
  int K=successes.size();
  NumericVector theta = no_init(K);
  if (method=="bernoulli") {
    for (int i=0; i<K; i++) {theta[i] = as<double>(rbeta(1, successes[i], failures[i]));}
  }
  return(theta);
}

// Helper function to select arm:
// [[Rcpp::export]]
int select_arm(NumericVector theta) {
  int arm = which_max(theta);
  return arm;
}

// Main UCB function:
// [[Rcpp::export]]
List thompson(
    int horizon,
    double v_star,
    int K,
    NumericVector prob,
    int update_every=1,
    String method="bernoulli",
    Nullable<IntegerVector> successes_ = R_NilValue,
    Nullable<IntegerVector> failures_ = R_NilValue
) {

  // Initialize:
  int T = 1;
  NumericVector policy (horizon); // vector to keep track of choices
  NumericVector regret (horizon, v_star); // initialize regret as v_star (maximal true action value)
  NumericVector times_chosen (K); // counts of times selected
  IntegerVector successes (K,1); // successes initialized to one
  IntegerVector failures (K,1); // failures initialized to one
  // If initial success/failure values have been supplied, then overwrite:
  if (successes_.isNotNull()) {
    IntegerVector successes(successes_); // casting to underlying type
  }
  if (failures_.isNotNull()) {
    IntegerVector failures(failures_); // casting to underlying type
  }
  NumericVector theta=posterior_means(successes, failures, method);

  // Recursion:
  while(T <= horizon) {

    // Rcout << "Round: " << T << "\n";

    // Select arm:
    int arm=select_arm(theta);

    // Observe reward:
    NumericVector rewards = generate_rewards(prob, method);
    int r = rewards[arm];

    // Update choices in T:
    policy[T-1] = arm+1; //update policy
    regret[T-1] = regret[T-1] - prob[arm]; // compute regret in this period
    times_chosen[arm] = times_chosen[arm] + 1; // update counter

    // Update:
    T++;
    if (r==1) {
      successes[arm]++;
    } else {
      failures[arm]++;
    }
    // Check if modulus is zero:
    int update_this_round = T % update_every == 0;
    if (update_this_round) {
      theta=posterior_means(successes, failures, method);
    }
  }

  // Output
  List output = List::create(
    Named("policy") = policy,
    Named("regret") = regret,
    Named("times_chosen") = times_chosen,
    Named("action_values") = theta,
    Named("successes") = successes,
    Named("failures") = failures
  );

  return output;
}

// Function to run simulation:
// [[Rcpp::export]]
NumericVector sim_thompson(
    double n,
    int horizon,
    double v_star,
    int K,
    NumericVector prob,
    int update_every=1,
    String method="bernoulli",
    Nullable<IntegerVector> successes_ = R_NilValue,
    Nullable<IntegerVector> failures_ = R_NilValue
) {
  NumericVector regret (horizon); // allocate memory
  double sim_counter=1.0; // counter
  while (sim_counter <= n) {
    Rcout << sim_counter << "/" << n << "\n";
    // Run algorithm:
    List policy = thompson(
      horizon,
      v_star,
      K,
      prob,
      update_every,
      method,
      successes_,
      failures_
    );
    NumericVector reg_sim=policy["regret"];
    regret = regret + reg_sim / n; // increment regret
    sim_counter++;
  }
  return(regret);
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
// thompson(
//   # n = 10,
//   horizon = horizon,
//   v_star = v_star,
//   K = K,
//   prob = prob,
//   update_every = 1,
//   method = method,
//   successes_ = NULL,
//   failures_ = NULL
// )
// */
