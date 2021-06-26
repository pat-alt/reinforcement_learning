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
NumericVector posterior_means(NumericVector successes, NumericVector failures, String method="bernoulli") {
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
List thompson_discounted(
    int horizon,
    int K,
    NumericMatrix prob,
    double discount_factor=1.0,
    int update_every=1,
    String method="bernoulli",
    Nullable<NumericVector> successes_ = R_NilValue,
    Nullable<NumericVector> failures_ = R_NilValue
) {

  // Initialize:
  int T = 1;
  NumericVector policy (horizon); // vector to keep track of choices
  NumericVector regret (horizon); // initialize regret as v_star (maximal true action value)
  NumericVector times_chosen (K); // counts of times selected
  NumericVector successes (K,1.0); // successes initialized to one
  NumericVector failures (K,1.0); // failures initialized to one
  // If initial success/failure values have been supplied, then overwrite:
  if (successes_.isNotNull()) {
    NumericVector successes(successes_); // casting to underlying type
  }
  if (failures_.isNotNull()) {
    NumericVector failures(failures_); // casting to underlying type
  }
  NumericVector theta=posterior_means(successes, failures, method);

  // Recursion:
  while(T <= horizon) {

    // Rcout << "Round: " << T << "\n";
    NumericVector prob_t = prob(T-1,_);
    double v_star = max(prob_t);

    // Select arm:
    int arm=select_arm(theta);

    // Observe reward:
    NumericVector rewards = generate_rewards(prob_t, method);
    int r = rewards[arm];

    // Update choices in T:
    policy[T-1] = arm+1; //update policy
    regret[T-1] = v_star - prob_t[arm]; // compute regret in this period
    times_chosen[arm] = times_chosen[arm] + 1; // update counter

    // Update:
    T++;
    successes = discount_factor * successes;
    failures = discount_factor * failures;
    if (r==1) {
      successes[arm] = successes[arm] + 1;
    } else {
      failures[arm] = failures[arm] + 1;
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
NumericVector sim_thompson_discounted(
    double n,
    int horizon,
    int K,
    NumericMatrix prob,
    double discount_factor=1.0,
    int update_every=1,
    String method="bernoulli",
    Nullable<NumericVector> successes_ = R_NilValue,
    Nullable<NumericVector> failures_ = R_NilValue
) {
  NumericVector regret (horizon); // allocate memory
  double sim_counter=1.0; // counter
  while (sim_counter <= n) {
    Rcout << sim_counter << "/" << n << "\n";
    // Run algorithm:
    List policy = thompson_discounted(
      horizon,
      K,
      prob,
      discount_factor,
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
/*** R
prob <- matrix(
  c(
    rep.int(c(0.1,0.5,0.9), 1000),
    rep.int(c(0.9,0.5,0.1), 1000),
    rep.int(c(0.5,0.5,0.5), 1000)
  ),
  nrow = 3000,
  byrow = TRUE
)
bernoulli_mab <- mab(prob)
unpack(bernoulli_mab)
thompson_discounted(
  horizon = horizon,
  K = K,
  prob = prob,
  discount_factor = 0.995,
  update_every = 1,
  method = method,
  successes_ = NULL,
  failures_ = NULL
)
*/
