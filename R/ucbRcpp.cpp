#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
List ucbRcpp(
    int horizon,
    double v_star,
    int K,
    NumericVector action_values,
    Function uncertainty,
    Function select_arm,
    NumericVector prob,
    Function fun
) {

  // Initialize:
  int T = 1;
  double delta = sqrt(1/T);
  NumericVector policy (horizon); // vector to keep track of choices
  NumericVector regret (v_star, horizon); // initialize regret as v_star (maximal true action value)
  NumericVector times_chosen (K); // counts of times selected
  if (Rf_isNull(action_values)) {
    NumericVector action_values (K); // action_values
  }
  NumericVector u; // uncertainty
  u = uncertainty(action_values, times_chosen, delta);
  NumericVector ucb; // upper confidence bound
  ucb = action_values + u;
  IntegerVector arm_v;
  IntegerVector rewards;
  int r;

  // Recursion:
  while(T <= horizon) {

    // Select arm:
    arm_v = select_arm(ucb);
    int arm = arm_v[0]-1;

    // Observe reward:
    rewards = fun(prob);
    r = rewards[arm];

    // Update choices in T:
    policy[T-1] = arm+1; //update policy
    double regr = regret[T-1] - prob[arm];
    regret[T-1] = 1; // compute regret in this period
    // times_chosen[arm] = times_chosen[arm] + 1; // update counter
    // double q = action_values[arm]; // action value at T
    // int m = times_chosen[arm]; // times chose at T

    // Update:
    T++;
    // double delta = sqrt(1/T);
    // action_values[arm] = q + 1/m * (r - q); // action value increment
    // IntegerVector u_arm_v;
    // u_arm_v = uncertainty(action_values[arm], m, delta);
    // double u_arm = u_arm_v[0];
    // ucb[arm] = action_values[arm] + u_arm; // UCB increment
  }

  // Output
  List output = List::create(
    Named("policy") = policy,
    Named("regret") = regret,
    Named("times_chosen") = times_chosen,
    Named("action_values") = action_values,
    Named("ucb") = ucb,
    Named("return") = r
  );

  return output;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
prob <- c(0.5,rep(0.4,9))
fun <- function(prop) {
  sapply(
    prob,
    function(p) {
      rbinom(1,1,p)
    }
  )
}
ucbRcpp(1000, 0.5, 10, rep(0,10), uncertainty = uncertainty, select_arm = select_arm, prob=prob, fun=fun)
*/
