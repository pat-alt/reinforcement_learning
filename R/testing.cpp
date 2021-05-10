#include <Rcpp.h>
#include <random>
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
NumericVector generate_rewards(NumericVector prob, String method="bernoulli", int seed=42) {
  int n = prob.size();
  NumericVector v = no_init(n);
  // Random number generator:
  std::default_random_engine generator;
  // if (seed_.isNotNull()) {
  //   int seed(seed_); // casting to underlying type
  //   std::default_random_engine generator(seed);
  // }
  // Bernoulli rewards:
  if (method=="bernoulli") {
    for (int i=0; i<n; i++) {
      std::bernoulli_distribution distribution(prob[i]);
      v[i] = distribution(generator);
    }
  }
  return(v);
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
prob <- c(0.5,rep(0.4,9))
generate_rewards(prob)
*/
