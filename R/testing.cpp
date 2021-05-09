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
NumericVector generate_rewards(int n, double size, NumericVector prob) {
  NumericVector v = no_init(n);
  for (int i=0; i<n; i++) {v[i] = as<double>(rbinom(1, size, prob[i]));}
  return(v);
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
generate_rewards(length(prob),1,prob)
*/
