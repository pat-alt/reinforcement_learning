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
NumericVector posteriour_means(IntegerVector successes, IntegerVector failures, String method="bernoulli") {
  int K=successes.size();
  NumericVector theta = no_init(K);
  if (method=="bernoulli") {
    for (int i=0; i<K; i++) {theta[i] = as<double>(rbeta(1, successes[i], failures[i]));}
  }
  return(theta);
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
successes <- c(100,1,1)
failures <- c(1,1,100)
posteriour_means(successes, failures)
*/
