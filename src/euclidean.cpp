#include <Rcpp.h>
using namespace Rcpp;


//' Compute the Euclidean distance
//'
//' @param x first vector
//' @param y second vector
// [[Rcpp::export]]
double euclidean(NumericVector x, NumericVector y) {
  return sqrt(sum( pow(x - y, 2) ));
}
