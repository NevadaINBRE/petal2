#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]


//' Compute the metric matrix of a symmetric metric
//' @param X A matrix with the conditions in the rows and the -omics data in the columns
//' @param func a metric function that takes in two vectors of equal length
// [[Rcpp::export]]
arma::vec mmsym(arma::mat& X, Rcpp::Function func) {
  int d = X.n_cols;
  int k = d * (d - 1) / 2;
  arma::dvec D(k);
  int ij = 0;
  for (int j = 0; j < d - 1; j++) {
    for (int i = j + 1; i < d; i++) {
      D(ij++) = Rcpp::as<double>(func(X.col(i), X.col(j)));
    }
  }
  return D;
}
