#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]


// Thanks to: https://math.stackexchange.com/a/646125
int lower_tri_idx(int d, int i, int j) {
  return (2*j*d - j*j + 2*i - 3*j - 2) / 2;
}


// [[Rcpp::export]]
arma::vec mmsym(arma::mat& X, Rcpp::Function func) {
  int d = X.n_cols;
  int k = d * (d - 1) / 2;
  arma::dvec D(k);
  for (int j = 0; j < d - 1; j++) {
    for (int i = j + 1; i < d; i++) {
      D(lower_tri_idx(d, i, j)) = Rcpp::as<double>(func(X.col(i), X.col(j)));
    }
  }
  return D;
}
