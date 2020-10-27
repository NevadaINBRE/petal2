petal_euclidean_Ptr <- RcppXPtrUtils::cppXPtr(
"double petal_euclidean(const arma::mat &A, const arma::mat &B) {
  return sqrt(arma::accu(arma::square(A - B)));
}", depends = c("RcppArmadillo"))
