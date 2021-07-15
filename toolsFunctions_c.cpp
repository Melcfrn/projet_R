#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;
using namespace std;
// [[Rcpp::export]]
List fastLm(const mat& X, const colvec& y, const rowvec& xi, const int& yi) {
  colvec coef = inv(X.t() * X) * (X.t() * y);  
  colvec res  = xi*coef - yi;           
  return List::create(Named("coefficients") = coef,
                      Named("residual")  = res);
}

// [[Rcpp::export]]
List fastLm_noindex(const mat& X, const colvec& y) {
  colvec coef = inv(X.t() * X) * (X.t() * y);          
  return List::create(Named("coefficients") = coef);
}