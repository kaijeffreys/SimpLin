#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]

using namespace Rcpp;
using namespace arma;


// [[Rcpp::export]]
List SimpLinCpp(arma::vec x, arma::vec y) {

  // Initialize
  int n = x.size();
  mat X = mat(n, 2, fill::ones);

  for(int i = 0; i < n; i++) {
    X(i,1) = x[i];
  }

  // Obtain coefficients
  vec coef = solve(X, y);

  // Calculate fitted vals, residuals, and MSE
  vec pred = coef[0] + coef[1] * x;

  double sse = 0.0;
  vec resid = zeros(n);

  for(int i = 0; i < n; i++) {
    resid[i] =  y[i] - pred[i];
    sse = sse + resid[i] * resid[i];
  }

  double mse = sse / (n - 2);

  // Calculate SE and confidence intervals
  mat cov = mse * inv(trans(X) * X);

  vec se = zeros(2);
  se[0] = sqrt(cov(0,0));
  se[1] = sqrt(cov(1,1));
  
  mat ci = mat(2,2,fill::zeros);
  
  double t_quant = R::qt(0.975, n-2, 0, FALSE);
    
  ci(0,0) = coef[0] - t_quant * se[0];
  ci(0,1) = coef[0] + t_quant * se[0];
  ci(1,0) = coef[1] - t_quant * se[1];
  ci(1,1) = coef[1] + t_quant * se[1];

  // Create and return output
  List out = List::create(Rcpp::Named("coefficients") = coef,
                          Rcpp::Named("Std.Err") = se,
                          Rcpp::Named("Conf.Int") = ci,
                          Rcpp::Named("residuals") = resid,
                          Rcpp::Named("fitted.values") = pred);


  return out;
}
