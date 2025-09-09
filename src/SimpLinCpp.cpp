#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]

using namespace Rcpp;
using namespace arma;

namespace helper {
  mat make_ci(arma::vec coef, arma::vec se, int n) {
    mat ci = mat(2,2,fill::zeros);
    double t_quant = R::qt(0.975, n-2, TRUE, FALSE);

    ci(0,0) = coef[0] - t_quant * se[0];
    ci(0,1) = coef[0] + t_quant * se[0];
    ci(1,0) = coef[1] - t_quant * se[1];
    ci(1,1) = coef[1] + t_quant * se[1];

    return ci;
  }
}

// [[Rcpp::export]]
List SimpLinCpp(arma::vec x, arma::vec y) {
  int n = x.size();
  mat X = join_rows(ones(n), x);
  
  // Obtain coefficients
  vec coef = arma::solve(X, y);

  // Calculate fitted vals, residuals, and MSE
  vec pred = coef[0] + coef[1] * x;
  vec resid = y - pred;
  double sse = arma::dot(resid,resid);
  double mse = sse / (n - 2);

  // Calculate standard errors
  mat cov = mse * inv(trans(X) * X);

  vec se = zeros(2);
  se[0] = sqrt(cov(0,0));
  se[1] = sqrt(cov(1,1));
  
  // Calculate confidence intervals
  mat ci = helper::make_ci(coef, se, n);

  // Create and return output
  List out = List::create(Rcpp::Named("coefficients") = coef,
                          Rcpp::Named("Std.Err") = se,
                          Rcpp::Named("Conf.Int") = ci,
                          Rcpp::Named("residuals") = resid,
                          Rcpp::Named("fitted.values") = pred);

  return out;
}
