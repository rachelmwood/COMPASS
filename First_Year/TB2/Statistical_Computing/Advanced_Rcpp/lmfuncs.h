// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>
using namespace arma;


vec arma_QR_lm_fast(mat& X, vec& y){
  mat Q;
  mat R;
  
  qr_econ(Q, R, X);
  vec Qty = trans(Q) * y;
  
  vec beta = solve(R, Qty);
  return beta;
}


vec dmvnInt(mat & X, vec & mu, mat & L)
{
  
  unsigned int d = X.n_cols;
  unsigned int m = X.n_rows;
  
  vec D = L.diag();
  vec out(m);
  vec z(d);
  
  double acc;
  unsigned int icol, irow, ii;
  for(icol = 0; icol < m; icol++)
  {
    for(irow = 0; irow < d; irow++)
    {
     acc = 0.0;
     for(ii = 0; ii < irow; ii++) acc += z.at(ii) * L.at(irow, ii);
     z.at(irow) = ( X.at(icol, irow) - mu.at(irow) - acc ) / D.at(irow);
    }
    out.at(icol) = sum(square(z));
  }

  out = exp( - 0.5 * out - ( (d / 2.0) * log(2.0 * M_PI) + sum(log(D)) ) );

  return out;
}


