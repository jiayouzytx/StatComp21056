#include <Rcpp.h>
using namespace Rcpp;
//' @title A Gibbs sampler using Rcpp
//' @description A Gibbs sampler using Rcpp
//' @param a the fixed parameter
//' @param b the fixed parameter
//' @param N the fixed parameter
//' @return a chain with tagret joint density \code{x}
//' @examples
//' \dontrun{
//' a<-1
//' b<-1
//' N<-10000
//' X1<-fun98(a,b,N)
//' plot(X1[,1],X1[,2],xlab = "x",ylab = "y")
//' }
//' @export
// [[Rcpp::export]]
NumericMatrix fun98( int a,int b,int N) {
  NumericMatrix x(N, 2);
  double x1 = 0, x2 = 0;
  double t1 = 0, t2 = 0;
  
  for(int i = 1; i < N; i++) {
    x2=x(i-1,1);
    t1=rbinom(1,25,x2)[0];
    x(i,0)=t1;
    x1=x(i,0);
    t2=rbeta(1,x1+a,25-x1+b)[0];
    x(i,1)=t2;
  }
  return(x);
}