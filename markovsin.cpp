#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;

//#define PI 3.14159265358979323846

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//   lo que tenemos que tener es apriori, verosimilitud, posterior y 

/***R
datos<-read.table("Documents/Projects/Estadistica_Computacional/DATOS_REGRESION_VENTAS.txt",header=TRUE)
plot(datos$comisiones_pagadas,datos$ganancias_por_ventas, main= "Diagrama de dispersión")
*/
// Quiero que mi modelo sea de la forma ganancias_netas= alpha + beta* comisiones_pagadas + epsilon
// donde epsilon ~ Normal(0, sigma^2)
// Necesito conocer alpha, beta y sigma

/***R
modelo_frecuentista <- lm(datos$ganancias_por_ventas ~ datos$comisiones_pagadas)
summary(modelo_frecuentista)

mean_alpha <- -1
sd_alpha <- 2.3
mean_beta <- 3.18
sd_beta <- 0.4


*/


//[[Rcpp::export]]
double logapriori_alpha(double alpha, double mean_alpha, double sd_alpha){
  // a priori para el intercepto u ordenada al origen
  return log(R::dnorm4(alpha, mean_alpha, sd_alpha,1));
}

//[[Rcpp::export]]
double logapriori_beta(double beta, double mean_beta, double sd_beta){
  // a priori para la pendiente
  return log(R::dnorm4(beta,mean_beta,sd_beta,1));
}

//[[Rcpp::export]]
double logapriori_sigma2(double sigma2, double shape_sigma2, double scale_sigma2){
  // a priori para el intercepto u ordenada al origen
  // gamma para que sean >0 somo la sigmita
  return log(R::dgamma(sigma2,shape_sigma2,scale_sigma2,1));
}

//[[Rcpp::export]]
double logapriori(NumericVector  theta,
                  double mean_alpha, 
                  double sd_alpha,
                  double mean_beta,
                  double sd_beta,
                  double shape_sigma2,
                  double scale_sigma2){
     double alpha =theta[0];
     double beta = theta[1];
     double sigma2 =theta[2];
     return logapriori_alpha(alpha,mean_alpha,sd_alpha) +
            logapriori_beta(beta, mean_beta,sd_beta) +
            logapriori_sigma2(sigma2,shape_sigma2,scale_sigma2);
}

//[[Rcpp::export]]
double loglikelihood (NumericVector theta,
                      NumericVector X,
                      NumericVector Y){
  double alpha = theta[0];
  double beta = theta[1];
  double sigma2 =theta[2];
  // numero de observaciones
  int n =X.size();
  NumericVector error(n);
  for(int i=0; i<n; i++) {
    error[i] = Y[i]-X[i]*beta - alpha;
  }
    //sqrt(2 pi sigma^ 2)^(-N/2)  exp((Y-Xb-a)^t(Y-Xb-a)/(2 sigmA^2))
    double loglikelihood =0;
  for( int i=0; i<n ; i++) {
    loglikelihood += log((1/sqrt((2*PI)*sigma2)*exp((-1/2)*pow(error[i]/sigma2,2)))); // choriqueso con pan de formulita
  }
  return loglikelihood;
}
//[[Rcpp::export]]
double logposterior (NumericVector theta,
                     double mean_alpha, 
                     double sd_alpha,
                     double mean_beta,
                     double sd_beta,
                     double shape_sigma2,
                     double scale_sigma2,
                     NumericVector X,
                     NumericVector Y ){
  // aqui llamo logapriori y logposterior y las sumo
  double logtotal= logapriori(theta, mean_alpha,sd_alpha, mean_beta, sd_beta, shape_sigma2, scale_sigma2)+ loglikelihood(theta, X, Y);
  return logtotal; 
}