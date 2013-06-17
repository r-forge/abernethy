#ifndef _MLEw2p_H
#define _MLEw2p_H

double Gfun2(arma::colvec Data, int Nf, double Bhat);

double Gfun2(arma::colvec Data, int Nf, double Bhat)  {
arma::colvec F=Data.rows(0,Nf-1);
arma::colvec DpB=pow(Data,Bhat);
double GBhat=arma::as_scalar( sum(DpB%log(Data))/sum(DpB)-sum(log(F))/Nf-1/Bhat );
return GBhat;
}


#endif
