/* MLEw2p.cpp
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 2, or (at your option) any
 * later version.
 *
 * These functions are distributed in the hope that they will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
 * GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 *
 * This is an implementation of the MLE optimization obtained by identifying the root of the derivative
 * with respect to Beta of the Weibull 2-parameter likelihood function as presented in The Weibull Handbook, Fifth Edition,
 * by Robert B. Abernethy.
 *
 * This function was developed using the RcppArmadillo library
 *
 *     Copyright (C) 2013 Jacob T. Ormerod
 */

#include "debias.h"
#include "MLEw2p.h"

SEXP MLEw2p (SEXP arg1, SEXP arg2, SEXP arg3)  
{
    using namespace Rcpp ;

	arma::colvec Data=Rcpp::as<arma::colvec>(arg1);	
	int Nf=Rcpp::as<int>(arg2);	
	Rcpp::NumericVector Control(arg3);	
	double X0=(double) Control[0];	
	double DL=(double) Control[1];	
	double DX = 0.02;	
	double X1=X0+DX;	
	double GX0=0.0,GX1=0.0,D=0.0,X2=0.0;	
		
// This is the secant method for Beta determination		
	while(pow(DX*DX,.5)>DL)  {	
		GX0=Gfun2(Data,Nf,X0);
		GX1=Gfun2(Data,Nf,X1);
		D=GX1-GX0;
		X2=X1-(X1-X0)*GX1/D;
		X0=X1;
		X1=X2;
		DX=X1-X0;
	}	
		
	double Eta=arma::as_scalar( pow(sum(pow(Data,X0))/Nf,1/X0));	
		
// Now calculate the negative log-likelihood as a measure of goodness of fit		
	double failcomp=0.0,suscomp=0.0;		
	arma::colvec F=Data.rows(0,Nf-1);		
	for(int i=0; i<Nf; i++)  {	
	failcomp=failcomp+R::dweibull(F(i),X0,Eta,1);	
	}	
	failcomp=-failcomp;	
	int Nd=Data.n_rows;	
	arma::colvec S;	
	if(Nd>Nf)  {	
		 S=Data.rows(Nf,Nd-1);
		for(int i=0; i<(Nd-Nf); i++)  {
		suscomp=suscomp+R::pweibull(S(i),X0,Eta,0,1);
		}
	}		
	double negLL=failcomp-suscomp;

	Rcpp::NumericVector outvec(3);	
	outvec[0]=Eta;	
	outvec[1]=X0;	
	outvec[2]=negLL;	
		
	return outvec;	
}
