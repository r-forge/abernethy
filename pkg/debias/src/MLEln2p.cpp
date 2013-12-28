/* MLEln2p.cpp
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
 * This is an implementation of the MLE optimization obtained by the direct Nelder-Meade simplex method
 * optimizing the negative log-likelihood function for the lognormal 2-parameter distribution consistent
 * with the R package survival. This code has been streamlined for the simplest 2 parameter case, rather  
 * than built in a generalized format for the solution of more than 2 parameters at one time. 
 * Inspiraton for this code came from a GNU implementation in C by Mike Hutt.  
 * http://www.mikehutt.com/neldermead.html 
 * His code is expository and generalized for multiple parameter solutions, however the logic was not as straight
 * forward as I would prefer.  Mike Hutt also addresses constraints, which were not applied here.
 *
 * An excellant Scholarpedia entry by Saša Singer and John Nelder (2009) helped assure accuracy of the algorithm.  
 * http://www.scholarpedia.org/w/index.php?title=Nelder-Mead_algorithm&action=cite&rev=91557
 * 
 * Botao Jia posted an excellant logic diagram at 
 * http://www.codeguru.com/cpp/article.php/c17505/Simplex-Optimization-Algorithm-and-Implemetation-in-C-Programming.htm
 * which was used to construct the final logic (although result was no different than the logic used by Mike Hutt).
 * Detail for this calculation was not presented in The Weibull Handbook, however the results are consistent with 
 * SuperSMITH software.
 *
 * This function was developed using the RcppArmadillo library
 *
 *     Copyright (C) 2013 Jacob T. Ormerod
 */

#include "debias.h"


double nLL2(arma::colvec par, arma::colvec Data, int Nf);

double nLL2(arma::colvec par, arma::colvec Data, int Nf)  {		
double failcomp=0.0,suscomp=0.0;		
arma::colvec F=Data.rows(0,Nf-1);		
for(int i=0; i<Nf; i++)  {		
failcomp=failcomp+R::dlnorm(F(i),par(0),par(1),1);		
}		
failcomp=-failcomp;		
int Nd=Data.n_rows;		
arma::colvec S;		
if(Nd>Nf)  {		
	 S=Data.rows(Nf,Nd-1);	
	for(int i=0; i<(Nd-Nf); i++)  {	
	suscomp=suscomp+R::plnorm(S(i),par(0),par(1),0,1);	
	}	
}		
double negLL=failcomp-suscomp;		
return negLL;		
}		


SEXP MLEln2p (SEXP arg1, SEXP arg2, SEXP arg3, SEXP arg4)  
{
    using namespace Rcpp ;

	arma::colvec Data=Rcpp::as<arma::colvec>(arg1);			
	int Nf=Rcpp::as<int>(arg2);			
	arma::colvec vstart=Rcpp::as<arma::colvec>(arg3);			
	double limit=Rcpp::as<double>(arg4);			
	int n=2, k=n+1;			
 // coefficients for reflection, expansion, and contraction				
	double ALPHA=1.0, BETA=0.5, GAMMA=2.0;			
 // construct the initial simplex				
	arma::mat v(2,3);			
	for(int i=0;i<k;i++)  {			
	v.col(i)=vstart;			
	}			
	v(0,1)=v(0,1)*1.2;			
	v(1,2)=v(1,2)*1.2;			
				
	arma::colvec f(3);			
	for(int i=0;i<k;i++)  {			
	f(i)=nLL2(v.col(i),Data,Nf);			
	}			
 // assign vertex order variables				
	arma::uvec ndx=sort_index(f);			
	int vs=(int) ndx(0);			
	int vh=(int) ndx(1);			
	int vg=(int) ndx(2);			
 // generate the initial error measure				
	double P2avg=arma::as_scalar(mean(v.row(1)));			
	double error=arma::as_scalar(sum(pow((v.row(1)-P2avg),2.0)/n));			
 // initialize the output dataframe for construction as a matrix				
	arma::rowvec DFrow(4);			
	DFrow(0)=v(0,vs);			
	DFrow(1)=v(1,vs);			
	DFrow(2)=f(vs);			
	DFrow(3)=error;			
	arma::mat DF=DFrow;			
				
 // initialization of variables used in the loop				
	arma::colvec vm, vr, ve, vc;			
	double fr, fe, fc;			
				
 // This is the main loop for the minimization				
	while(error>limit)  {			
 // calculate the centroid				
	vm=(v.col(vs)+v.col(vh))/2.0;			
 // reflect vg to new vertex vr				
	vr=(vm+ALPHA*(vm-v.col(vg)));			
	fr=nLL2(vr,Data,Nf);			
 // depending on success, save reflected vertex in place of vg				
	if (fr < f(vh) && fr >= f(vs)) {			
	v.col(vg)=vr;			
	f(vg)=fr;			
	}else{			
		if(f[vs]<fr)  {		
 // test for an outside contraction				
 // test for an outside contraction				
		if (fr < f(vg) && fr >= f(vh))   {		
			vc=vm+BETA*(vr-vm);	
			}else{	
 // this is an inside contraction				
				vc=vm-BETA*(vr-vm);
			}	
			fc=nLL2(vc,Data,Nf);	
 // upon acceptance replace vg with contracton				
			if (fc < f(vg)) {	
				v.col(vg) = vc;
				f(vg)=fc;
			}else{	
 // upon rare-to-never case shrink the simplex				
				v.col(vh)=v.col(vs)+(v.col(vh)-v.col(vs))/2.0;
				v.col(vg)=v.col(vs)+(v.col(vg)-v.col(vs))/2.0;
 // This case results in two function calculations and simply replacing vh and vg points				
				f(vh)=nLL2(v.col(vh),Data,Nf);
				f(vg)=nLL2(v.col(vg),Data,Nf);
			}	
		}else{		
// now we make an expansion				
			ve=vm+GAMMA*(vr-vm);	
			fe=nLL2(ve,Data,Nf);	
 // store the better of reflection or expansion in place of vg				
			if (fe < fr) {	
				v.col(vg) =ve;
				f(vg)=fe;
			}else{	
				v.col(vg) = vr;
				f(vg)=fr;
			}	
		}		
	}			
 //re-assign vertex order variables 				
	ndx=sort_index(f);			
	vs=(int) ndx(0);			
	vh=(int) ndx(1);			
	vg=(int) ndx(2);			
				
	P2avg=arma::as_scalar(mean(v.row(1)));			
	error=arma::as_scalar(sum(pow((v.row(1)-P2avg),2.0)/n));			
	DFrow(0)=v(0,vs);			
	DFrow(1)=v(1,vs);			
	DFrow(2)=-f(vs);			
	DFrow(3)=error;			
	DF=join_cols(DF,DFrow);			
				
// then close main iteration loop				
	}					
				
	return wrap(DF);				
	
}
