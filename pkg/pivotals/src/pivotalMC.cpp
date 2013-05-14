/* pivotalMC.cpp
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
 * These functions implement the core MC simulation loop for generation of pivotal values
 * for both the explained variance, R squared, from median rank regression and the position of B-values for
 * preparation of confidence bounds.  For complete consistencey with SuperSMITH software
 * this function is only valid forcomplete failure data.  However, plotting points are input as median ranks,
 * so the user may experiment with datasets that have had ranks adjusted for suspensions.
 * The hope is to encourage study of Abernethy's P-value estimate methods by correlation with high quality samples.
 * and exploration into  establishing pivotal confidence bounds for data including suspensions.
 *
 *     Copyright (C) 2013 Jacob T. Ormerod
 */

#include "pivotals.h"

SEXP pivotalMCw2p(SEXP arg1, SEXP arg2, SEXP arg3, SEXP arg4, SEXP arg5, SEXP arg6){
    using namespace Rcpp ;

// mranks must be determined in calling code, choices include exact and Bernard's estimation methods
// quantity of mranks will identify the number of complete failures.
	Rcpp::NumericVector mranks(arg1);
	int F=mranks.size();

// establish output to be provided and prepare case records
	Rcpp::NumericVector SimControl(arg2);
	double R2test= SimControl[0];
	double  CItest=SimControl[1];
	int prrout=0;
	int pivout=0;

	int S = as<int>(arg3);
	int seed = as<int>(arg4);
// get the descriptive quantiles for pivitals
	Rcpp::NumericVector dq(arg5);
	int ndq = dq.size();

// variables to control a progress output
	bool ProgRpt = as<bool>(arg6);
int ProgPct=0;
int LastPct=0;

// testing has suggested that prr output does not depend on value of Beta or Eta
// but prr output would vary according to mrank differences (as with treatment of censoring)
// The pivotal quantities for confidence bounds will be effected by the sampled Beta and Eta
// but should ultimately be able to be transformed so that the median will conform to Eta=Beta=1
	double Eta = SimControl[2];
	double Beta = SimControl[3];

	RNGScope scope;
	Environment base("package:base");
	Function SetSeed = base["set.seed"];
	SetSeed(seed);

// Fill a matrix appropritate for application of linear fit XonY
// these are the y values of log(mrank)
	arma::mat X(F,2);
	X.fill(1.0);
	for (int i=0; i<F; i++) {
		X(i,1)=log(log(1/(1-mranks[i])));
	}

// establish the quantiles for evaluation of confidence bounds
	arma::colvec CBq(ndq);
	for(int i = 0; i<ndq; i++)  {
		CBq(i)=log(log(1/(1-dq[i])));
	}

// The main iteration loop to generate the population of R-square values
// for random samples of the 2-parameter weibull distribution
	arma::colvec y, coef, res;
	double Residual, TVar, pvalue, CCC2;
	arma::colvec R2(S);
	arma::mat qpiv(S,ndq);


	for(int i=0; i<S; i++)  {
		y = Rcpp::as<arma::colvec>(rweibull(F, Beta, Eta));
		y = arma::sort(y);
		y=log(y);
// solve the linear equation and extract the R-square value using Armadillo's solve function
// this method applies the "X over Y" regression of the Weibull
		coef = arma::solve(X, y);

// the prr vector is build here only if called for by output control
	if(R2test>0.0) {
		res  = y - X*coef;
		Residual = arma::as_scalar(sum(square(res)));
		TVar = arma::as_scalar(sum(square(y-mean(y))));
		R2(i) = (TVar-Residual)/TVar;
	}

// Here the pivotals for confidence bounds are built
// note that this pivotal is composed of (yp-u_hat)/b_hat, which is negative of Lawless' pivotal
// thie pivotals matrix is only built if called for by output control
	if(CItest>0.0)  {

		qpiv.row(i)=arma::trans((CBq-coef(0))/coef(1));

	}


	if(ProgRpt) {
// Progress report
ProgPct = (i+1)*100/S;
if(ProgPct > LastPct)  {
Rprintf("%3d%% completion",(i+1)*100/S);
Rprintf("\r");
R_FlushConsole();
R_ProcessEvents();
}
LastPct = ProgPct;
		}
//close main loop
	}

	int LCB=0;
	arma::rowvec LBpiv(ndq);
	arma::rowvec HBpiv(ndq);
	arma::rowvec median(ndq);




// process the prr vector according to ouput control
	if(R2test>0.0) {
	prrout = 1;
	R2=arma::sort(R2);
	if(R2test< 1.0) {
		prrout=2;
		arma::colvec Absolute(1);
		Absolute(0)=1.0;
		R2=join_cols(R2,Absolute);
// pve_u is an integer representation  of the percentile of R-square
		arma::uvec pvalue_u=arma::find(R2>R2test,1,"first");
// as long as S is sufficiently large (>10^4) there is no accuracy to be gained by interpolation
		pvalue= (double) (pvalue_u(0)) /S*100;
// note: integer math in this dimension specification may breakdown if S!= multiple of 10
		CCC2= (double) R2(S/10-1);


	}}

	if(CItest>0.0) {
		pivout=1;
		for(int i=0; i<ndq; i++)  {
			qpiv.col(i)=arma::sort(qpiv.col(i));
		}
	if(CItest< 1.0) {
		pivout=2;
		LCB=(int) S*(1-CItest)/2;
		LBpiv=qpiv.row(LCB-1);
		HBpiv=qpiv.row(S-LCB-1);
		median=qpiv.row(S/2-1);

	}}

	int outputcase=prrout+4*pivout;
	switch(outputcase)
	{
		case 1:
			return wrap(R2);
			break;
		case 2:
		return DataFrame::create(
			Rcpp::Named("Pvalue")=pvalue,
			Rcpp::Named("CCC2")=CCC2 );
			break;

	case 4:
		return wrap(qpiv);
		break;
// this is the unlikely case that both extended output objects are called for
	case 5:
		return List::create(
		Rcpp::Named("prr")=wrap(R2),
		Rcpp::Named("pivotals")=wrap(qpiv) );
		break;

	case 6:
		return List::create(
		Rcpp::Named("prrCCC2")=
		DataFrame::create(
			Rcpp::Named("Pvalue")=pvalue,
			Rcpp::Named("CCC2")=CCC2 ),
		Rcpp::Named("pivotals")=wrap(qpiv) );
		break;

	case 8:
		return DataFrame::create(
			Rcpp::Named("Lower")=wrap(arma::trans(LBpiv)),
			Rcpp::Named("Median")=wrap(arma::trans(median)),
			Rcpp::Named("Upper")=wrap(arma::trans(HBpiv)) );
		break;

	case 9:
		return List::create(
			Rcpp::Named("prr")=wrap(R2),
			Rcpp::Named("pivotals")=
			DataFrame::create(
				Rcpp::Named("Lower")=wrap(arma::trans(LBpiv)),
				Rcpp::Named("Median")=wrap(arma::trans(median)),
				Rcpp::Named("Upper")=wrap(arma::trans(HBpiv)) )
			);
		break;

	case 10:
		return List::create(
			Rcpp::Named("prrCCC2")=
			 DataFrame::create(
				Rcpp::Named("Pvalue")=pvalue,
				Rcpp::Named("CCC2")=CCC2 ),
			Rcpp::Named("pivotals")=
			DataFrame::create(
				Rcpp::Named("Lower")=wrap(arma::trans(LBpiv)),
				Rcpp::Named("Median")=wrap(arma::trans(median)),
				Rcpp::Named("Upper")=wrap(arma::trans(HBpiv)) )
			);
		break;

		default:
            return wrap(0.0);

	}

}


SEXP pivotalMCln2p(SEXP arg1, SEXP arg2, SEXP arg3, SEXP arg4, SEXP arg5, SEXP arg6){
    using namespace Rcpp ;

// mranks must be determined in calling code, choices include exact and Benard's estimation methods
// quantity of mranks will identify the number of complete failures.
	Rcpp::NumericVector mranks(arg1);
	int F=mranks.size();

// establish output to be provided and prepare case records
	Rcpp::NumericVector SimControl(arg2);
	double R2test= SimControl[0];
	double  CItest=SimControl[1];
	int prrout=0;
	int pivout=0;

	int S = as<int>(arg3);
	int seed = as<int>(arg4);
// get the descriptive quantiles for confidence bound pivitals
	Rcpp::NumericVector dq(arg5);
	int ndq = dq.size();

// variables to control a progress output
	bool ProgRpt = as<bool>(arg6);
int ProgPct=0;
int LastPct=0;

// testing has suggested that prr output does not depend on value of Mu orSigma
// but prr output would vary according to mrank differences (as with treatment of censoring)
// The pivotal quantities for confidence bounds will be effected by the sampled Mulog and Sigmalog
// but should ultimately be able to be transformed so that the median will conform to Mulog=0;Sigmalog=1
	double Mu = SimControl[2];
	double Sigma = SimControl[3];

	RNGScope scope;
	Environment base("package:base");
	Function SetSeed = base["set.seed"];
	SetSeed(seed);

// Fill a matrix appropritate for application of linear fit XonY
// these are the y values of log(mrank)
	arma::mat X(F,2);
	X.fill(1.0);
	for (int i=0; i<F; i++) {
		X(i,1)=Rf_qnorm5(mranks[i],0.0,1.0,1,0);
	}

// establish the quantiles for evaluation of confidence bounds
	arma::colvec CBq(ndq);
	for(int i = 0; i<ndq; i++)  {
		CBq(i)=Rf_qnorm5(mranks[i],0.0,1.0,1,0);
	}

// The main iteration loop to generate the population of R-square values
// for random samples of the 2-parameter weibull distribution
	arma::colvec y, coef, res;
	double Residual, TVar, pvalue, CCC2;
	arma::colvec R2(S);
	arma::mat qpiv(S,ndq);


	for(int i=0; i<S; i++)  {
		y = Rcpp::as<arma::colvec>(rnorm(F, Mu, Sigma));
		y = arma::sort(y);
//  for lognormal the datum at y is already log(y)
//y=log(y);
// solve the linear equation and extract the R-square value using Armadillo's solve function
// this method applies the "X over Y" regression of the Weibull
		coef = arma::solve(X, y);

// the prr vector is built here only if called for by output control
	if(R2test>0.0) {
		res  = y - X*coef;
		Residual = arma::as_scalar(sum(square(res)));
		TVar = arma::as_scalar(sum(square(y-mean(y))));
		R2(i) = (TVar-Residual)/TVar;
	}

// Here the pivotals for confidence bounds are built
// note that this pivotal is composed of (yp-u_hat)/s_hat, which is negative of Lawless' pivotal
// thie pivotals matrix is only built if called for by output control
	if(CItest>0.0)  {
// need to confirm coeficient use here
		qpiv.row(i)=arma::trans((CBq-coef(0))/coef(1));

	}


	if(ProgRpt) {
// Progress report
ProgPct = (i+1)*100/S;
if(ProgPct > LastPct)  {
Rprintf("%3d%% completion",(i+1)*100/S);
Rprintf("\r");
R_FlushConsole();
R_ProcessEvents();
}
LastPct = ProgPct;
		}
//close main loop
	}

	int LCB=0;
	arma::rowvec LBpiv(ndq);
	arma::rowvec HBpiv(ndq);
	arma::rowvec median(ndq);




// process the prr vector according to ouput control
	if(R2test>0.0) {
	prrout = 1;
	R2=arma::sort(R2);
	if(R2test< 1.0) {
		prrout=2;
		arma::colvec Absolute(1);
		Absolute(0)=1.0;
		R2=join_cols(R2,Absolute);
// pve_u is an integer representation  of the percentile of R-square
		arma::uvec pvalue_u=arma::find(R2>R2test,1,"first");
// as long as S is sufficiently large (>10^4) there is no accuracy to be gained by interpolation
		pvalue= (double) (pvalue_u(0)) /S*100;
// note: integer math in this dimension specification may breakdown if S!= multiple of 10
		CCC2= (double) R2(S/10-1);


	}}

	if(CItest>0.0) {
		pivout=1;
		for(int i=0; i<ndq; i++)  {
			qpiv.col(i)=arma::sort(qpiv.col(i));
		}
	if(CItest< 1.0) {
		pivout=2;
		LCB=(int) S*(1-CItest)/2;
		LBpiv=qpiv.row(LCB-1);
		HBpiv=qpiv.row(S-LCB-1);
		median=qpiv.row(S/2-1);

	}}

	int outputcase=prrout+4*pivout;
	switch(outputcase)
	{
		case 1:
			return wrap(R2);
			break;
		case 2:
		return DataFrame::create(
			Rcpp::Named("Pvalue")=pvalue,
			Rcpp::Named("CCC2")=CCC2 );
			break;

	case 4:
		return wrap(qpiv);
		break;
// this is the unlikely case that both extended output objects are called for
	case 5:
		return List::create(
		Rcpp::Named("prr")=wrap(R2),
		Rcpp::Named("pivotals")=wrap(qpiv) );
		break;

	case 6:
		return List::create(
		Rcpp::Named("prrCCC2")=
		DataFrame::create(
			Rcpp::Named("Pvalue")=pvalue,
			Rcpp::Named("CCC2")=CCC2 ),
		Rcpp::Named("pivotals")=wrap(qpiv) );
		break;

	case 8:
		return DataFrame::create(
			Rcpp::Named("Lower")=wrap(arma::trans(LBpiv)),
			Rcpp::Named("Median")=wrap(arma::trans(median)),
			Rcpp::Named("Upper")=wrap(arma::trans(HBpiv)) );
		break;

	case 9:
		return List::create(
			Rcpp::Named("prr")=wrap(R2),
			Rcpp::Named("pivotals")=
			DataFrame::create(
				Rcpp::Named("Lower")=wrap(arma::trans(LBpiv)),
				Rcpp::Named("Median")=wrap(arma::trans(median)),
				Rcpp::Named("Upper")=wrap(arma::trans(HBpiv)) )
			);
		break;

	case 10:
		return List::create(
			Rcpp::Named("prrCCC2")=
			 DataFrame::create(
				Rcpp::Named("Pvalue")=pvalue,
				Rcpp::Named("CCC2")=CCC2 ),
			Rcpp::Named("pivotals")=
			DataFrame::create(
				Rcpp::Named("Lower")=wrap(arma::trans(LBpiv)),
				Rcpp::Named("Median")=wrap(arma::trans(median)),
				Rcpp::Named("Upper")=wrap(arma::trans(HBpiv)) )
			);
		break;

		default:
            return wrap(0.0);

	}

}
