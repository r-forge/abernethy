/* MRRw2pXonY.cpp
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
 * This is function implements Median Rank Regression (MRR)using X on Y ordering as "best practice"
 * in "The Weibull Handbook, Fifth Edition" by Dr. Robert B. Abernethy for the 2-parameter Weibull distribution.
 * Two arguements are required: a vector of data values (often recorded as time), and an equal size
 * vector signifying event termination, 1 for failure, 0 for suspension (censored).
 * The vectors must be of equal length and sorted according to ascending data values.
 * Unchecked disaster will result otherwise.
 * This function calls the pivotals package C++ function medianRank() directly, so Bernare's approximation
 * is applied to the ranks adjusted as applicable for suspensions.
 * This function is consistent with The Weibull Handbook, Fifth Edition and SuperSMITH software.
 *
 * This function was developed using the RcppArmadillo library
 *
 *     Copyright (C) 2013 Jacob T. Ormerod
 */

#include "pivotals.h"

SEXP MRRw2pXonY (SEXP arg1, SEXP arg2)
{
    using namespace Rcpp ;

	Rcpp::NumericVector time(arg1);
	Rcpp::NumericVector event(arg2);
	Rcpp::NumericVector mrank(medianRank(event));
	int N=time.size();
	int F=mrank.size();
// declare the arma objects with n_rows = F for bounds checking
	arma::mat X(F,2);
	arma::colvec y(F);
// fill the arma objects
	for(int i=0,j=0; i<N; i++)  {
		if(event[i]>0) {
			X(j,0)=1.0;
			X(j,1)=log(log(1/(1-mrank[j])));
			y(j)=log(time[i]);
			j++;
		}
	}
	arma::colvec coef, res;
	double Residual, TVar, R2;

// solve the linear equation and extract the R-square value using Armadillo's solve function
// this method applies the "X over Y" regression of the Weibull
	coef = arma::solve(X, y);
	res  = y - X*coef;
	Residual = arma::as_scalar(sum(square(res)));
	TVar = arma::as_scalar(sum(square(y-mean(y))));
	R2 = (TVar-Residual)/TVar;
// Finally prepare a single vector with each coefficient and the variance (R2)
	Rcpp::NumericVector outvec(3);
	outvec[0]=exp(coef(0));
	outvec[1]=1/coef(1);
	outvec[2]=R2;

	return outvec;

	}
