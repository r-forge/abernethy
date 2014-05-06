/* MRR.cpp					
 *					
 * This program is free software; you can redistribute it and/or modify it					
 * under the terms of the GNU General Public License as published by the					
 * Free Software Foundation; either version 2, or (at your option) any					
 * later version.					
 *					
 * These functions are distributed in the hope that they will be useful,					
 * but WITHOUT ANY WARRANTY; without even the implied warranty of					
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the					
 * GNU General Public License for more details.					
 *					
 *  You should have received a copy of the GNU General Public License					
 *  along with this program; if not, a copy is available at					
 *  http://www.r-project.org/Licenses/					
 *					
 * This collectioin of functions implement Median Rank Regression (MRR). Alternate use of  X~Y ordering					
 * enables comparison, while X on Y has been designated as "best practice"					
 * in "The Weibull Handbook, Fifth Edition" by Dr. Robert B. Abernethy for fitting fatique-life data					
 * to the Weibull distribution.					
 *					
 * Two arguements are required: a vector of data values (often recorded as time), and an equal size					
 * vector signifying event termination, 1 for failure, 0 for suspension (censored).					
 * The vectors must be of equal length and sorted according to ascending data values.					
 * Unchecked disaster will result otherwise.					
 * This function calls the pivotals package C++ function medianRank() directly, so Benard's approximation					
 * is applied to the ranks adjusted as applicable for suspensions.					
 * These functions are consistent with The Weibull Handbook, Fifth Edition and SuperSMITH software.					
 *					
 * This function was developed using the RcppArmadillo library					
 * Author: Jacob Ormerod					
 *     Copyright (C) 2013-2014 OpenReliability.org					
 */					
					
#include "abremPivotals.h"	
#include <math.h>				
					
SEXP MRRw2pXonY (SEXP arg1, SEXP arg2)		
{		
    using namespace Rcpp ;		
		
        Rcpp::NumericVector fail(arg1);		
        Rcpp::NumericVector position(arg2);		
		
		
        int F=fail.size();		
// declare the arma objects with n_rows = F for bounds checking		
        arma::mat X(F,2);		
        arma::colvec y(F);		
// fill the arma objects		
        for(int i=0; i<F; i++)  {		
	X(i,0)=1.0;	
	X(i,1)=log(log(1/(1-position[i])));	
	y(i)=log(fail[i]);	
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
				

SEXP MRRw2pYonX (SEXP arg1, SEXP arg2)		
{		
    using namespace Rcpp ;		
		
        Rcpp::NumericVector fail(arg1);		
        Rcpp::NumericVector position(arg2);		
		
		
        int F=fail.size();		
// declare the arma objects with n_rows = F for bounds checking		
        arma::mat X(F,2);		
        arma::colvec y(F);		
// fill the arma objects		
        for(int i=0; i<F; i++)  {		
	X(i,0)=1.0;	
	X(i,1)=log(fail[i]);	
	y(i)=log(log(1/(1-position[i])));	
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



SEXP MRRg2pXonY (SEXP arg1, SEXP arg2)		
{		
    using namespace Rcpp ;		
		
        Rcpp::NumericVector fail(arg1);		
        Rcpp::NumericVector position(arg2);		
		
		
        int F=fail.size();		
// declare the arma objects with n_rows = F for bounds checking		
        arma::mat X(F,2);		
        arma::colvec y(F);		
// fill the arma objects		
        for(int i=0; i<F; i++)  {		
	X(i,0)=1.0;	
	X(i,1)=log(log(1/(1-position[i])));	
	y(i)=log(fail[i]);	
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
		


SEXP MRRg2pYonX (SEXP arg1, SEXP arg2)		
{		
    using namespace Rcpp ;		
		
        Rcpp::NumericVector fail(arg1);		
        Rcpp::NumericVector position(arg2);		
		
		
        int F=fail.size();		
// declare the arma objects with n_rows = F for bounds checking		
        arma::mat X(F,2);		
        arma::colvec y(F);		
// fill the arma objects		
        for(int i=0; i<F; i++)  {		
	X(i,0)=1.0;	
	X(i,1)=log(fail[i]);	
	y(i)=log(log(1/(1-position[i])));	
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
	


        SEXP MRRln2pXonY (SEXP arg1, SEXP arg2)			
{			
    using namespace Rcpp ;			
			
        Rcpp::NumericVector fail(arg1);			
        Rcpp::NumericVector position(arg2);			
			
			
        int F=fail.size();			
// declare the arma objects with n_rows = F for bounds checking			
        arma::mat X(F,2);			
        arma::colvec y(F);			
// fill the arma objects			
        for(int i=0; i<F; i++)  {			
	X(i,0)=1.0;		
	X(i,1)=Rf_qnorm5(position[i],0.0,1.0,1,0);		
	y(i)=log(fail[i]);		
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
	


    SEXP MRRln2pYonX (SEXP arg1, SEXP arg2)		
    {		
        using namespace Rcpp ;		
		
        Rcpp::NumericVector fail(arg1);		
        Rcpp::NumericVector position(arg2);		
		
		
        int F=fail.size();		
// declare the arma objects with n_rows = F for bounds checking		
        arma::mat X(F,2);		
        arma::colvec y(F);		
// fill the arma objects		
        for(int i=0; i<F; i++)  {		
	X(i,0)=1.0;	
	X(i,1)=log(fail[i]);	
	y(i)=Rf_qnorm5(position[i],0.0,1.0,1,0);	
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
	
		
		