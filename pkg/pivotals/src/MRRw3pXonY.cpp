/* MRRw3pXonY.cpp
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
 * This function optimizes the R^2 regression error by applying a third parameter, t0, translating
 * all event data (failures and suspensions) by subtraction of the t0 amount.  Positive t0 may
 * be indicative of a failure free period (such as often encountered with bearings), while a 
 * negative t0 would suggest some pre-stress placed on samples prior to placement in service.
 * A 3-parameter model will always improve the fit of data (to the extent it is valid) since it is 
 * a more complex model.  Dr. Abernethy advises caution on its application citing needs for physical
 * explaination, and sufficient failure points (more than 21, unless fewer can be supported by prior 
 * knowledge). 
 * Somewhat beyond the text of The New Weibull Handbook, Fifth Edition, there has been some
 * suggestion that validation of a 3p model over 2p counterpart might also include a likelihood ratio
 * test with a Chi_squared P-value >50%.
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
 *
 *     Copyright (C) 2013 Jacob T. Ormerod
 */

#include "pivotals.h"
#include <math.h>


    using namespace Rcpp ;
	
	double dR2dx(double X, NumericVector &data, NumericVector &event,double limit)  {			
	int N=data.size();		
	Rcpp::NumericVector mdata(N);		
	for(int i=0; i<N; i++) {mdata[i]=data[i]-X;}		
	Rcpp::NumericVector fit1=MRRw2pXonY(mdata,event);		
	for(int i=0; i<N; i++) {mdata[i]=data[i]-(X-0.1*limit);}		
	Rcpp::NumericVector fit2=MRRw2pXonY(mdata,event);		
	double slope=(fit1[2]-fit2[2])/(0.1*limit);		
			
	return slope;		
}			

SEXP MRRw3pXonY (SEXP arg1, SEXP arg2, SEXP arg3)
{
	Rcpp::NumericVector time(arg1);			
	Rcpp::NumericVector event(arg2);			
	double DL=as<double>(arg3);			
	int N=time.size();			
				
				
				
	int maxit=100;			
				
	double C1=time[N-1];			
 // get min(x), the minimum complete failure as constraint C1				
	for(int i=0; i<N; i++)  {			
		if(event[i]==1)  {		
 // just get the first failure, they were required to have been sorted				
			C1=time[i];	
			break;	
		}		
	}			
 // Tao Pang's original variable labels from FORTRAN are used where possible				
 // initial step is based on limit*10,000				
	double DX=DL*pow(10,4);			
	double X0=0.0;			
	int istep=0;			
	double X1=X0+DX;			
	if(X1>C1) {X1=X0+0.9*(C1-X0);}			
				
	double FX0=dR2dx(X0,time,event,DL);			
	double FX1=dR2dx(X1,time,event,DL);			
 // FX1 will contain slope sign information to be used only one time to find X2				
	double D=fabs(FX1-FX0);			
	double X2=X1+fabs(X1-X0)*FX1/D;			
	if(X2>C1) {X2=X1+0.9*(C1-X1);}			
	X0=X1;			
	X1=X2;			
 // development diagnostic code				
 //	arma::rowvec DFrow(4);			
 //	DFrow(0)=istep;			
 //	DFrow(1)=X0;			
 //	DFrow(2)=DX;			
 //	DFrow(3)=FX1;			
 //	arma::mat DF;			
 //	DF=DFrow;			
				
				
 // This is the start of the main loop				
	while(fabs(DX)>DL&& istep<maxit)  {			
	FX0=FX1;			
	FX1=dR2dx(X1,time,event,DL);			
 // FX1 will contain slope sign information to be used only one time to find X2				
	D=fabs(FX1-FX0);			
	X2=X1+fabs(X1-X0)*FX1/D;			
	if(X2>C1) {X2=X1+0.9*(C1-X1);}			
	X0=X1;			
	X1=X2;			
	DX=X1-X0;			
	istep=istep+1;			
 // development diagnostic code				
 //	DFrow(0)=istep;			
 //	DFrow(1)=X0;			
 //	DFrow(2)=DX;			
 //	DFrow(3)=FX1;			
 //	DF=join_cols(DF,DFrow);			
	}			
				
	Rcpp::NumericVector mdata(N);			
	for(int i=0; i<N; i++) {mdata[i]=time[i]-X0;}			
	Rcpp::NumericVector finalfit=MRRw2pXonY(mdata,event);			
	Rcpp::NumericVector outvec(4);			
	outvec[0]=finalfit[0];			
	outvec[1]=finalfit[1];			
	outvec[2]=X0;			
	outvec[3]=finalfit[2];			
				
	return wrap(outvec);			


	}
