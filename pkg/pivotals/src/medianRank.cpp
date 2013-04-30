/* medianRank.cpp
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
 * This is function implements the adjusted ranks per Leonard G. Johnson for handling suspended
 * data events(as simplified by Drew Auth). The function also applies Bernard's approximation
 * within a single loop to generate the median rank, point estimates on the adjusted ranks.
 * This function is consistent with The Weibull Handbook, Fifth Edition and SuperSMITH software.
 *
 * This function was developed using the RcppArmadillo library
 *
 *     Copyright (C) 2013 Jacob T. Ormerod
 */

#include "pivotals.h"



SEXP medianRank (SEXP arg1)
{
    using namespace Rcpp ;

	arma::colvec event = Rcpp::as<arma::colvec>(arg1);
	int N = event.n_rows;
// abundance of caution regarding mixed-type math (int and double)
	double Ndbl = (double) N;
	int F = arma::as_scalar(sum(event));
	arma::colvec adj_rank(N+1);
	adj_rank.fill(0.0);
// median_rank is only used to accumulate the elements
// of the final return vector, so NumericVector is best choice
	Rcpp::NumericVector median_rank(F);
	for(int i=1,j=0; i<N+1; i++)
	{
		double rr=(double)(N-i)+1.0;
		if(event(i-1)>0)
		{
			adj_rank(i)= (rr*adj_rank(i-1)+Ndbl+1.0)/(rr+1.0);
			if(j<F) {
			median_rank[j]=(adj_rank(i)-0.3)/(Ndbl+0.4);
			j++; }
		}
		else
		{
			adj_rank(i)=adj_rank(i-1);
		}
	}

	return median_rank;

}


SEXP medianRank1 (SEXP arg1)
{
    using namespace Rcpp ;

	arma::colvec event = Rcpp::as<arma::colvec>(arg1);
	int N = event.n_rows;
// abundance of caution regarding mixed-type math (int and double)
	double Ndbl = (double) N;
	int F = arma::as_scalar(sum(event));
	arma::colvec adj_rank(N+1);
	adj_rank.fill(0.0);
// median_rank is only used to accumulate the elements
// of the final return vector, so NumericVector is best choice
	Rcpp::NumericVector median_rank(F);
	for(int i=1,j=0; i<N+1; i++)
	{
		double rr=(double)(N-i)+1.0;
		if(event(i-1)>0)
		{
			adj_rank(i)= (rr*adj_rank(i-1)+Ndbl+1.0)/(rr+1.0);
			if(j<F) {
			median_rank[j]=Rf_qbeta(0.5,adj_rank(i),rr,1,0);
			j++; }
		}
		else
		{
			adj_rank(i)=adj_rank(i-1);
		}
	}

	return median_rank;

}
