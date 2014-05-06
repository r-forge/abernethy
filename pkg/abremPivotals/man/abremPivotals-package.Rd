\name{abremPivotals-package}
\alias{abremPivotals}
\docType{package}
\title{
Linear Rank Regression Models with Pivotal Monte Carlo Methods for Reliability Analysis 
}
\description{
Fast linear regression with optional plotting position method selections for Weibull, lognormal, and Gumbel distributions including right censored data.  Implements 3rd parameter optimizations and a fast pivotal Monte Carlo engine for goodness of fit and confidence interval determination. 

Intended to be a reverse depend for package abrem "Abernethy Reliability Methods". Consistent with "The New Weibull Handbook, Fifth Edition" by Dr. Robert B. Abernethy.

}

\section{Introduction}{
	\pkg{abremPivotals} This package provides the technical code for linear regression fitting of reliability data as it is presented in "The New Weibull Handbook, Fifth Edition" by Dr. Robert B. Abernethy.  A wide selection of methods for estimating plotting positions indluding right-censored data, known as suspensions, is provided.  Engineering reliability problems tend to differ from biostatistic problems in the lack of available data points.  The cost of failure in engineering problems can be so great that determinations must be made given small sets of failure experience.  Linear regression methods, specifically median rank regression, is preferred due to high bias of the maximum likelihood estimate given small data sets.  In order to compensate for the loss of unbiased goodness of fit measures and the production of confidence intervals, the practice of Monte Carlo pivotal analysis (perhaps bootstrapping) has been employed by the engineering community for several decades on ordinary linear regression models.  This package provides a fast implementation of such a pivotal engine, written using Rcpp and RcppArmadillo.  Third parameter optimizations are also implemented for the covered distributions.
	
	This package has been prepared to be a reverse depend for an application layer package, abrem, however it can be quite usefull on its own for certain studies.
}

\author{
Jacob T. Ormerod

Maintainer: Jacob T. Ormerod <jake@openreliability.org>
}
\references{
 Dr. Robert B. Abernethy, (2008) "The New Weibull Handbook, Fifth Edition"
 
 Eddelbuettel D, (2013) "Seamless R and C++ Integration with Rcpp"
 
 Leonard G Johnson, (1964) "The Statistical Treatment of Fatigue Experiments"
 
 Jerald F. Lawless, (2003) "Statistical Models and Methods for Lifelime Data, Second Edition" 
 
 ReliaSoft Corporation (Retrieved 2014) "Life Data Analysis Reference Book" 
}

\keyword{ package reliability }



