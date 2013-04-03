\name{pivotals-package}
\alias{pivotals}
\docType{package}
\title{
Functions to support pivotal analysis studies
}
\description{
Pivotal analysis is used to establish goodness of fit by means of Critical Correlation Coeficient (CCC) 
 and the p-value of r and r^2.  This analysis is also used and to establish confidence interval bounds
 for a given data set size. Only complete failure data is compliant with the New Weibull Handbook. 

}
\details{
Functions:
\code{CBpiv}\cr
\code{mrank}\cr
\code{MRRw2pxy}\cr
\code{prrVal}\cr
\code{prrVec}\cr

}
\section{Introduction}{
	\pkg{pivotals} This package features a multi-purpose pivotal engine written in fast C++ code 
	based on MRR X on Y fitting of random data for a selected model.
	
	 New research is in progress to define a best practice for handling suspension data in the pivotal analysis.
	 It is an express purpose of this package to facilitate study of this new development.
	 This package will also be used for the development of tabulated data for the CCC at high resolution
	 and the development of p-value estimates based on such tabulated data.
}


\author{
Jacob T. Ormerod

Maintainer: David J. Silkworth <davidsilkworth@openreliability.me>
}
\references{
 Dr. Robert B. Abernethy, (2008) "The New Weibull Handbook, Fifth Edition"
 Jerald F. Lawless, (2003) "Statistical Models and Methods for Lifelime Data, Second Edition" 
}

\keyword{ package }



