\name{MLEw2p_cpp}

\alias{MLEw2p_cpp}

\title{Weibull 2-parameter MLE calculation.} 

\description{
\code{MLEw2p_cpp} is a wrapper function to a fast C++ implementation optimizing parameters of the 2-parameter
 Weibull distribution for a set of data consisting of failures, or alternatively failures and suspensions.
}

\usage{
 MLEw2p_cpp(x, s=NULL, MRRfit=NULL)
}

\arguments{
  \item{x}{A vector of failure data.}
  \item{s}{An optional vector of suspension data.}
  \item{MRRfit}{An optional vector such as produced by MRRw2pxy having parameter order [1] Eta, [2] Beta.
  If not provided, this function will calculate a suitable estimate of Beta to initiate the optimization.}
}

\value{
   A vector containing results in the following order: Eta (scale), Beta (shape), negative Log-Likelihood.
}

\details{
  This function calls a C++ function that performs the root identification of the derivative of the likelihood function
  with respect to Beta, then given the optimal Beta calculate Eta from as the root of the derivative of the 
  likelihood function with respect to Eta.  The optimization algorithm employed is a discrete Newton, or secant, method
  as demonstrated in a FORTRAN program published by Tao Pang.
}

\references{
  Dr. Robert B. Abernethy, (2008) "The New Weibull Handbook, Fifth Edition"
  Tao Pang,(1997) "An Introduction to Computational Physics"
}

\examples{
failures<-c(90,96,30,49,82)
suspensions<-c(100,45,10)
fit_result<-MLEw2p_cpp(failures,suspensions)
}

\keyword{ likelihood }