\name{MLEw2p_abrem}

\alias{MLEw2p_abrem}

\title{Weibull 2-parameter MLE calculation using optim} 

\description{
\code{MLEw2p_abrem} fits a set of data consisting of failures, or alternatively failures and suspensions,
 to the 2-parameter Weibull distribution.
}

\usage{
 MLEw2p_abrem(x, s=NULL, MRRfit=NULL, limit=1.0e-6, listout=FALSE)
}

\arguments{
  \item{x}{A vector of failure data.}
  \item{s}{An optional vector of suspension data.}
  \item{MRRfit}{An optional vector such as produced by MRRw2pxy having parameter order [1] Eta, [2] Beta.
  If not provided, this function will calculate a suitable estimate of Beta to initiate the optimization.}
  \item{limit}{A limiting value for the optimization.  Default 1.0e-6 should serve most purposes.}
  \item{listout}{A boolean flag for permitting an alternate output as a list containing the primary result vector
  and a dataframe providing details of the optimization search.}
}

\value{
   A vector containing results in the following order: Eta (scale), Beta (shape), negative Log-Likelihood).  This
   vector is returned by default and also as list item [[1]] depending on the listout argument.  The second optional
   list item is a dataframe providing details of the optimization search.
}

\details{
  This function will sort all input data while preserving failure and suspension information in paired vectors.
  The code is expository as it implements the root identification of the derivative of the likelihood function
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
fit_result<-MLEw2p_abrem(failures,suspensions)
}

\keyword{ likelihood }