\name{MLEw3p_secant}

\alias{MLEw3p_secant}

\title{Weibull 3rd parameter fitting by optimization of the Maximum Likelihood} 

\description{
\code{MLEw3p_secant} optimizes the fit on a set of data consisting of failures, or alternatively failures and suspensions,
 by adjusting for a third, translation, parameter for the Weibull distribution.
}

\usage{
MLEw3p_secant(x,s=NULL,limit=10^-5,listout=FALSE)
}

\arguments{
  \item{x}{A vector of failure data.}
  \item{s}{An optional vector of suspension data.}
  \item{limit}{a convergence limit on the optimization of the third parameter value.}
  \item{listout}{a logical value indicating whether output should be a list including the default output vector
  and a dataframe listing the progression of the optimization routine.}
}

\value{
   A vector containing results in the following order: Eta (scale), Beta (shape), t(0), R^2 (explained variance of regression)
   Alternatively a list including the default output vector and a dataframe listing the progression of the optimization routine.  
}

\details{
   A discrete Newton method, also called the secant method is used to identify the root of the derivative of the MLE~t0 function.
   In this case the derivative is numerically determined by a two point method.  The 3-parameter Weibull MLE optimization
   is known to present instability with some data.  This is a particular challenge that this routine has been designed to handle.
   It is expected that when instability is encountered the function will terminate gracefully at the point at which MLE fitting
   calculations fail, providing output of last successful calculation.
}

\references{
    Dr. Robert B. Abernethy, (2008) "The New Weibull Handbook, Fifth Edition"
	Tao Pang,(1997) "An Introduction to Computational Physics"
}

\examples{
failures<-c(90,96,30,49,82)
suspensions<-c(100,45,10)
fit_result<-MLEw3p_secant(failures,suspensions)
}

\keyword{ likelihood }