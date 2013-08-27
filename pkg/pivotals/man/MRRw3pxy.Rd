\name{MRRw3pxy}

\alias{MRRw3pxy}

\title{Weibull 3rd parameter optimization using X on Y median rank regression} 

\description{
\code{MRRw3pxy} optimizes the fit on a set of data consisting of failures, or alternatively failures and suspensions,
 by adjusting for a third, translation, parameter for the Weibull distribution.
}

\usage{
MRRw3pxy(x,s=NULL,limit=10^-5)
}

\arguments{
  \item{x}{A vector of failure data.}
  \item{s}{An optional vector of suspension data.}
  \item{limit}{a convergence limit on the optimization of the third parameter value.}
}

\value{
   A vector containing results in the following order: Eta (scale), Beta (shape), t(0), R^2 (explained variance of regression)
}

\details{
  This function will sort all input data while preserving failure and suspension information in paired vectors.
  Then a call is made to a fast C++ implementation to complete the regression according to the X on Y orientation
  designated as best paractice by Dr. Abernethy.
}

\references{
    Dr. Robert B. Abernethy, (2008) "The New Weibull Handbook, Fifth Edition"
}

\examples{
failures<-c(90,96,30,49,82)
suspensions<-c(100,45,10)
regression_result<-MRRw3pxy(failures,suspensions)
}

\keyword{ regression }