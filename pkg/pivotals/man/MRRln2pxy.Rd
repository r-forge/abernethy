\name{MRRln2pxy}

\alias{MRRln2pxy}

\title{X on Y median rank regression for Lognormal 2-parameter } 

\description{
\code{MRRw2pxy} fits a set of data consisting of failures, or alternatively failures and suspensions,
 to the 2-parameter Lognormal distribution.
}

\usage{
MRRln2pxy(x,s=NULL)
}

\arguments{
  \item{x}{A vector of failure data.}
  \item{s}{An optional vector of suspension data.}
}

\value{
   A vector containing results in the following order: Mulog, Sigmalog, R^2 (regression variance)
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
regression_result<-MRRln2pxy(failures,suspensions)
}

\keyword{ regression }