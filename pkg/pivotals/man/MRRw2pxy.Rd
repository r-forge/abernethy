\name{MRRw2pxy}

\alias{MRRw2pxy}

\title{X on Y median rank regression for Weibull 2-parameter } 

\description{
\code{MRRw2pxy} fits a set of data consisting of failures, or alternatively failures and suspensions,
 to the 2-parameter Weibull distribution.
}

\usage{
MRRw2pxy(x,s=NULL)
}

\arguments{
  \item{x}{A vector of failure data.}
  \item{s}{An optional vector of suspension data.}
}

\value{
   A vector containing results in the following order: Eta (scale), Beta (shape), R^2 (regression variance)
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
regression_result<-MRRw2pxy(failures,suspensions)
}

\keyword{ regression }