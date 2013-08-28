\name{RBAbeta}

\alias{RBAbeta}

\title{Reduced Bias Adjustment for Weibull MLE} 

\description{
\code{RBAbeta} generates the reduction factor based on the median bias of the weibull MLE (C4^3.5)
 alternatively a reduction factor based on the mean bias can be returned (C$^6)
}

\usage{
 RBAbeta(Nx, basis="median")
}

\arguments{
  \item{Nx}{The quantity of actual failures}
  \item{basis}{a string value indicating the basis for bias reduction, "median" is the default, but "mean" 
  may be chosen.}
}

\value{
   A factor to be multiplied to the MLE Beta for bias reduction.
}

\details{
  Many references discuss the bias reduction in terms of mean reduction.  The RBA addresses median bias reduction.
}

\references{
  Dr. Robert B. Abernethy, (2008) "The New Weibull Handbook, Fifth Edition"
}

\examples{
failures<-c(90,96,30,49,82)
suspensions<-c(100,45,10)
MLEfit<-MLEw2p_abrem(failures,suspensions)
MLE_RBA<-c(MLEfit[1],MLEfit[2]*RBAbeta(length(failures)),MLEfit[3])
}

\keyword{ likelihood }