\name{RBAw}

\alias{RBAw}

\title{Reduced Bias Adjustment for Weibull MLE} 

\description{
\code{RBAw} generates the reduction factor based on the median bias of the weibull MLE (C4^3.5)
}

\usage{
 RBAw(Nx)
}

\arguments{
  \item{Nx}{The quantity of actual failures}
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
MLE_RBA<-c(MLEfit[1],MLEfit[2]*RBAw(length(failures)),MLEfit[3])
}

\keyword{ likelihood }