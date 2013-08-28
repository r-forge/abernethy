\name{RBAsigma}

\alias{RBAsigma}

\title{Reduced Bias Adjustment for Weibull MLE} 

\description{
\code{RBAsigma} generates the reduction factor based on the median bias of the weibull MLE (C4^3.5)
}

\usage{
 RBAsigma(Nx)
}

\arguments{
  \item{Nx}{The quantity of actual failures}
}

\value{
   A factor to be multiplied to the MLE Sigma (or Sigmalog) for bias reduction.
}

\details{
  This adjustment is based on well-established theory for statistical quality control.
}

\references{
  Dr. Robert B. Abernethy, (2008) "The New Weibull Handbook, Fifth Edition"
}

\examples{
failures<-c(90,96,30,49,82)
suspensions<-c(100,45,10)
MLEfit<-MLEln2p_cpp(failures,suspensions)
MLE_RBA<-c(MLEfit[1],MLEfit[2]*RBAsigma(length(failures)),MLEfit[3])
}

\keyword{ likelihood }