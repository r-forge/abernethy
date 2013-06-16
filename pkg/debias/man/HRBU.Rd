\name{HRBU}

\alias{HRBU}

\title{Unbias factors for Weibull MLE} 

\description{
\code{HRBu} generates the reduction factor based on the mean bias of the weibull MLE (roughly C4^6) for complete failure
 samples, increased correction as suspensions increase.
}

\usage{
 HRBU(Nx, Ns=NULL)
}

\arguments{
  \item{Nx}{The quantity of actual failures}
  \item{Ns}{An optional quantity of suspensions}
}

\value{
   A factor to be multiplied to the MLE Beta for bias reduction.
}

\details{
  This as many references discuss the bias reduction in terms of mean reduction.  
  Abernethy's RBA addresses median bias reduction.
}

\references{
  ReliaSoft Corporation, Life Data Analysis Reference, Tucson, AZ: ReliaSoft Publishing, 2005
}

\examples{
failures<-c(90,96,30,49,82)
suspensions<-c(100,45,10)
MLEfit<-MLEw2p_abrem(failures,suspensions)
MLE_Unbiased<-c(MLEfit[1],MLEfit[2]*HRBU(length(failures),length(suspensions)),MLEfit[3])
}

\keyword{ likelihood }