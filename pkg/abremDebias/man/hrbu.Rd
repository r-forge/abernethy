\name{hrbu}

\alias{hrbu}

\title{Hirose and Ross beta unbias factors for Weibull MLE} 

\description{
\code{hrbu} generates the reduction factor based on the mean bias of the weibull MLE beta parameter(roughly C4^6) for complete failure
 samples, modestly increased correction as number of suspensions increases.
}

\usage{
 hrbu(Qx, Qs=NULL)
}

\arguments{
  \item{Qx}{The quantity of actual failures}
  \item{Qs}{An optional quantity of suspensions}
}

\value{
   A factor to be multiplied to the MLE Beta for bias reduction.
}

\details{
  This as many references discuss the bias reduction in terms of mean.  
  (Dr. Abernethy makes a strong recommendation for use of the median bias reduction.)
}

\references{
  ReliaSoft Corporation, Life Data Analysis Reference, Tucson, AZ: ReliaSoft Publishing, 2005
}

\examples{
failures<-c(90,96,30,49,82)
suspensions<-c(100,45,10)
MLEfit<-mlefit(mleframe(failures,suspensions))
MLE_Unbiased<-c(MLEfit[1],MLEfit[2]*hrbu(length(failures),length(suspensions)))
}

\keyword{ likelihood bias}