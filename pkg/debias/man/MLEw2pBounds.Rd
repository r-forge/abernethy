\name{MLEw2pBounds}

\alias{MLEw2pBounds}

\title{Likelihood Ratio Bounds for Weibull} 

\description{
\code{MLEw2pBounds} This function generates points for a display of the likelihood ratio bounds at given two-sided
 confidence limit for the 2-parameter Weibull distribution.
}

\usage{
MLEw2pBounds(x, s=NULL, CL=0.9, Blives=(c(1,5,10,20,30,40,50,60,80,90,95,99)/100),  MLEcontour=NULL, debias=FALSE,show=FALSE) 
}

\arguments{
  \item{x}{A vector of failure data.}
  \item{s}{An optional vector of suspension data.}
  \item{CL}{a two-sided confidence limit.}
  \item{Blives}{a vector of quantiles for which the corresponding log(Eta) values will be calculated to represent the bounds.}
  \item{MLEcontour}{an optional argument to hold the output of a preceding call to MLEw2pContour on the same event data.
   (The MLEcontour input is not checked in any way.)}
  \item{debias}{a logical value indicating whether the RBA and FF adjustments should be applied to the call to MLEw2pContour
   from this function.  (This is of no effect if the MLEcontour is provided as an input argument.)}
  \item{show}{a logical value indicating whether a graphical output is desired (independent of abrem activity).} 
}

\value{
   A dataframe containing the plotting point information for the Lower, Datum, and Upper bound log(Eta) values at
	the y-axis positions of the Blives as input.
}

\details{
   The confidence bounds are constructed on the Weibull plot from a type of pivotal analysis of the points on the likelihood ratio contour.
}

\references{
    Dr. Robert B. Abernethy, (2008) "The New Weibull Handbook, Fifth Edition"
}

\examples{
fig3cF<-c(1500,2250,4000,4300,7000)
fig3cS<-c(1750,5000)
Bounds<-MLEw2pBounds(fig3cF,fig3cS,debias=TRUE)
}

\keyword{ likelihood }