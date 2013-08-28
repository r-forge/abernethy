\name{MLEw2pContour}

\alias{MLEw2pContour}

\title{Likelihood Contour for Weibull} 

\description{
\code{MLEw2pContour} This function generates points for a display of the likelihood contour at given confidence limit			
  for the 2-parameter Weibull distribution.
}

\usage{
MLEw2pContour(x, s=NULL, CL=0.9, DF=1,debias=FALSE,show=FALSE)
}

\arguments{
  \item{x}{A vector of failure data.}
  \item{s}{An optional vector of suspension data.}
  \item{CL}{a two-sided confidence limit.}
  \item{DF}{an integer value indicating degrees of freedom to apply to the Chi square test, which defaults to DF=1 for
  confidence bound use. Should be set to 2 for comparison of two models each with 2 parameters.}
  \item{debias}{a logical value indicating whether the RBA and FF adjustments should be applied to output.}
  \item{show}{a logical value indicating whether a graphical output is desired (independent of abrem activity).} 
}

\value{
   A list containing 4 vectors of plotting points for top, bottom, and sides of the contour. 
}

\details{
   The contour points (p1,p2) identified as satisfying the root of the equation,
   (log(ML(p1_hat,p2_hat))-log(RL(p1,p2))*FF - chisquare(CL,DF)/2=0
   ,where ML is Maximum Likelihood for the data,and RL is Ratioed Likelihood for the data at selected points
   for the contour.  Depending on the value of debias, the contour points  are then modified by the RBA (median basis)
   on the Wiebull shape parameter, Beta.
}

\references{
    Dr. Robert B. Abernethy, (2008) "The New Weibull Handbook, Fifth Edition"
	Tao Pang,(1997) "An Introduction to Computational Physics"
}

\examples{
fig3cF<-c(1500,2250,4000,4300,7000)
fig3cS<-c(1750,5000)
Contour<-MLEw2pContour(fig3cF,fig3cS,debias=TRUE)
}

\keyword{ likelihood }