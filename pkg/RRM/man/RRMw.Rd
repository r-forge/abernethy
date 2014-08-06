\name{RRMw}

\alias{RRMw}

\title{ReliaSoft Ranking Method} 

\description{
\code{RRMw} implements a likelihood based placement of specific fail time and rank position for life-data sets including interval-censored data
}

\usage{
 RRMw(x=NULL, s=NULL, interval=NULL) 
}

\arguments{
  \item{x}{A vector of known failure times}
  \item{s}{A vector of last life-times known for right-censored, suspension, data.}
  \item{interval}{A dataframe consisting of "left" and "right" times (using those column names) bounding an interval in which failure must have occurred.
  Discoveries, or left-censored data, are entered with any non-positive numeric value (including NA) in the "left" column of the dataframe.}
}

\value{
   A list of named dataframes: "ppp", "trials", and "increments".  The ppp dataframe is consistent with the required for for the x arguement of the key regression function lslr.
    The other objects in this list provide detail on intermediate steps of the method.
}

\details{
  This function is provided as a test for eventual inclusion in package abremPivotals.  A later version of this implementation is expected to be called internally from getPPP when interval data has been provided.
}

\references{
"Appendix: Special Analysis Methods." ReliaWiki RSS. N.p., n.d. Web. 06 Aug. 2014. <http://reliawiki.org/index.php/Appendix%3A_Special_Analysis_Methods#ReliaSoft_Ranking_Method>.
}

\examples{
failures<-c(90,96,30,49,82)
suspensions<-c(100,45,10)
intervals<-data.frame(left=c(rep(0,3),rep(20,2), rep(50,3)),
 right=c(50,55,60,rep(40,2),rep(70,2),85))
pppDF<-RRMw(failures,suspensions,intervals)[[1]]
}

\keyword{ likelihood }