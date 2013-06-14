\name{abremObj}

\alias{abremObj}

\title{abrem object maker for pivotals package testing } 

\description{
\code{abremObj} Prepares a set of data consisting of failures, or alternatively failures and suspensions,
 as an abrem class object for use with pivotals MRR functions.
}

\usage{
 abremObj(x, s=NULL)
}

\arguments{
  \item{x}{A vector of failure time data.}
  \item{s}{An optional vector of suspension time data.}
}

\value{
   A list object of class abrem
}

\details{
  This function will sort all input data while preserving failure and suspension information in a paired
  event vector. The vectors will be available as list items $data$time and $data$event.
  It is expected that this function will be depreciated in favor of a final abrem::Abrem function.
}

\references{
    package abrem
}

\examples{
failures<-c(90,96,30,49,82)
suspensions<-c(100,45,10)
MRRarg<-abremObj(failures,suspensions)
}

\keyword{ class }