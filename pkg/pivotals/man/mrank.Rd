\name{mrank}
\alias{mrank}

\title{ determination of median ranks }

\description{
This is a wrapper function calling C++ code that executes an algorithm based on Leonard C. Johnson's 
method for establishing adjusted ranks for suspended data in fatige failure tests.  
Benard's approximation is applied to adjusted ranks to produce a vector of median ranks.
}

\usage{
mrank(x,options=NULL) 
}

\arguments{
\item{x}{An ordered event vector consisting of 1's for failures, 0's for suspensions.}
\item{options}{An optional list such as constructed by wiebulltoolkit::wb.options including the
 named item methods.rank for which only a value of "qbeta" has been implemented to alter the otherwise default
 Benard's approximation for determing point estimates for plotting positions.}
}


\value{
Returns a vector of the median ranks.
}

\references{
  Dr. Robert B. Abernethy, (2008) "The New Weibull Handbook, Fifth Edition"
  Leonard C. Johnson (1964) "The Statistical Treatment of Fatique Experiments"  
}

\examples{
median_ranks<-mrank(c(1,0,1,1,0,1,0,0,1,1))
}

\keyword{ median ranks }

