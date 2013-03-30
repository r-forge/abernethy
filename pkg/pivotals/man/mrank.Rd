\name{mrank}
\alias{mrank}

\title{ determination of median ranks }

\description{
This is a wrapper function calling C++ code that executes an algorithm based on Leonard C. Johnson's 
method for establishing adjusted ranks for suspended data in fatige failure tests.  
Benard's approximation is applied to adjusted ranks to produce a vector of median ranks.
}

\usage{
mrank(x) 
}

\arguments{
\item{x}{The quantity of complete failures for evaluation, or an event vector}
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

