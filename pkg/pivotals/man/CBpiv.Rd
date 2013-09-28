\name{CBpiv}
\alias{CBpiv}

\title{ P-value determination by pivotal MC with CCC^2 report }

\description{
This is a wrapper function calling C++ code that executes a pivotal analysis to deliver double sided confidence
bounds at the B-value quantiles for the model distribution.  The bounds are presented as log values for X axis plotting
suitable for transformation to a specific linear fit to data of the same size.  The pivotal points are expected 
to be the basis for a curve generation upon ultimate display.
}

\usage{
CBpiv(x, CI, S = 10^4, Bval=mrank(rep(1,15)), Eta=1.0, Beta=1.0, model = "w2", seed=1234, ProgRpt = FALSE) 
}

\arguments{
\item{x}{The quantity of complete failures for evaluation, or an event vector}
\item{CI}{The double sided confidence interval of interest. }
\item{S}{The number of random samples to be drawn for Monte Carlo simulation. S must be a multiple 
of 10, not less than 1,000.  The default of 10^4 is adequate for most instances. S is implemented as an
unsigned int in C++ code.  The maximum limit is 4x10^9 if system memory permits.}
\item{Bval}{A vector of B-values at which to determine the confidence bounds.}
\item{Eta}{The Eta parameter to be used in random sampling.  Default = 1.0}
\item{Beta}{The Beta parameter to be used in random sampling. Default = 1.0}
\item{model}{A character string representing the model of interest.  The default value of "w2" for 
2-parameter Weibull is the only model currently valid.} 
\item{seed}{an integer used to set the RNG seed.  Default = 1234}
\item{ProgRpt}{A boolean value to control the generation of percent completion feedback in the R terminal.}
}


\value{
Returns a dataframe holding the Lower bound, the Median, and the Upper bound according to the sequence of B-values provided.
}

\references{
  Dr. Robert B. Abernethy, (2008) "The New Weibull Handbook, Fifth Edition"
  
}

\examples{
bounds<-CBpiv(10,0.9)
}

\keyword{ interval confidence }

