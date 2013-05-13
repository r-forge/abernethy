\name{prrVec}
\alias{prrVec}

\title{ 
Percentile of r and R^2 by pivotal MC, returning a complete vector of CDF }

\description{
This is a wrapper function calling C++ code that executes a pivotal analysis.  The P-Value (prr) is the percentile
at which a given Variation (Rsqr) exists in a large random sampling of the model distribution based on
pivotal parameters.  The entire vector of Variation results is returned in a sorted fashion.
}

\usage{
prrVec(x, S=10^4, model="w2", seed=1234, options=NULL, ProgRpt=FALSE)
}

\arguments{
\item{x}{The quantity of complete failures for evaluation, or an event vector}
\item{S}{The number of random samples to be drawn for Monte Carlo simulation. S must be a multiple 
of 10, not less than 1,000.  The default of 10^4 is adequate for most instances.}
\item{model}{A character string representing the model of interest.  The default value of "w2" is for the
2-parameter Weibull. "ln2" or "n" for 2-parameter lognormal or normal, executed by the same code.} 
\item{seed}{an integer used to set the RNG seed.  Default = 1234}
\item{options}{An optional list such as constructed by wiebulltoolkit::wb.options including the
 named item methods.rank for which only a value of "qbeta" has been implemented to alter the otherwise default
 Benard's approximation for determing point estimates for plotting positions.}
\item{ProgRpt}{A boolean value to control the generation of percent completion feedback in the R terminal.}
}


\value{
Returns a vector of the entire CDF of Variation (r^2) from pivotal analysis.
}

\references{
  Dr. Robert B. Abernethy, (2008) "The New Weibull Handbook, Fifth Edition"
}

\examples{
CDF<-prrVec(12)
}

\keyword{ Goodness of Fit }

