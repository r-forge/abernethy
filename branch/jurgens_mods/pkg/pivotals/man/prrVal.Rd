\name{prrVal}
\alias{prrVal}

\title{ P-value determination by pivotal MC with CCC^2 report }

\description{
This is a wrapper function calling C++ code that executes a pivotal analysis.  The P-Value (prr) is the percentile
at which the provided Variation (Rsqr) exists in a large random sampling of the model distribution based on
pivotal parameters.  The 90% Critical Coefficient is the Rsqr value at the 10th percentile of the samples.
}

\usage{
prrVal(x, Rsqr, S = 10^4, Eta=1.0, Beta=1.0, model = "w2", ProgRpt = FALSE) 
}

\arguments{
\item{x}{The quantity of complete failures for evaluation, or an event vector}
\item{Rsqr}{The variance (R squared) from a linear regression of a data sample of interest having size=x or event vector represented by x.}
\item{S}{The number of random samples to be drawn for Monte Carlo simulation. S must be a multiple 
of 10, not less than 1,000.  The default of 10^4 is adequate for most instances.}
\item{Eta}{The Eta parameter to be used in random sampling.  Default = 1.0}
\item{Beta}{The Beta parameter to be used in random sampling. Default = 1.0}
\item{model}{A character string representing the model of interest.  The default value of "w2" for 
2-parameter Weibull is the only model currently valid.} 
\item{ProgRpt}{A boolean value to control the generation of percent completion feedback in the R terminal.}
}


\value{
Returns a dataframe holding the P-value and 90% Critical Correlatiob Coefficient Squared, CCC^2
}

\references{
  Dr. Robert B. Abernethy, (2008) "The New Weibull Handbook, Fifth Edition"
}

\examples{
GoodnessOfFit<-prrVal(12,0.9745088)
}

\keyword{ Goodness of Fit }

