\name{CCC2table}
\alias{CCC2table}
\docType{data}
\title{Critical Correlation Coefficients squared for two-parameter Weibull distributions}
\description{Experimental dataset used by several methods of determining goodness-of-fit as described in \dQuote{The New Weibull Handbook}, Fifth Edition.}
\usage{CCC2table}
\format{A dataframe with columns \code{$failures}, \code{$CCC2wb2}, \code{$signif} (the number of 'guaranteed' accurate digits) and \code{$S} (the number of Monte Carlo simulations on which the CCC2 values are based.}
\source{
CCC2 concept: Robert B. Abernethy, Wes Fulton and others.
Table was created by Jurgen Symynck <jurgen.symynck @nospam@ kahosl.be> and Jacob Ormerod <jtormerod @nospam@ gmail.com>
}
\section{Warning}{
This dataset is currently experimental and NOT suitable for usage. The number of simulations (\code{S}) is likely much too low for achieving the four significant digits.}

\references{
Robert B. Abernethy,  \emph{The New Weibull Handbook, Fifth Edition} (Robert B. Abernethy, 2004)
}
\keyword{datasets}