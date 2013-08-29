\name{CCC2table}
\alias{CCC2table}
\docType{data}
\title{
    Critical Correlation Coefficients squared for Two-Parameter
    Weibull Distributions
}
\description{
    Experimental dataset used by several methods of determining
    goodness-of-fit as described in \dQuote{The New Weibull Handbook},
    Fifth Edition.

    This dataframe is automatically loaded when loading the \pkg{abrem}
    package, and is needed for the function \code{CCC2wb2}.
}
\usage{CCC2table}
\format{
    A dataframe with columns \code{$failures}, \code{$CCC2wb2}, \code{$signif}
    (the number of 'guaranteed' accurate digits) and \code{$S}
    (the number of Monte Carlo simulations on which the CCC2 values are based).
}
\source{
    CCC2 concept: Robert B. Abernethy, Wes Fulton and others.

    This table was created by Jurgen Symynck <jurgen.symynck@kahosl.be>

    and Jacob Ormerod <jtormerod@gmail.com>
}
\section{Warning}{
    This dataset is currently experimental and NOT suitable for usage.
    The number of simulations (\code{S}) is much too low for achieving
    the four significant digits.
}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%    \code{\link{CCC2wb2}}
}

\references{
    Robert B. Abernethy,  \emph{The New Weibull Handbook}, Fifth Edition
    (Robert B. Abernethy, 2004)
}
\keyword{datasets}