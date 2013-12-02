\name{MLEw2p_optim}

\alias{MLEw2p_optim}

\title{Weibull 2-parameter MLE calculation using the default Simplex algorithm in optim} 

\description{
\code{MLEw2p_optim} fits a set of data consisting of failures, or alternatively failures and suspensions,
 to the 2-parameter Weibull distribution.
}

\usage{
 MLEw2p_optim(x, s=NULL, MRRfit=NULL)
}

\arguments{
  \item{x}{A vector of failure data.}
  \item{s}{An optional vector of suspension data.}
  \item{MRRfit}{An optional vector such as produced by MRRw2pxy having parameter order [1] Eta, [2] Beta.
  If not provided, this function will calculate a suitable estimate of Beta to initiate the optimization.}
}

\value{
   A vector containing results in the following order: Eta (scale), Beta (shape), Log-Likelihood)
}

\details{
  This function will sort all input data while preserving failure and suspension information in paired vectors.
  The code is expository as it directly implements the call to optim, just as it is made via packages survival and
  fitdistrplus for the same purpose.  The default optimization method is an implementation of the "Simplex" algorithm
  by Nelder and Mead (1965), which uses only function values and is robust but relatively slow.
}

\references{
    CRAN packages fitdistrplus,survival and stats::optim.Rd
}

\examples{
failures<-c(90,96,30,49,82)
suspensions<-c(100,45,10)
fit_result<-MLEw2p_optim(failures,suspensions)
}

\keyword{ likelihood }