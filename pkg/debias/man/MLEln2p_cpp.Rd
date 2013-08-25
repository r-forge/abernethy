\name{MLEln2p_cpp}

\alias{MLEln2p_cpp}

\title{Lognormal 2-parameter MLE calculation.} 

\description{
\code{MLEln2p_cpp} is a wrapper function to a fast C++ implementation optimizing parameters of the 2-parameter
 lognormal distribution for a set of data consisting of failures, or alternatively failures and suspensions.
}

\usage{
 MLEln2p_cpp(x, s=NULL, MRRfit=NULL)
}

\arguments{
  \item{x}{A vector of failure data.}
  \item{s}{An optional vector of suspension data.}
  \item{MRRfit}{An optional vector such as produced by MRRln2pxy or MRRln2pyx having parameter order [1] Mulog, [2] Sigmalog.
  If not provided, this function will calculate a suitable estimate of the parameters to initiate the optimization.}
}

\value{
   A vector containing results in the following order: Mulog, Sigmalog, Log-Likelihood.
}

\details{
  This function calls a C++ function that performs the Nelder-Meade simplex optimization of the negative log-likelihood
  function.  The code is streamlined for the two-parameter, only, case.
}

\references{
  Mike Hutt,a GNU implementation in C  http://www.mikehutt.com/neldermead.html
  Sasa Singer and John Nelder (2009) Nelder-Mead algorithm. Scholarpedia, 4(7):2928.
  Botao Jia, Simplex-Optimization-Algorithm-and-Implemetation-in-C-Programming.htm, codeguru.com/cpp/article.php/c17505/
}

\examples{
failures<-c(90,96,30,49,82)
suspensions<-c(100,45,10)
fit_result<-MLEln2p_cpp(failures,suspensions)
}

\keyword{ likelihood }