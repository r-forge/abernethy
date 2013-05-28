\name{options.abrem}
\alias{options.abrem}
\alias{mrr}
\alias{bernard}
\alias{xony}
\alias{yonx}
\alias{prcomp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Abrem Calculation Options}
\description{
   The  \code{options.abrem()} function handles the options for the various calculations performed by the \pkg{abrem} package.}
\usage{options.abrem(\dots)}
\arguments{
    \item{...}{Options for the various calculations performed by the \pkg{abrem} package.}
}

\details{
   The  \code{options.abrem()} function borrows its internal structure from the \code{\link[graphics]{par}} function of package \pkg{graphics}. It can be used in different ways:
   
   \code{options.abrem()}
   
   Returns the currently used options and there values.

   \code{options.abrem()$dist}\cr\code{options.abrem("dist")}
   
   Returns the current value of an option.
   
   \code{options.abrem(CL=0.95)}\cr\code{options.wb(list(CL=95,R=5e4))}
   
   Sets the specified options.
   Currently, there is no way to reset the options to the default values using this function.  One might, before changing any options, store the option list in a temporary variable like  \code{abrem.default <- options.abrem()} for restoring it later by running  \code{options.abrem(abrem.defaults)}.}
\value{
   \code{options.abrem()} returns a list containing the current options of the \pkg{abrem} package.  Also, it creates a globally accessible list named  \code{options_abrem}, holding the options.  One should always use the  \code{options.abrem()} function to access the option list, do not access the list directly.}
\section{Abrem options}{
    \describe{
        \item{\code{dist}}{The target distribution for \code{abrem.fit()}. Currently, \code{"weibull"} and \code{"lognormal"} are supported.}
%        \item{\code{method.reg}}{The fit method to be used. Currently, \code{"mrr"} for Median Rank Regression and \code{"surv"} for Maximum Likelihood Estimation (MLE) using the \code{survreg()} function are supported.}
%        \item{\code{method.rank}}{When \code{method.reg = "mrr"}, the type of rank regression. Currently, \code{"qbeta"} for exact rank regression using the incomplete Beta function, and \code{"bernard"} for Bernards Approximation to the exact method are supported.}
        \item{\code{S}}{
        An integer describing the number of Monte Carlo simulations on which the Monte Carlo pivotal confidence bounds are based.  The default value is  \code{1e4}. High values are needed for good confidence bounds at the lower end of the Weibull model, especially for data with heavy censoring.}
        \item{\code{cl}}{
        Confidence Level: A number from the interval \code{[0,[1} determining the width of the calculated confidence intervals.}
        \item{\code{cb.points}}{
        An integer controlling the amount of unreliability levels for which confidence bounds are calculated.}
         \item{\code{pivotals}}{For debugging purposes, do not use.}
        \item{\code{blives}}{A numeric vector with some B-lives for which conficence will be calculated.}
        \item{\code{sides}}{Either \code{"lower"},  \code{"upper"} or \code{"double"}, specifying the type of bound(s) to be calculated.}
    }
}
\author{Jurgen Symynck \email{jurgen.symynck@kahosl.be}}
\note{}
\section{To Do}{
  \itemize{ 
      \item Add the possibility to reset to the default values.}}
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{options.abremplot}}
\code{\link{abrem.fit}}
}
\examples{
abrem.defaults <- options.abrem()
options.abrem(R=5e5,CL=0.99)
options.abrem()
options.abrem(abrem.defaults)
}
