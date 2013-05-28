\name{abrem.fit}
\alias{abrem.fit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Fit Distributions To \code{abrem} Objects}
\description{
   The  \code{abrem.fit()} function fits distributions to \code{abrem} objects.}
\usage{abrem.fit(x,\dots)}
\arguments{
    \item{x}{Object of class \code{abrem}.}
    \item{...}{Options for fitting and for plotting the results; see details.}
}
\details{
%\section{Fit options}{
    This function calculates fits for the dataset provided in the abrem object and add it to the object. In addition of the abrem object, other arguments can be supplied
        \describe{
            \item{\code{dist}}{A character string with the distribution target for fitting. Possible values are \code{"weibull"} and \code{"lognormal"}. Defaults to \code{"weibull"}.}
            \item{\code{method}}{A vector of character strings describing settings for fitting. Possible values are \code{"mrr"} for Median Rank Regression, \code{"surv"} for Maximum Likelihood Estimation (MLE) by means of \code{\link[survival]{survreg}}, \code{"qbeta"} for exact ranking, \code{"bernard"} for Bernards approximation to exact ranking, \code{"xony"} and \code{"yonx"} for X-on-Y regression or Y-on-X regression, respectively. Note that ALL relevant settings must be supplied for the fitting to succeed, if one supplies \code{"bernard"} then one must supply \code{"mrr"} and \code{"xony"} also. Defaults to \code{c("mrr","xony","qbeta")}. Also, only sensible combinations of method arguments will result in a fit. For instance, \code{"surv"} and \code{"xony"} do not match together because MLE uses no rank regression.}
        }
        One can also add any parameters available from \code{options.abremplot}, like \code{"col"} or \code{"is.plot.legend"}. These plotting options will be used when plotting the fit using \code{plot.abrem}.%\code{\link[abremout:plot.abrem]{plot.abrem}}.
}
\value{
   \code{abrem.fit()} returns its argument \code{x}, extended with the calculated fit and the optional graphical and calculation arguments as profided to the function.}
   
\author{Jurgen Symynck \email{jurgen.symynck@kahosl.be}}
\note{}
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
    \code{\link{options.abrem}}
    \code{\link{Abrem}}
}
\examples{
    da <- Abrem(time=c(500,510,900,1200,1300))
    da <- abrem.fit(da,dist="weibull", method=c("mrr","xony","exact"),pch=3)
    da <- abrem.fit(da,dist="weibull", method=c("mrr","xony","bernard"),
        col="red",pch=4,lty=2)
    if(require(survival)){
        da <- abrem.fit(da,dist="weibull",method="surv", col="blue",pch="s")
        da <- abrem.fit(da,dist="lognormal",method="surv", col="green3",pch=8)
        message("Comparison between MLE and MRR:")
        message(paste0(
            "  MLE: beta=",da$fit[[3]]$beta,
            ", eta=",da$fit[[3]]$eta))
    }
    message(paste0(
        "  MRR: beta=",da$fit[[1]]$beta,
        ", eta=",da$fit[[1]]$eta))
}
