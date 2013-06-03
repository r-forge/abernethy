\name{params.to.ft}
\alias{params.to.ft}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
    Create (un-)censored Testdata From Distribution Parameters}
\description{
    Create a set of (life-)time observations (possibly with censoring),
    perfectly matching a given distribution.
}
\usage{params.to.ft(dist, \ldots)}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{dist}{
        The target distribution for creating the (life-)time observations.
    }
    \item{\ldots}{
        An integer with the total number of (complete) observations, or a
        vector describing the censoring scheme.
        slope and shape parameters of the Weibull distribution for this dataset.
    }
}
\details{}
\value{
    A data frame with the specified censoring scheme, the dataset has the
    chosen distributions parameters.
}
\author{Jurgen Symynck \email{jurgen.symynck@kahosl.be}}
\note{
    Currently, only Weibull distributions fitted with
    \code{method.fit = c("mrr","xony","qbeta")} and \code{dist = "weibull"}
    are supported.
}
\seealso{}
\examples{
### generate two synthetic Weibull datasets ###
### with shape = 3 and scale 1000, resp.    ###
d1 <- params.to.ft("weibull",beta=3,eta=1000,n=10)
d1 <- abrem.fit(Abrem(d1),method.fit=c("mrr","xony","qbeta"))
print(d1$data)
message(paste0(
    "  beta=",d1$fit[[1]]$beta,
    ", eta=",d1$fit[[1]]$eta))
d2 <- params.to.ft("weibull",beta=3,eta=1000,event=c(1,1,1,0,0,0,0,0,0,0))
d2 <- abrem.fit(Abrem(d2),method.fit=c("mrr","xony","qbeta"))
print(d1$data)
message(paste0(
    "  beta=",d2$fit[[1]]$beta,
    ", eta=",d2$fit[[1]]$eta))
}