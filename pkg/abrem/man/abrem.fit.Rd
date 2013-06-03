\name{abrem.fit}
\alias{abrem.fit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
    Fit Distributions to \code{abrem} Objects
}
\description{
   The \code{abrem.fit} function fits distributions to \code{abrem} objects.
}
\usage{abrem.fit(x,\dots)}
\arguments{
    \item{x}{
        Object of class \code{abrem}.
    }
    \item{\dots}{
        Options for fitting the (life-)time observations,
        and for plotting the results.
    }
}
\details{
    This function calculates fits for the (life-)time observations in the
    \code{abrem} object and adds them to the object alongside any
    pre-existing fits.

    Fitting options are supplied with the \code{"dist"} and \code{"method.fit"}
    arguments; see \code{\link[abrem]{options.abrem}} for more details.

    One can also add any parameters available from \code{options.abremplot},
    such as \code{"col"} or \code{"is.plot.legend"}. These plotting options
    will be used when plotting the fit using \code{plot.abrem}.
    %\code{\link[abremout:plot.abrem]{plot.abrem}}.
}
\value{
    \code{abrem.fit()} returns its argument \code{x}, extended with the
    calculated fit and the optional graphical and calculation arguments as
    provided to the function, enabling the extension of \code{abrem} object
    with additional calculation results.
}
\author{Jurgen Symynck \email{jurgen.symynck@kahosl.be}}
\note{
    The \code{$time} column of the \code{abrem} object can have \code{NA}
    values when accompanied with the 'censored' indicator in the \code{$event}
    column (= \code{0}). While this poses no problem for \code{abrem.fit} when
    using Median Rank Regression, it results in different fits when using the
    Maximum Likelihood Estimantion method of \code{\link[survival]{survreg}}.
}
\section{To Do}{
    \itemize{
        \item Research the effect of \code{NA} values in the (life-)time
            observations when not using Median Rank Regression.
        \item Solve inappropriate list item naming.
    }
}
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
    \code{\link{options.abrem}}, \code{\link{Abrem}}
}
\examples{
da <- Abrem(time=c(500,510,900,1200,1300))
da <- abrem.fit(da,dist="weibull", method.fit=c("mrr","xony","exact"),pch=3)
da <- abrem.fit(da,dist="weibull", method.fit=c("mrr","xony","bernard"),
    col="red",pch=4,lty=2)
if(require(survival)){
    da <- abrem.fit(da,dist="weibull",method.fit="surv", col="blue",pch="s")
    da <- abrem.fit(da,dist="lognormal",method.fit="surv", col="green3",pch=8)
    message("Comparison between MLE and MRR:")
    message(paste0(
        "  MLE: beta=",da$fit[[3]]$beta,
        ", eta=",da$fit[[3]]$eta))
}
message(paste0(
    "  MRR: beta=",da$fit[[1]]$beta,
    ", eta=",da$fit[[1]]$eta))
}
