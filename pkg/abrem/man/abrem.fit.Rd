\name{abrem.fit}
\alias{abrem.fit}
\alias{dist}
\alias{method.fit}
\alias{prr}
\alias{P-value}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
    Fit Distributions to \code{Abrem} Objects
}
\description{
   This function fits probability distributions to \code{Abrem} objects.
}
\usage{abrem.fit(x,\dots)}
\arguments{
    \item{x}{
        Object of class \code{"abrem"}.
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

    Fitting options are passed with the \code{dist} and \code{method.fit}
    arguments:
    
    \describe{
        \item{\code{dist}}{
            A character string with the target distribution for fitting.
            Possible values are \code{"weibull"} or \code{"weibull2p"},
            \code{"weibull3p"} (three parameter Weibull), \code{"lognormal"}
            or \code{"lognormal2p"}.


            Currently, some unsupported values can be passed:
            \code{"weibull-2"},
            \code{"weibull2p-2"},
            \code{"lognormal-2"} and
            \code{"lognormal2p-2"}. These differentiate between R-based
            and C++ based implementations of the rank regression code. Check
            the code of function \code{\link{calculateSingleFit}} for implementation details.
            
            Defaults to \code{"weibull"}.
        }
        \item{\code{method.fit}}{
            A vector of class \code{"character"} with fitting options.
            
            Defaults to \code{c("rr","xony")}.
            
            
            \itemize{
                \item \code{"rr"}: Rank Regression (RR). Depending on the method for
                calculating probability plot positions chosen during the creation of the
                \code{Abrem} object (see option \code{\link{pp}} and
                function \code{\link{Abrem}}), this can
                either be "exact median rank regression" or
                "Bernard's approximate median rank regression".
                If this method is used then it is mandatory to additionally specify
                either X-on-Y or Y-on-X regression.

                \item \code{"xony"},\code{"yonx"}: Differentiate between X-on-Y and Y-on-X
                regression, respectively. For rank regression in lifetime analysis,
                it is best practice to use the X values ((life-)time observations)
                as the response variables whose horizontal distance to the fit line
                must be minimized, and the Y values (unreliabilities) as the
                explanatory variable.

                \item \code{"mle"}: Maximum Likelihood Estimation (MLE), currently using
                function \code{\link[debias]{MLEw2p_abrem}} of the \pkg{debias} package.
                %%Currently, MLE is only possible for two-parameter Weibull distributions.

                \item \code{"mle-rba"}: Maximum Likelihood Estimation with Reduced Bias
                based on the median bias of the weibull MLE.
                This options uses function \code{\link[debias]{RBAbeta}} of the
                \pkg{debias} package, and is therefore limited to
                two-parameter Weibull distributions at the moment. In the future,
                choosing between different implementations of the MLE code and
                support for other distributions (already available in package
                \pkg{debias}) will be possible.
            }
        }

        Additionally, one can pass any options available from \code{options.abrem},
        such as \code{col} or \code{is.plot.legend}. The graphical options
        will be used when plotting the (life-)time observations using \code{plot.abrem}.
        Subsequent calls to \code{abrem.conf} will inherit these options.

        The "prr" goodness-of-fit indicator is also calculated
        here. See the "Examples" section on how to retrieve it, see the "References"
        section for additional information on the prr and pve value.
        % \code{\link[abremout:plot.abrem]{plot.abrem}}.
    }
}
\value{
    The function returns its argument \code{x}, extended with the
    calculated fit and the optional graphical and calculation arguments as
    provided to the function.
}
\author{Jurgen Symynck \email{jurgen.symynck@kahosl.be}}
%\note{
%    The \code{$time} column of the \code{abrem} object can have \code{NA}
%    values when accompanied with the 'censored' indicator in the \code{$event}
%    column (= \code{0}). While this poses no problem for \code{abrem.fit} when
%    using Median Rank Regression, it results in different fits when using the
%    Maximum Likelihood Estimantion method of \code{\link[survival]{survreg}}.
%}
\section{To Do}{
    \itemize{
        \item Research the effect of \code{NA} values in the (life-)time
            observations of the \code{abrem} argument when not using rank regression.
    }
}
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
    \code{\link{options.abrem}}, \code{\link{Abrem}}
}
\references{
    \itemize{
        \item
            \emph{Improved Goodness of Fit:
                P-value of the Correlation Coefficient}, Wes Fulton
                
            \url{http://www.barringer1.com/jun05prb.htm}
    }
}
\examples{
da1 <- Abrem(runif(5,100,2000))
da1 <- abrem.fit(da1,dist="weibull",method.fit=c("rr","xony"),pch=3)

prr <- da1$fit[[1]]$gof$prr
message("prr goodness-of-fit of first fit:")
message(paste0(prr,ifelse(prr >= 10," -> Good fit."," -> BAD fit!")))

da1 <- abrem.fit(da1,dist="weibull",method.fit="mle",col="red1")
da1 <- abrem.fit(da1,dist="weibull",method.fit="mle-rba",col="orange3",lty=2)
da1 <- abrem.fit(da1,dist="lognormal",method.fit=c("rr","xony"),col="steelblue3",pch=8)
da1 <- abrem.conf(da1)
plot(da1)
message("Comparison between RR, MLE and MLE-RBA:")
message(paste0(
    "  RR : beta=",signif(da1$fit[[1]]$beta),
    ", eta=",signif(da1$fit[[1]]$eta)))
message(paste0(
    "  MLE: beta=",signif(da1$fit[[2]]$beta),
    ", eta=",signif(da1$fit[[2]]$eta)))
message(paste0(
    "  MLE: beta=",signif(da1$fit[[3]]$beta),
    ", eta=",signif(da1$fit[[3]]$eta)))
}
