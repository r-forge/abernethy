\name{options.abrem}
\alias{options.abrem}
\alias{dist}
\alias{method.fit}
\alias{method.conf.blives}
\alias{conf.what}
\alias{conf.blives.sides}
\alias{conf.n}
\alias{bernard}
\alias{mrr}
\alias{surv}
\alias{xony}
\alias{yonx}
\alias{mcpivotal}
\alias{bbb}
\alias{cl}
\alias{blives}
\alias{verbosity}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
    Abrem Calculation Options
}
\description{
    This function handles the options for the various calculations performed
    by the \pkg{abrem} package.
}
\usage{options.abrem(\dots)}
\arguments{
    \item{\dots}{
        Options for the various calculations performed
        by the \pkg{abrem} package.
    }
}
\details{
    This function borrows its internal structure from
    the \code{\link[graphics]{par}} function of package \pkg{graphics}.
    It can be used in different ways:
   
   \code{options.abrem()}
   
   Returns the currently used options and there values.

   \code{options.abrem()$dist}\cr\code{options.abrem("dist")}
   
   Returns the current value of an option.
   
   \code{options.abrem(cl=0.95)}\cr\code{options.wb(list(cl=95,S=5e4))}
   
   Sets the specified options.
   
   Currently, there is no way to reset the options to the default values using
   this function.  One might, before changing any options, store the option
   list in a temporary variable like  \code{abrem.default <- options.abrem()}
   for restoring it later by running  \code{options.abrem(abrem.defaults)}.

    The function creates a globally accessible list named  \code{options_abrem},
    holding the options.  One should always use the  \code{options.abrem()}
    function to access the option list, do not access this list directly.
}
\value{
   Executing \code{options.abrem()} without arguments returns a named list
   containing the currently active global options of the \pkg{abrem} package.
   
   When supplying arguments to \code{options.abrem()}, these arguments are
   returned in a names list.
}
\section{Abrem options}{
    \describe{
        \item{\code{dist}}{
            A character string with the distribution target for fitting.
            Possible values are \code{"weibull"} and \code{"lognormal"}.
            Defaults to \code{"weibull"}.
        }
        \item{\code{method.fit}}{
            A vector of class \code{"character"} with several fitting options.
            Possible values are \code{"mrr"} for Median Rank Regression,
            \code{"surv"} for Maximum Likelihood Estimation (MLE) by means of
            \code{\link[survival]{survreg}}, \code{"qbeta"} for exact ranking,
            \code{"bernard"} for Bernards approximation to exact ranking,
            \code{"xony"} and \code{"yonx"} for X-on-Y regression or
            Y-on-X regression, respectively.

            Note that currently, ALL relevant settings must be supplied for the
            fitting to succeed, if one supplies \code{"bernard"} then one must
            supply \code{"mrr"} and \code{"xony"} also. Also, only sensible
            combinations of method arguments will result in a fit. For instance,
            \code{"surv"} and \code{"xony"} do not match together because MLE
            uses no rank regression.

            Defaults to \code{c("mrr","xony","qbeta")}.
        }
        \item{\code{S}}{
            An integer describing the number of Monte Carlo simulations on
            which the Monte Carlo pivotal confidence bounds are based.
            High values are needed for good confidence bounds at the lower
            end of the Weibull model, especially for data with heavy censoring.
            Defaults to \code{1e4}.
        }
        \item{\code{cl}}{
            Confidence Level: A number from the interval \code{[0,[1}
            determining the width of the calculated confidence intervals
            for various entities.
        }
        \item{\code{blives}}{
            A numeric vector specifying B-lives for which confidence will
            be calculated.
        }
        \item{\code{conf.n}}{
            An integer controlling the amount of unreliability levels for
            which confidence bounds are calculated and ultimately plotted.
            Higher numbers will result in smoother confidence bounds. In any
            case, confidence intervals will be calculated for the specified
            B-lives, \code{50 [\%]} and the dataset's characteristic life eta.
        }
        \item{\code{pivotals}}{
            Currently unused. Defaults to \code{FALSE}.
        }
        \item{\code{sides}}{
            Either \code{"lower"},  \code{"upper"} or \code{"double"},
            specifying the type of bound(s) to be calculated.
            Defaults to \code{c("double")}, the other options are currently
            not implemented.
        }
    }
}
\author{Jurgen Symynck \email{jurgen.symynck@kahosl.be}}
\note{}
\section{To Do}{
    \itemize{
        \item Add the possibility to reset to the default values.
        \item Implement returning of the pivotals dataframe in the
            \code{abrem} object.
        \item Implement support for lower and upper confidence limits.
        \item Add detection of unsupported arguments.
    }
}
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
    \code{\link{options.abremplot}},
    \code{\link{abrem.fit}},
    \code{\link{abrem.conf}}
}
\examples{
abrem.defaults <- options.abrem()
options.abrem(S=5e5,cl=0.99)
options.abrem()
options.abrem(abrem.defaults)
}
