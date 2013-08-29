\name{options.abrem}
\alias{options.abrem}
\alias{dist}
\alias{method.fit}
\alias{conf.what}
\alias{conf.blives.sides}
\alias{conf.n}
\alias{method.conf.blives}
\alias{pp}
\alias{S}
\alias{pivotals}
\alias{cl}
\alias{blives}
\alias{verbosity}
\alias{main}
\alias{sub}
\alias{xlim}
\alias{ylim}
\alias{xlab}
\alias{ylab}
\alias{log}
\alias{coordinate.text.size}
\alias{signif}
\alias{pch}
\alias{lwd}
\alias{lwd.points}
\alias{lty}
\alias{cex.points}
\alias{col}
\alias{col.grid}
\alias{is.plot.grid}
\alias{is.plot.fit}
\alias{is.plot.pp}
\alias{is.plot.ppcoordinates}
\alias{is.plot.legend}
%\alias{#legend.position}
\alias{legend.text.size}
\alias{label}
\alias{is.legend.blives}
\alias{is.legend.gof}
\alias{is.plot.cb}
\alias{persistent}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
    \pkg{Abrem} Options
}
\description{
    This function handles the various calculation, printing and plotting options
    of the \pkg{abrem} package.
}
\usage{options.abrem(\dots)}
\arguments{
    \item{\dots}{
        Options for calculating, printing and plotting with the \pkg{abrem}
        package.
    }
}

\details{
    This function borrows its internal structure from
    the \code{\link[graphics]{par}} function of package \pkg{graphics}.
    It can be used in different ways:

    \describe{
        \item{\code{options.abrem()}}{
            Returns the currently used options and there values.
        }
        \item{\code{options.abrem()$dist}\cr\code{options.abrem("dist")}}{
            Returns the current value of an option.
        }
        \item{\code{options.abrem(cl=0.95)}\cr
        \code{options.abrem(list(cl=0.95,S=5e4))}}{
            Sets the specified options.
        }
    }

    Currently, there is no way to reset the options to the default values using
    this function.  One might, before changing any options, store the option
    list in a temporary variable like

    \code{abrem.defaults <- options.abrem()}

    for restoring it later by running \code{options.abrem(abrem.defaults)}.

    The function creates a globally accessible list named  \code{options_abrem},
    holding the options.  One should always use the  \code{options.abrem}
    function to access the option list, do not access this list directly.

%            Possible values are \code{"rr"} for Rank Regression,
%            \code{"mle"} for Maximum Likelihood Estimation (MLE) by means of
%            \code{\link[survival]{survreg}}, \code{"qbeta"} for exact ranking,
%            \code{"bernard"} for Bernards approximation to exact ranking,
%            \code{"xony"} and \code{"yonx"} for X-on-Y regression or
%            Y-on-X regression, respectively.
%            Defaults to \code{c("mrr","xony","qbeta")}.%

%            Note that currently, ALL relevant settings must be supplied for the
%            fitting to succeed, if one supplies \code{"bernard"} then one must
%            supply \code{"mrr"} and \code{"xony"} also. Also, only sensible
%            combinations of method arguments will result in a fit. For instance,
%            \code{"surv"} and \code{"xony"} do not match together because MLE
%            uses no rank regression.

}
\value{
   Executing \code{options.abrem} without arguments returns a named list
   containing the currently active global options of the \pkg{abrem} package.
   
   When arguments were supplied, these are returned in a named list.
}
\section{Abrem options}{

            Calculation oriented options:

    \describe{
        \item{\code{dist}}{
            A character string with the distribution target for fitting.
            Possible values are \code{"weibull"} or \code{"weibull2p"},
            \code{"weibull3p"} (three parameter Weibull), \code{"lognormal"}
            or \code{"lognormal2p"}
            % some undocumented values are

            Defaults to \code{"weibull"}.
        }
        \item{\code{method.fit}}{
            A vector of class \code{"character"} with fitting options.
            See section "details" for in-depth discussion of the settings.
        }
        \item{\code{conf.what}}{
            A vector of class \code{"character"} describing for which entities
            that confidence should be calculated.

            Defaults to \code{c("blives")}, the only type currently supported.
        }
        \item{\code{conf.blives.sides}}{
            Either \code{"lower"},  \code{"upper"} or \code{"double"},
            specifying the type of bound(s) to be calculated.

            Defaults to \code{c("double")}, the other options are currently
            not implemented.
        }
        \item{\code{conf.n}}{
            An integer controlling the amount of unreliability levels for
            which confidence bounds are calculated and ultimately plotted.
            Higher numbers will result in smoother confidence bounds. In any
            case, confidence intervals will be calculated for:
            \itemize{
                \item the B-lives specified with option \code{\link{blives}}
                \item the B-life at \code{50 [\%]} unreliability
                \item the B-life at the calculcate characteristic life
                    or logmean (depending on the fitted distribution)
            }
        }
        \item{\code{method.conf.blives}}{
            A vector of class \code{"character"} describing the techniques used
            for calculating confidence for B-lives. Possible values are
            \code{"mcpivotals"} (Monte Carlo Pivotal confidence bounds)
            and \code{"bbb"} (Beta Binomial confidence bounds).

            Defaults to \code{c("mcpivotals")}.
        }
        \item{\code{pp}}{
            \code{pp} is short for "(probability) plot postions". A vector of
            class \code{"character"} describing the type of plot positions to be
            calculated.
            Possible values are \code{"median"} (exact median plot postions
            calculated using the Incomplete Beta function) and  \code{"bernard"}
            (a good approximation to the 'median' plot positions). Both options
            can be supplied at the same time but currently, only the first
            element in the \code{\link{pp}} vector will be used for further fitting.
            
            Currently, there are no plans to implement less useful
            \code{"hazen"} and \code{"mean"} plotting positions.

            Defaults to \code{c("median")}.
        }
        \item{\code{S}}{
            An integer describing the number of Monte Carlo simulations on
            which the Monte Carlo pivotal confidence bounds AND calculation
            of the "prr" goodness-of-fit indicator are based.
            High values are needed for good confidence bounds at the lower
            end of the fitted model, especially for data with heavy censoring.
            
            Note that \code{S >= 1000} and that \code{S} must be divisible by 10.

            Defaults to \code{1e4}.
        }
        \item{\code{pivotals}}{
            Currently unused. Defaults to \code{FALSE}.
        }
        \item{\code{cl}}{
            Confidence level: A number from the interval \code{[0,[1}
            determining the width of the calculated confidence intervals
            for various entities.

            Defaults to \code{0.9}.
        }
        \item{\code{blives}}{
            A numeric vector indicating unreliability levels for which
            B-life confidence will be calculated.

            Defaults to \code{c(0.1,0.05,0.01)}.
        }
    }

        Graphically oriented options:

    \describe{
        \item{\code{verbosity}}{
            An integer from \code{c(1,2,3)} setting the level of verbosity of
            all \pkg{abrem} functions. Use this for debugging.

            Defaults to \code{1}.
        }
        \item{\code{main}}{
            A character string; the main title of the plot.

            Defaults to \code{"Probability Plot\\n"}.
        }
        \item{\code{sub}}{
            A character string; the subtitle of the plot.

            Defaults to \code{NULL}.
        }
        \item{\code{xlim}}{
            A numeric vector with two values determining the plotting range
            of the horizontal axis of the plot.
            \code{plot.abrem} calculates horizontal limits automatically
            from its data argument.

            Defaults to \code{NULL}.
        }
        \item{\code{ylim}}{
            A numeric vector with two values determining the plotting range
            of the vertical axis of the plot.  Allowed values come
            from the interval \code{]0,1[}. \code{plot.abrem} calculates
            vertical limits automatically from its data argument; if vertical
            plotting positions do not exceed the standard range of
            \code{c(0.01,0.99)}, then this standard range is used.
            Otherwise, the standard range is expanded to include the
            extreme plot positions.

            Defaults to \code{NULL}.
        }
        \item{\code{xlab}, \code{ylab}}{
            Character strings containing the labels for the X and Y axis.

            Default to \code{"Time To Failure"} and
            \code{"Unreliability [\%]"} respectively.
        }
        \item{\code{log}}{
            A character string describing the type of plotting canvas to use.
            Possible values are \code{"x"} (Weibull canvas), \code{"xy"}
            (Lognormal canvas), \code{""} (Weibull Y axis, linear X axis)
            and \code{"y"} (Lognormal Y axis, linear X axis).

            Defaults to \code{"x"}, Weibull canvas.
        }
        \item{\code{coordinate.text.size}}{
            A number determining the relative coordinate text label size.

            Defaults to \code{0.7}.
        }
        \item{\code{legend.text.size}}{
            A number determining the relative legend text size.

            Defaults to \code{0.7}.
        }
        \item{\code{label}}{
            A character string with the title of the legend box, can be used to
            label a dataset or fit.

            Defaults to \code{""}.
        }
        \item{\code{signif}}{
            An integer describing the significant digits of various numbers
            that are displayed in the legend.

            Defaults to \code{4}.
        }
        \item{\code{pch}}{
            An integer or single character describing the plotting symbol, used for
            plotting the datapoints.  For more info, see
            \code{\link[graphics]{points}}.

            Defaults to \code{1}.
        }
        \item{\code{lwd}}{
            An integer describing the line width of both the fitted
            line and the confidence bounds, if any.

            Defaults to \code{2}.
        }
        \item{\code{lwd.points}}{
            An integer describing the thickness of the datapoint glyphs.

            Defaults to \code{2}.
        }
        \item{\code{cex.points}}{
            An number describing the relative size of the datapoint glyphs.

            Defaults to \code{1}.
        }
        \item{\code{col}}{
            An integer or character string describing the color of a
            graphical entity.
            See function \code{\link[grDevices]{colors}} for available colors.

            Defaults to \code{"black"}.
        }
        \item{\code{col.grid}}{
            An integer or character string describing the color of the grid

            Defaults to \code{"gray"}.
        }
        \item{\code{is.plot.grid}}{
            Boolean switch controlling the plotting of the grid.

            Defaults to \code{TRUE}.
        }
        \item{\code{is.plot.fit}}{
            Boolean switch controlling the plotting of the fitted line.

            Defaults to \code{TRUE}.
        }
        \item{\code{is.plot.pp}}{
            Boolean switch controlling the plotting of the
            (life-)time observations plot positions.

            Defaults to \code{TRUE}.
        }
        \item{\code{is.plot.ppvalues}}{
            Boolean switch controlling the plotting of the text labels
            (the coordinates on the plot) next to the plot positions.

            Defaults to \code{FALSE}.
        }
        \item{\code{is.plot.legend}}{
            Boolean switch controlling the plotting of the legend.

            Defaults to \code{TRUE}.
        }
        \item{\code{is.legend.blives}}{
            Boolean switch controlling the plotting of B-life confidence bounds
            in the legend.

            Defaults to \code{TRUE}.
        }
        \item{\code{is.legend.gof}}{
            Boolean switch controlling the plotting of goodness-of-fit
            indicators in the legend.

            Defaults to \code{TRUE}.
        }
        \item{\code{is.plot.cb}}{
            Boolean switch controlling the plotting of various types of
            confidence bounds (if they are present in the \code{abrem} object).

            Defaults to \code{TRUE}.
        }
        \item{\code{persistent}}{
            Experimental parameter, do not use.

            Defaults to \code{TRUE}.
        }
    }
}
\author{Jurgen Symynck \email{jurgen.symynck@kahosl.be}}
\note{
    Typical usage of \pkg{abrem} involves calling a chain of functions like:

    \code{da <- Abrem(c(10,11,27))}\cr
    \code{da <- abrem.fit(da)}\cr
    \code{da <- abrem.conf(da)}\cr
    \code{plot(da)}

    Do not call \code{options.abrem} in between these functions because some
    options are locked and cannot be altered further in this chain. This is an
    implication of the way the \code{abrem} object is structured.
    
    The correct time to specify an option is when it is needed for the first
    time. For example, when the color setting option \code{col = "red"} is
    supplied as an option of function \code{Abrem}, it will be used for
    datapoints, fits and confidence bounds. If supplied to \code{\link{abrem.conf}},
    only the confidence bounds will have the specified color, hereby overriding
    any previously inherited color setting from \code{\link{abrem.fit}}
    or \code{\link{Abrem}}.
}
\section{To Do}{
    \itemize{
        \item Add the possibility to reset to the default values.
        \item Implement returning of the pivotals dataframe in the
            \code{abrem} object.
        \item Add detection of unsupported arguments.
    }
}
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
    \code{\link{Abrem}},
    \code{\link{abrem.fit}},
    \code{\link{abrem.conf}},
    \code{\link{plot.abrem}}
}
\examples{
## backup options ##
abrem.defaults <- options.abrem()

## setting new options ##
options.abrem(S=5e5,cl=0.99)
%options.abrem(sub="Testing options.abrem()")

## listing options ##
options.abrem()
options.abrem()$main

## restore options ##
options.abrem(abrem.defaults)
}
