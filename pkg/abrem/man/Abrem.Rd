\name{Abrem}
\alias{Abrem}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
    Create an \code{abrem} Object for Lifetime and Reliability Analysis
}
\description{
    This function creates an object of class \code{"abrem"} for further processing
    by the other functions of \pkg{abrem}.
}
\usage{Abrem(x,\dots)}
\arguments{
    \item{x}{
        Either a dataframe containing at least \code{$time} and \code{$event}
        columns, or a vector of class \code{"numeric"} or \code{"integer"} with
        (life-)time observations.
    }
    \item{\dots}{
        Graphical options for plotting the \code{abrem} object; see section "Details".
    }
}
\details{
    There are several methods to supply arguments for building an \code{abrem}
    object.
    \itemize{
        \item When a single unnamed vector of class \code{"numeric"}
            or \code{"integer"} is supplied, it is treated as a vector
            of (life-)time observations.
        \item If argument \code{time} is provided, it is treated as
            a vector of (life-)time observations.
        \item If argument \code{event} is provided, it is treated as
            a vector of event indicator with possible values of
            \code{0} and \code{1}.
        \item If argument \code{x} is of class \code{"data.frame"},
            then it should al least contain \code{$time} and \code{$event}
            columns. Additional columns in the dataframe will be reused in the
            \code{abrem} object, allowing for extra information like
            serial numbers to be included.
    }
    
    Note that is is currently allowed to have \code{NA} values in the
    \code{time} argument. In that case, the vector is expected to be
    ordered and no ordering will be applied by \code{Abrem}. This feature is
    useful in combination with the output of \code{\link{params.to.ob}}

    \code{Abrem} always generates (probability) plot positions for graphically
    displaying the (life-)time observations and for later usage
    by \code{\link{abrem.fit}}. The type of plot positions is
    controlled with the \code{\link{pp}} argument.
    See \code{\link{options.abrem}} for more details.

    Additionally, one can supply any options available from \code{\link{options.abrem}},
    such as \code{\link{col}} or \code{\link{is.plot.legend}}. Some of these options
    will be used when plotting the (life-)time observations using \code{\link{plot.abrem}}.
    Subsequent calls to \code{\link{abrem.fit}} and \code{\link{abrem.conf}}
    will inherit these options.

    % \code{\link[abremout:plot.abrem]{plot.abrem}}.
}
\value{
    This function returns an named list of class \code{"abrem"}. The first list
    item (\code{$data}) is a dataframe with at least three columns:
    \describe{
        \item{\code{$time}}{
            An ordered vector with (life-)time observations.
        }
        \item{\code{$event}}{
            A vector of class \code{"numeric"} with right-censoring indicators.
            Values of \code{1} mean "dead" or "failed" while \code{0}
            mean "alive" or "right-censored"/"suspended" observations.
            This censoring indicator scheme is modeled after the
            \code{\link[survival]{Surv}} function of the \pkg{survival}
            package.
        }
        \item{\code{rank. \ldots }}{
            Depending on the argument \code{\link{pp}} (either supplied as
            an argument or taken from \code{\link{options.abrem}}), a vector
            of class \code{"numeric"} with exact median ranks
            (\code{$rank.median}) or Bernard's approximate median ranks
            (\code{$rank.bernard}).
        }
    }
}
\author{Jurgen Symynck \email{jurgen.symynck@kahosl.be}}
\note{}
\section{To Do}{
    \itemize{
        \item Add support for specifying right censored (life-)time observations
        using the format

        \code{Abrem(fail=c(500,1200,900),susp=c(1300,510))}.
    }
}
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
    \code{\link{options.abrem}}
}
\examples{
## These code lines all generate the same object ##
Abrem(c(500,1200,900,1300,510))
Abrem(time=c(500,1200,900,1300,510))
Abrem(time=c(500,1200,900,1300,510),event=c(1,1,1,1,1))
# Abrem(fail=c(500,1200,900,1300,510))
# Abrem(fail=c(500,1200,900,1300,510),susp=c())
da <- data.frame(
    serial=c("S12","S16","S17","S3","S5"),
    time=c(500,1200,900,1300,510),
    event=c(1,1,1,1,1))
Abrem(da)
## generate a similar dataset with suspentions ##
Abrem(time=c(500,1200,900,1300,510),event=c(1,1,1,0,0))
# Abrem(fail=c(500,1200,900),susp=c(1300,510))
da <- data.frame(time=c(500,1200,900,1300,510),event=c(1,1,1,0,0)); Abrem(da)
}
