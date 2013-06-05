\name{plot.abrem}
\alias{plot.abrem}
\alias{mark}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
    Abrem Object Plotting
}
\description{
    This function adds the \code{.abrem} method to \code{\link[graphics]{plot}}
    from the \pkg{graphics} package. Currently,  \code{plot.abrem()}
    behaves as follows:
    \itemize{
        \item Displays the (life)time observations and fits (if any)of an
            \code{abrem} object. The calculated Weibull or Lognormal cumulative
            distribution is plotted on Weibull probability paper. Two-parameter
            Weibull plots are displayed as a straight line.
        \item (right-)Censored data (suspensions) are supported, where some
            observations are not failures.
        % \item Monte Carlo pivotal confidence bounds are calculated and
        % displayed, Beta-binomial confidence bounds can be displayed.
        \item The \code{mark()} function draws a point on the plot with dotted
            lines extending towards the axes, for marking points of interest.}
}
\usage{\method{plot}{abrem}(x, \dots)}
\arguments{
    \item{x}{
        Object of class \code{abrem}.
    }
    \item{\dots}{
        Options for plotting the \code{abrem} object; see section "Details".
    }
}
\details{
    The code{\dots} argument can be any graphical parameter that can be
    supplied to \code{\link[graphics]{plot.default}}, and any option that is
    set by the function \code{\link[abrem]{options.abremplot}}.
}
\value{
    Currently, \code{plot.abrem} returns no value. In the future it will
    return the \code{abrem} object, with any additional plotting
    options appended.
}
\section{To Do/Known issues/Bugs}{
    \itemize{
        \item The legend positions are slightly to the right, overlapping the
            right Y axis (especially for small horizontal ranges).
        \item Plotting confidence bounds with \code{blives=c()} and
            \code{conf.n=0} produces errors and \code{"numeric(0)"} in the legend.
        \item \code{"numeric(0)"} in the legend when supplying
            custom \code{blives} values.
        \item return arguments, with additional plotting information attached.
        \item Legends are still shown even when no fits were calculated: more
            specific when \pkg{survival} is not installed, i still attempts
            to show legends for \code{method.fit="surv"}.
   }
}
\author{Jurgen Symynck \email{jurgen.symynck@kahosl.be}}

\seealso{
    \code{\link[abrem]{Abrem}},
    \code{\link[abrem]{abrem.fit}},
    \code{\link[abrem]{options.abremplot}}
}
\examples{
data(p2.2,package="abrem")
da <- abrem.fit(Abrem(p2.2),dist="weibull",
    method.fit=c("mrr","xony","qbeta"),pch=3)
da <- abrem.conf(da,method.conf.blives="mcpivotal")
da <- abrem.fit(da,dist="weibull",
    method.fit=c("mrr","xony","bernard"),
    col="red",pch=4,lty=2)
da <- abrem.fit(da,dist="weibull",method.fit="surv",
    col="blue",pch="s")
da <- abrem.fit(da,dist="lognormal",method.fit="surv",
    col="green3",pch=8)
plot(da,xlim=c(1e-2,1e4))
}
