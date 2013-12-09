\name{abrem.conf}
\alias{abrem.conf}
\alias{contours}
\alias{MLE contours}
\alias{cl}
\alias{conf.n}
\alias{conf.what}
\alias{method.conf.blives}
\alias{conf.blives.sides}
\alias{S}

\title{Add Confidence to \code{abrem} Objects}
\description{
    This function adds confidence calculations to
    various entities in \code{abrem} objects.
}
\usage{abrem.conf(x,which="all",\dots)}
\arguments{
    \item{x}{Object of class \code{"abrem"}.}
    \item{which}{Calculate which fit in the \code{abrem} object will be processed.}
    \item{\dots}{Options for calculating confidence, and for plotting the results.}
}
\details{
    This function adds confidence calculations to various entities in
    \code{abrem} objects and adds them to the object alongside any pre-existing
    confidence calculations.
    
    Additional options for calculating B-life confidence are passed with:

    \describe{
        \item{\code{cl}}{
            Confidence level: A single number from the interval \code{[0,[1}
            specifying the confidence level for various confidence calculations.

            Defaults to \code{0.9}.
        }
        \item{\code{conf.blives.sides}}{
            Either \code{"lower"}, \code{"upper"} or \code{"double"},
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

            Defaults to \code{25}.
        }
        \item{\code{conf.what}}{
            A vector of class \code{"character"} describing for which entities
            that confidence should be calculated.

            Defaults to \code{c("blives")}, the only type currently supported.
        }
        \item{\code{method.conf.blives}}{
            A vector of class \code{"character"} describing the technique to be
            used for calculating confidence for B-lives. Possible values are
            \code{"bbb"} (Beta Binomial confidence bounds),
            \code{"lrb"} (Likelihood Ratio confidence bounds) and
            \code{"mcpivotals"} or \code{"mcpivotal"} (Monte Carlo Pivotal
            confidence bounds).
            
            Monte Carlo Pivotal confidence bounds use a large number of
            simulations to calculate the confidence bounds. See option
            \code{"S"} for more info.
            
            Defaults to \code{c("mcpivotals")}.
        }
        \item{\code{S}}{
            An integer describing the number of Monte Carlo simulations on
            which the Monte Carlo pivotal confidence bounds and calculation
            of the "prr" goodness-of-fit indicator are based.
        
            High values are needed for good confidence bounds at the lower
            end of the fitted model, especially for data with heavy censoring.

            Note that \code{S >= 1000} and that \code{S} must be divisible by 10.
            
            Defaults to \code{1e4}.
        }
    }

    Additionally, one can pass any options available from \code{options.abrem},
    such as \code{col} or \code{is.plot.legend}. The graphical options
    will be used when plotting the (life-)time observations using \code{plot.abrem}.
}
\value{
    The function returns its argument \code{x}, extended with the confidence
    calculations and any optional graphical and calculation arguments
    as passed to the function.
}
\author{Jurgen Symynck \email{jurgen.symynck@kahosl.be}}
\note{
   \itemize{
        \item Currently, only \code{which = "all"} is supported, meaning that a
            call to \code{abrem.conf} attempts calculation of confidence for all
            fits in the \code{abrem} object.
        \item Currently, only \code{conf.what = "blives"} and
            \code{conf.blives.sides = "double"} are supported.
    }
}
\section{Warning}{
    Currently, the Monte Carlo
    pivotal confidence bounds are only identical to superSMITH's
    MC pivotal bounds for complete, uncensored data. For heavily censored
    datasets with few failures, the bounds appear more optimistic than
    superSMITH's bounds. Research on this issue is ongoing.
}
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
    \code{\link{options.abrem}},
    \code{\link{contour.abrem}}
}
\examples{
## full dataset ##
da1 <- Abrem(runif(10,100,1e4),label="Complete data")
da1 <- abrem.fit(da1)
da1 <- abrem.conf(da1,method.conf.blives="mcpivotals",col="red")
da1 <- abrem.conf(da1,method.conf.blives="bbb",col="orange")
da1 <- abrem.conf(da1,method.conf.blives="lrb",col="yellow3")
print(da1$fit[[1]]$conf$blives[[1]])
plot(da1,main="Comparison between MC Pivotal bounds and BB Bounds\n")

## censored dataset: generatas a warning for MC Pivotal confidence bounds ##
da2 <- runif(8,100,1e4)
da2 <- Abrem(fail=da2,susp=rep(max(da2),2),label="Type II censored data")
    # generate a 'type 2' censored dataset
da2 <- abrem.fit(da2)
da2 <- abrem.conf(da2,method.conf.blives="mcpivotals",col="blue1")
da2 <- abrem.conf(da2,method.conf.blives="bbb",col="steelblue")
da2 <- abrem.conf(da2,method.conf.blives="lrb",col="cyan3")
plot(da2,main="Comparison between different bound types.\n")

## show variability in Monte Carlo Pivotal bounds with low S ##
da3 <- Abrem(rweibull(5,3,1000))
da3 <- abrem.fit(da3)
for(i in 1:20) da3 <- abrem.conf(da3,S=1000,lwd=1,col="red")
    # just keep adding bounds to the abrem object...
plot(da3,is.plot.legend=FALSE,
    main="Variability in MC Pivotal Conf. Bounds for S=1000\n")
}
