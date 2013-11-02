\name{abrem.conf}
\alias{abrem.conf}
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

    Confidence calculation options are supplied with
    \code{\link{cl}}, \code{\link{conf.what}}, \code{\link{conf.blives.sides}},
    \code{\link{method.conf.blives}} and other arguments;
    see \code{\link{options.abrem}} for more details.
    
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
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
    \code{\link{options.abrem}}
}
\examples{
da1 <- Abrem(runif(10,100,1e4))
da1 <- abrem.fit(da1,dist="weibull",method.fit=c("rr","xony"))
da1 <- abrem.conf(da1,method.conf.blives="mcpivotals",col="red")
da1 <- abrem.conf(da1,method.conf.blives="bbb",col="orange")
print(da1$fit[[1]]$conf$blives[[1]])
plot(da1,main="Comparison between MC Pivotal bounds and BB Bounds\n")

## demo of variability in the MC Pivotal conf. bounds ###
da2 <- Abrem(rweibull(5,3,1000))
da2 <- abrem.fit(da2,dist="weibull",method.fit=c("rr","xony"))
for(i in 1:20){
    da2 <- abrem.conf(da2,S=1000,lwd=1,col="red")
}
plot(da2,is.plot.legend=FALSE,
    main="Variability in MC Pivotal Conf. Bounds for S=1000\n")
}
