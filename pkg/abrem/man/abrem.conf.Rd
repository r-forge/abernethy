\name{abrem.conf}
\alias{abrem.conf}
\title{Add Confidence To \code{abrem} Objects}
\description{
    This function adds confidence calculations to
    various entities in \code{abrem} objects.
}
\usage{abrem.conf(x,which="last",\dots)}
\arguments{
    \item{x}{Object of class \code{abrem}.}
    \item{which}{Calculate which fit of class \code{abrem} will be processed.
        Can be \code{"all"} and \code{"last"}.
        Currently, only \code{"last"} is implemented.}
    \item{\dots}{Options for calculation confidence,
        and for plotting the results.}
}
\details{
    This function adds confidence calculations to various entities in
    \code{abrem} objects and adds them to the object alongside any pre-existing
    confidence calculations.

    Confidence calculation options are supplied with
    \code{cl}, \code{conf.what}, \code{conf.blives.sides},
    \code{method.conf.blives} and other arguments;
    see \code{\link[abrem]{options.abrem}} for more details.

}
\value{
    The function returns its argument \code{x}, extended with the
    confidence calculations and the optional graphical and calculation arguments
    as provided to the function, enabling the extension of \code{abrem} object
    with additional calculation results.}
\author{Jurgen Symynck \email{jurgen.symynck@kahosl.be}}
\note{
    Currently, only \code{which="last"} is supported, meaning that after each
    fit result, a call to \code{abrem.conf} is needed for calculating
    confidence on  this result.
}
\section{To Do}{
   \itemize{
      \item Implement support for \code{which="all"}.
   }
}
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
    \code{\link{options.abrem}}
}
\examples{
data(p2.2,package="abrem")
da <- Abrem(p2.2)
da <- abrem.fit(da,dist="weibull",method.fit=c("mrr","xony","qbeta"))
da <- abrem.conf(da,method.conf.blives=c("mcpivotal","bbb"))
print(da$fit[[1]]$conf$blives[[1]])
print(da$fit[[1]]$conf$blives[[2]])
}
