\name{Abrem}
\alias{Abrem}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create data object for abrem analysis}
\description{
   The  \code{Abrem()} function creates an object of class \code{abrem} for further processing by the other functions of \pkg{abrem} and \pkg{abremout}.}
\usage{Abrem(x,\dots)}
\arguments{
    \item{x}{Either a dataframe containing \code{$time} and \code{$event} columns, or a vector of class \code{"numeric"} or \code{"integer"} with (life)time observations.}
    \item{...}{Further arguments for creating the \code{abrem} object.}
}
\details{}
\value{
   \code{Abrem()} returns an object of class \code{abrem}. It is a named list where the first list item is named \code{$data}, a dataframe with at least two colums, \code{$time} and \code{$event}. The first is an ordered vector with (life)time observations, the latter is an indicator of the censoring type of the corresponding observation: values of \code{1} mean "dead" while \code{0} mean "alive" or censored observations. See \code{\link[survival]{Surv}} for additional info.}
   
\author{Jurgen Symynck \email{jurgen.symynck@kahosl.be}}
\note{}
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
    \code{\link{options.abrem}}
}
\examples{
## These lines all generate the same object ##
Abrem(c(500,1200,900,1300,510))
Abrem(time=c(500,1200,900,1300,510))
Abrem(time=c(500,1200,900,1300,510),event=c(1,1,1,0,0))
da <- data.frame(time=c(500,1200,900,1300,510),event=c(1,1,1,0,0))
Abrem(da)
}
