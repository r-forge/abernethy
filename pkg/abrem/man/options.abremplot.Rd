\name{options.abremplot}
\alias{options.abremplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plotting Options for plot.abrem() Method}
\description{
   The  \code{options.abremplot()} function handles the options for plotting abrem objects from the \pkg{abrem} package.}
\usage{options.abremplot(\dots)}
\arguments{
    \item{...}{Options for plotting abrem objects from the \pkg{abrem} package. See details.}
}
\details{
   The  \code{options.abremplot()} function borrows its internal structure from the \code{\link[graphics]{par}} function of package \pkg{graphics}. It can be used in different ways:
   \code{options.abremplot()}
   
   Returns the currently used options and there values.

   \code{options.abremplot()$col}\cr\code{options.abremplot("col")}
   
   Returns the current value of an option.
   
   \code{options.abremplot(is.plot.grid=FALSE)}\cr\code{options.abremplot(list(is.plot.grid=FALSE,main="Weibull plot"))}
   
   Sets the specified options.
   Currently, there is no way to reset the options to the default values using this function.  One might, before changing any options, store the option list in a temporary variable like  \code{abremplot.default <- options.abremplot()} for restoring it later by running  \code{options.abremplot(abremplot.defaults)}.}
\value{
   \code{options.abremplot()} returns a list containing the current options for plotting abrem objects.  Also, it creates a globally accessible list named  \code{options_abremplot}, holding the options.  One should always use the  \code{options.abremplot()} function to access the option list, do not access the list directly.}
\section{Plot options}{
    \describe{
        \item{\code{main}}{A character string; the main title of the Weibull plot. Defaults to \code{"Weibull Plot\n"}.}
        \item{\code{sub}}{A character string; the subtitle of the Weibull plot. Defauls to \code{NULL}.}
        \item{\code{xlim}}{A numeric vector with two values determining the plotting range of the horizontal axis of the Weibull plot.  \code{plot.abrem()} calculates horizontal limits automatically from the data argument.}
        \item{\code{ylim}}{A numeric vector with two values determining the plotting range of the vertical axis of the Weibull plot.  Allowed values come from the interval \code{]0,1[}. Defauls to \code{c(0.01,0.99)}.}
%        \item{\code{xlab}, \code{ylab}}{Character strings containing the labels for the X and Y axis. Defaults to \code{"Time To Failure"} and \code{"Unreliability [%]"} respectively.}
        \item{\code{coordinate.text.size}}{A number determining the relative coordinate text label size. Defaults to \code{0.7}.}
        \item{\code{signif}}{An integer describing the significant digits of various numbers that are -displayed in the legend. Defaults to \code{4}.}
        \item{\code{pch}}{An integer or character describing the plotting symbol, used for plotting the datapoints.  For more info, see  \code{\link[graphics]{points}}.}
        \item{\code{lwd}}{An integer describing the line width of both the Weibull fitted line and the confidence bounds.}
        \item{\code{lwd.points}}{An integer describing the size of the datapoints. Defaults to \code{2}.}
        \item{\code{col}}{The color of a Weibull plot.  Can either be an integer or a string.  See  \code{\link[grDevices]{colors}} for available colors.}
        \item{\code{col.grid}}{The color of the grid.  Can either be an integer or a string.  See  \code{\link[grDevices]{colors}} for available colors.}
        \item{\code{is.plot.grid}}{Boolean switch controlling the plotting of the grid. Defaults to \code{TRUE}.}
        \item{\code{is.plot.fittedline}}{Boolean switch controlling the plotting of the fitted line. Defaults to \code{TRUE}.}
        \item{\code{is.plot.datapoints}}{Boolean switch controlling the plotting of the datapoints. Defaults to \code{TRUE}.}
        \item{\code{is.plot.datacoordinates}}{Boolean switch controlling the plotting of the text labels (the coordinates on the plot) next to the datapoints. Defaults to \code{FALSE}}
        \item{\code{is.plot.legend}}{Boolean switch controlling the plotting of the legend.  Defaults to \code{TRUE}.}
        \item{\code{legend.text.size}}{A number determining the relative legend text size. Defaults to \code{0.7};}
        \item{\code{legend.title}}{A character string with the title of the legend box. Defaults to \code{NULL};}
        \item{\code{is.legend.blives}}{Boolean switch controlling the plotting of B-lives in the legend. Defaults to \code{TRUE}.}
        \item{\code{is.legend.gof}}{Boolean switch controlling the plotting of goodness-of-fit indications in the legend. Defaults to \code{FALSE}.}
        \item{\code{is.plot.cb}}{Boolean switch controlling the plotting of various types of confidence bounds. Defaults to \code{TRUE}.}
        \item{\code{persistent}}{Experimental parameter, do not use.  Defaults to \code{TRUE}.}
    }
}
\author{Jurgen Symynck \email{jurgen.symynck@kahosl.be}}
\note{}
\section{To Do}{
  \itemize{ 
      \item Add the possibility to reset the options to their default values.}}
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{Abrem}}
\code{\link{options.abrem}}
%\code{\link[=abremout:plot.abrem]{plot.abrem}}
% above doesn't work...
}
\examples{
    abremplot.defaults <- options.abremplot()
    options.abremplot(main="New plot title\n",col="red",is.plot.legend=FALSE)
    options.abremplot()$main
    options.abremplot()$col
    options.abremplot()$is.plot.legend
    invisible(options.abremplot(abremplot.defaults))
}
