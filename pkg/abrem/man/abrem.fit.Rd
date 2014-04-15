\name{abrem.fit}
\alias{abrem.fit}
\alias{dist}
\alias{method.fit}
\alias{prr}
\alias{P-value}
\alias{threshold}
\alias{in.legend}

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

            %Currently, some unsupported values can be passed:
            %\code{"weibull-2"},
            %\code{"weibull2p-2"},
            %\code{"lognormal-2"} and
            %\code{"lognormal2p-2"}. These differentiate between R-based
            %and C++ based implementations of the rank regression code. Check
            %the code of function \code{\link{calculateSingleFit}} for implementation details.
            
            Defaults to \code{"weibull"}.
        }
        \item{\code{in.legend}}{
            Logical value controlling the inclusion of various elements in 
            the legend. 
            
            If \code{in.legend=FALSE} is passed,
            the resulting fit calculations will be omitted from the legend, 
            leaving only observation summary data.
            
            Defaults to \code{TRUE}.
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
                "Benard's approximate median rank regression".
                If this method is used then it is mandatory to additionally specify
                either X-on-Y or Y-on-X regression.

                \item \code{"xony"},\code{"yonx"}: Differentiate between X-on-Y and Y-on-X
                regression, respectively. For rank regression in lifetime analysis,
                it is best practice to use the X values ((life-)time observations)
                as the response variables whose horizontal distance to the fit line
                must be minimized, and the Y values (unreliabilities) as the
                explanatory variable.

                \item \code{"mle"}: Maximum Likelihood Estimation (MLE), using
                many functions of the \pkg{debias} package.
                
                % In the legend a suffix to the fit method can sometimes be
                % observed. This indicates which implementation the software
                % has usedhas to do with ....


                \item \code{"mle-rba"}: Maximum Likelihood Estimation with Reduced Bias
                based on the median bias of the distributions MLE.
                This options uses functions \code{\link[debias]{RBAbeta}} and 
                \code{\link[debias]{RBAsigma}}of the
                \pkg{debias} package.

                %% In the future,
                %% choosing between different implementations of the MLE code and
                %% support for other distributions (already available in package
                %% \pkg{debias}) will be possible.
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
        
        Passing the \code{threshold} parameter here will result in plotting the
        fit (and its associated plot positions) with a threshold value subtracted. 
        
        If three parameter models like \code{weibull3p} are used and 
        \code{threshold = TRUE}, the calculated third parameter (\code{t0}) of
        the \emph{last three-parameter fit} in the abrem object will be used for 
        plotting the graphical entities. If a numeric value is passed, then 
        only the value from the \emph{last} call to \code{abrem.fit} is used.
        % what if different t0 values are given with different fits? this should
        % result in only one fit used.
        % test the statement that the t0 of the last three-parameter fit
        % is used
        
        Currently, there is no graceful error recovery after attempting to fit
        lifetime data including negative time observations, for example 
        \code{abrem.fit(Abrem(-5:10)).}
        
    }
}
\value{
    The function returns its argument \code{x}, extended with the
    calculated fit and the optional graphical and calculation arguments as
    provided to the function.
}
\author{Jurgen Symynck \email{jusy@openreliability.org}}
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
#da1 <- abrem.conf(da1)
me <- "Comparison between RR, MLE, MLE-RBA and lognormal2p"
plot(da1,main=me);message(me)
message(paste0(
    "  RR : beta=",signif(da1$fit[[1]]$beta),
    ", eta=",signif(da1$fit[[1]]$eta)))
message(paste0(
    "  MLE: beta=",signif(da1$fit[[2]]$beta),
    ", eta=",signif(da1$fit[[2]]$eta)))
message(paste0(
    "  MLE: beta=",signif(da1$fit[[3]]$beta),
    ", eta=",signif(da1$fit[[3]]$eta)))
    
### threshold parameter usage demo ###
data(abrem_mix1)
earlyda <-abrem_mix1[1:10]
midda   <-abrem_mix1[11:131]
endda   <-abrem_mix1[132:200]
da       <-Abrem(abrem_mix1,col="gray",pch=1,
            label="Complete, unaltered dataset")
da21     <-Abrem(fail=endda,susp=c(earlyda,midda),col="black",pch=19)
da22     <-Abrem(fail=endda,susp=c(earlyda,midda),col="blue",pch=3)
da23     <-Abrem(fail=endda,susp=c(earlyda,midda),col="green3",pch=4)
da21 <- abrem.fit(da21,
    label="threshold=FALSE",dist="weibull3p",threshold=FALSE)
da22 <- abrem.fit(da22,
    label="threshold=TRUE",dist="weibull3p",threshold=TRUE)
da23 <- abrem.fit(da23,
    label="threshold=5000",dist="weibull3p",threshold=1800)
plot.abrem(list(da,da21,da22,da23),xlim=c(10,1e5),
    main="Threshold parameter usage")
}
