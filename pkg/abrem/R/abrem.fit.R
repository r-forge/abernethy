# R package 'abrem'
# Abernethy Reliability Methods
# Implementations of lifetime data analysis methods described in
# 'The New Weibull Handbook, Fifth edition' by Dr. Robert B. Abernethy.
# May 2013, Jurgen Symynck
# Copyright 2013, Jurgen Symynck
#
# For the latest version of this file, check the Subversion repository at
# http://r-forge.r-project.org/projects/abernethy/
#
# Disclaimer:
#    The author is not affiliated with Dr. Abernethy or Wes Fulton - CEO of
#    Fulton Findings(TM) and author of the software package SuperSMITH
#-------------------------------------------------------------------------------
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#    For more info on this software and its predecesser, the "weibulltoolkit",
#    consult following documents:
#
#    - "Weibull analysis using R, in a nutshell",
#      (Jurgen Symynck, Filip De Bal, 2010)
#    - "Monte Carlo pivotal confidence bounds for Weibull analysis
#      with implementations in R",
#      (Jurgen Symynck, Filip De Bal, 2011)
#
# +-----------------------------------+
# |  execute this program with R:     |
# |  http://www.r-project.org/        |
# +-----------------------------------+
#
abrem.fit <- function(x,...){
    args <- splitargs(list(...))
    opp <- options.abremplot()
    opa <- options.abrem()
#    opp <- ifelse(is.null(args$opp),NULL,modifyList(opp, args$opp))
    # TODO: check for argument "add": this shoudn'gt be supplied here
    if(length(args$opp)==0){opp <- NULL
    }else{opp <- modifyList(opp, args$opp)}
        # opp is NULL when no graphical parameters
        # have been passed through the function.
    opa <- modifyList(opa, args$opa)
    if(!missing(x)){
        if(identical(class(x),"abrem")){
            if(is.null(x$fit)){
                # create the first fit in the abrem object
                i <- 1
                x$fit <- list()
            }else{
                i <- length(x$fit)+1
                # append a new fit to the existing object
            }
            x$fit[[i]] <- list()
            #opa <- options.abrem()
            #opa <- modifyList(opa, list(...))
            x$fit[[i]]$options <- opa
            if(!is.null(args$opp)) x$fit[[i]]$options.abremplot <- args$opp
                # save the graphical settings explicitly specified
                # in the function call into the fit
                # this means tha NOT ALL graphical options are saved,
                # just the ones that differ from the standard settings.
            x$fit[[i]]$n <- length(x$data$time)
                # this assumes that any NA time in the time column
                # of d is there for a good reason:
                # accompanied with a censoring indicator (0 or FALSE)
            x$fit[[i]]$fail <- sum(x$data$event)
            x$fit[[i]]$cens <- x$fit[[i]]$n-x$fit[[i]]$fail
            if(all(c("mrr","xony") %in% tolower(opa$method.fit)) &&
                (tolower(opa$dist) %in% c("weibull","weibull2p"))){
                    x$fit[[i]]$data  <- mrank.data(x$data,method = opa$method.fit)
                        # just pass the whole vector for further processing
                    #x$fit[[i]]$lm  <- mrr(x$fit[[i]]$data)
                    x$fit[[i]]$lm  <- lm(log(x$fit[[i]]$data$time) ~
                        F0inv(x$fit[[i]]$data$mrank),x$fit[[i]]$data)
                    x$fit[[i]]$beta <- 1/coef(x$fit[[i]]$lm)[2]
                    x$fit[[i]]$eta  <- exp(coef(x$fit[[i]]$lm)[1])
            }
            if("surv" %in% tolower(opa$method.fit)){
                # expand code to only support this
                # when survival package is loaded
                if(require(survival)){
                    x$fit[[i]]$data <-
                        Surv(time=x$data$time,event=x$data$event)
                    x$fit[[i]]$survreg <-
                        survreg(x$fit[[i]]$data~1,dist=opa$dist)
                    if(tolower(opa$dist) %in% c("weibull","weibull2p")){
                        x$fit[[i]]$beta <- 1/x$fit[[i]]$survreg$scale
                        x$fit[[i]]$eta  <-
                            exp(x$fit[[i]]$survreg$coefficients[1])
                    }
                    if(tolower(opa$dist) %in% c("lognormal","lognormal2")){
                        x$fit[[i]]$meanlog <- x$fit[[i]]$survreg$coefficients[1]
                        x$fit[[i]]$sdlog <- x$fit[[i]]$survreg$scale
                    }
                }else{
                    warning("Currently, \"surv\" is only supported through ",
                        "package survival.")
                        # TODO: check the effect of message() or warning()
                        # on r-forge building
                }
            }
            if("yonx" %in% tolower(opa$method.fit)){
                warning("Y on X regression is currently not supported, ",
                    "doing nothing.")
            }
            if("prcomp" %in% tolower(opa$method.fit)){
                warning("Principal component / Single Value Decomposition ",
                    "is currently not supported, doing nothing.")
            }
#           list(data=d,beta=as.double(beta),eta=as.double(eta),
#              n=n,fail=fail,cens=cens)
              # the return structure of this function needs to be revised to include
              # lm() output
        }else{stop("Input data is not of class \"abrem\".")}
    }else{stop("No lifetime data provided.")}
    x
}