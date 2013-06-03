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
params.to.ft <- function(dist, ... ){
    # function to generate test data that result in a perfect fit
    # (when using MRR)
    # beta,eta:  slope and shape parameters of 2 parameter Weibull
    # event: either an integer with the number of complete observations, or
    # a vector with censoring information (e.g.: c(1,1,1,0,0,0,1)
    #TODO: the code should also generate
    # datasets corresponding to type2 censoring data
    # scheme. This will be especially necessary when
    # method.reg = "surv" will be implemented
    opp <- options.abremplot()
    opp <- modifyList(opp, list(...))

    opa <- options.abrem()
    opa <- modifyList(opa, list(...))
    if(!missing(dist)){
        if(!is.null(opa$n)){
            if(opa$n >= 2){
                if(is.null(opa$event)){
                    opa$event <- rep(1,opa$n)
                }else{
                    stop(match.call()[[1]],": Either \"n\" or \"event\" ",
                        "should be supplied, not both.")
                    }
            }else{
                stop(match.call()[[1]],": Number of failures (\"n\") must be ",
                    "at least 2.")
            }
        }
        ### assuming opa$event is present
        if(!is.null(opa$event)){
            if(length(opa$event) >= 2){
            }else{
                stop(match.call()[[1]],": Number of failures (\"n\") must be ",
                    "at least 2.")
            }
        }else{
            stop(match.call()[[1]],": (\"event\") vector must have length ",
                "of at least 2.")
        }
        if(all(opa$event==0)){
            stop(match.call()[[1]],": There are only censored events in ",
                "the \"event\" vector.")
        }
        if("mrr" %in% tolower(opa$method.fit)){
            if(tolower(dist) %in% c("weibull","weibull2p")){
                if(!is.null(opa$beta) && !is.null(opa$eta)){
                    ranks <- abrem:::mrank.data(
                        data.frame(time=1:length(opa$event),event=opa$event),
                            method.fit=opa$method.fit)$mrank
                    ret <- data.frame(time=qweibull(ranks,opa$beta,opa$eta),event=opa$event)
                        # a good thing that qweibull deals nicely with NA's!
                }else{stop(match.call()[[1]],": \"beta\" and/or \"eta\" arguments not supplied.")}
            }
            if(tolower(dist) %in% c("lognormal","lognormal2p")){
                #message("Currently only \"weibull\" is supported, doing nothing.")
                if(!is.null(opa$meanlog) && !is.null(opa$sdlog)){
                    ranks <- abrem:::mrank.data(
                        data.frame(time=1:length(opa$event),event=opa$event),
                            method.fit=opa$method.fit)$mrank
                    ret <- data.frame(time=qlnorm(ranks,opa$meanlog,opa$sdlog),event=opa$event)
                        # a good thing that qweibull deals nicely with NA's!
                }else{stop(match.call()[[1]],": \"meanlog\" and/or \"sdlog\" arguments not supplied.")}
            }
        }else{
            stop(match.call()[[1]],": Currently, only median rank ",
                "regression (mrr) is supported.")
            ret <- NULL
        }
            # TODO: here the old code using nlm() that was used in  older
            # versions of wbparams.to.ft() should be implemented
            # for the 'Surv' method
        ret
    }else{
        stop(match.call()[[1]],": Target distribution \"dist\" is missing.")
        ret <- NULL
    }
}
