# R package 'abrem'
# Abernethy Reliability Methods
# Implementations of lifetime data analysis methods described in
# 'The New Weibull Handbook, Fifth edition' by Dr. Robert B. Abernethy.
# April 2014, Jurgen Symynck
# Copyright 2014, Jurgen Symynck
#
# For more info, visit http://www.openreliability.org/
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
# +-----------------------------------+
# |  execute this software with R:    |
# |  http://www.r-project.org/        |
# +-----------------------------------+

params.to.ob <- function(dist, ... ){
    # function to generate test data that result in a perfect fit
    # (when using RR; X-on-Y, median plot positions)
    # beta,eta:  slope and shape parameters of 2 parameter Weibull
    # event: either an integer with the number of complete observations, or
    # a vector with censoring information (e.g.: c(1,1,1,0,0,0,1)

    opa <- options.abrem()
    opa <- modifyList(opa, list(...))
    if(!missing(dist)){
        if(!is.null(opa$n)){
            if(opa$n >= 2){
                if(is.null(opa$event)){
                    opa$event <- rep(1,opa$n)
                }else{
                    stop("Either Argument \"n\" or \"event\" ",
                        "should be supplied, not both.")
                }
            }else{
                stop("Argument \"n\" (number of failures) must be ",
                    "at least 2.")
            }
        }
        ### assuming opa$event is present
        if(!is.null(opa$event)){
            if(length(opa$event) >= 2){
            }else{
                stop("Argument \"n\" (number of failures) must be ",
                                    "at least 2.")
            }
        }else{
            stop("Argument \"event\" should have length of at least 2.")
        }
        if(all(opa$event==0)){
            stop("Argument \"event\"contains only censored events.")
        }
        if("rr" %in% tolower(opa$method.fit)){
            if(tolower(dist) %in% c("weibull","weibull2p","weibull-2","weibull2p-2")){
                if(!is.null(opa$beta) && !is.null(opa$eta)){
                    if("median" %in% opa$pp){
                        ranks <- rep(NA,length(opa$event))
                        ranks[opa$event==1] <- .Call("medianRank1",opa$event,
                            PACKAGE= "pivotals")
                        ret <- data.frame(time=qweibull(ranks,opa$beta,opa$eta),event=opa$event)
                            # a good thing that qweibull deals nicely with NA's!
#                        if(!is.null(opa$return.na))
#                            if(!opa$return.na){
#                                if(!is.null(opa$return.na))
#                                    if(!opa$return.na){
#
#                            }
#

                    }else{
                        stop("Currently, only \"median\" rank regression is supported.")
                    }
#                        if("benard" %in% opa$pp){
#                            ret$data <- cbind(ret$data,benard=NA)
#                            ret$data[ret$data$event==1,'benard'] <- .Call("medianRank",opa$event,
#                                PACKAGE= "pivotals")
                }else{stop("Arguments \"beta\" and/or \"eta\" not supplied.")}
            }
            if(tolower(dist) %in% c("lognormal","lognormal2p")){
                #message("Currently only \"weibull\" is supported, doing nothing.")
                if(!is.null(opa$meanlog) && !is.null(opa$sdlog)){
                    if("median" %in% opa$pp){
                        ranks <- rep(NA,length(opa$event))
                        ranks[opa$event==1] <- .Call("medianRank1",opa$event,
                            PACKAGE= "pivotals")
                        #ret <- data.frame(time=qweibull(ranks,opa$beta,opa$eta),event=opa$event)
                        ret <- data.frame(time=qlnorm(ranks,opa$meanlog,opa$sdlog),event=opa$event)
                            # a good thing that qweibull deals nicely with NA's!

                    }else{
                        stop("Currently, only \"median\" rank regression is supported.")
                    }
#                    ranks <- rank.data(
#                        data.frame(time=1:length(opa$event),event=opa$event),
#                            method.fit=opa$method.fit)$mrank
#                    ret <- data.frame(time=qlnorm(ranks,opa$meanlog,opa$sdlog),event=opa$event)
                }else{stop("Arguments \"meanlog\" and/or \"sdlog\" not supplied.")}
            }
        }else{
            stop("Currently, only rank regression is supported.")
            ret <- NULL
        }
        ret
    }else{
        stop("Argument \"dist\" is missing.")
        ret <- NULL
    }
}
