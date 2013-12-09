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
    # x is a single Abrem or a list of Abrem objects
    supported_dist <- c(
        "weibull","weibull2p","weibull-2","weibull2p-2",
        "weibull3p",
        "lognormal","lognormal2p","lognormal-2","lognormal2p-2",
        "lognormal3p")
    supported_fit <-  c("rr","mle","mle-rba","mle2","mle2-rba")
        # TODO: expand with choices between "pivotals", CPP and internal code
        # TODO: this should probably change to "rr-2" and "mle-2"!
    if(missing(x)){
        stop("Argument \"x\" is missing.")
    }else{
        if(identical(class(x),"abrem")) x <- list(x)
        if(!all(sapply(x,function(x)identical(class(x),"abrem")))){
            stop("\"x\" argument is not of class \"abrem\" or ",
            "a list of \"abrem\" objects.")
        }
        # from here on, x is a list of one or more abrem object
        if(!is.null(x[[1]]$options)){
            opa <- x[[1]]$options
            # use the options of the first abrem object in the list
        }else stop("Abrem object has not options set.")
        opa <- modifyList(opa, list(...))
        if(is.null(opa$dist)){
            if(opa$verbosity >= 1)message("abrem.fit : ",
                ": Target distribution defaults to Weibull 2P.")
                opa$dist <- "weibull2"
        }else{
            if(length(opa$dist)>1)
                stop("Too many target distributions supplied.")
            if(!any(tolower(opa$dist) %in% supported_dist)){
                stop(paste0(opa$dist," is not a supported target fit method."))
            }else{
                if(is.null(opa$method.fit)){
                    if(opa$verbosity >= 1)message("abrem.fit : ",
                        ": Fit method defaults to \"rr\", \"xony\".")
                        opa$method.fit <- c("rr","xony")
                }else{
                    fits <- length(which(opa$method.fit %in% supported_fit))
                    if(fits > 1){
                        stop("Only one fit method should be supplied.")
                    }else{
                        if("rr" %in% tolower(opa$method.fit)){
                            if(!any(c("xony","yonx") %in%
                                tolower(opa$method.fit))){
                                if(opa$verbosity >= 1){
                                    message("abrem.fit : ",
                                        ": Fit method \"rr\" defaults to \"xony\"")
                                    opa$method.fit <- c(opa$method.fit,"xony")
                                }
                            }
                        }
                        x <- lapply(x,calculateSingleFit,...)
                        # TODO: only one object or list of abrem objects?
                    }
                }
            }
        }
    }
    if(length(x)==1) x[[1]] else x
        # return list of abrem objects when argument x was a list
        # otherwise return single abrem object
}