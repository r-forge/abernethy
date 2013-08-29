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
Abrem <- function(x,...){
    #arg <- list(...)
    arg <- splitargs(...)
        # TODO: check effect on c(...) or (...)
#    arg2 <- arg[!(names(arg) %in% names(options.abrem()))]
        # extract the arguments that are NOT abrem options.
    opa <- modifyList(options.abrem(), arg$opa)
    ret <- list()
    class(ret) <- "abrem"
    timeorder <- c()
#    lowest <- 1
    if(!missing(x)){
        ret$data <- NULL
        if(is.vector(x)){
            if(opa$verbosity >= 2)message(match.call()[[1]],
                ": Argument \"x\" is a vector of (life-)time observations...")
            if(any(is.na(x))) timeorder <- 1:length(x)
            else timeorder <- order(x)
                # the above is tp prevens ordering attempts when NA values are
                # present in the lifetime observation vector.
                # having NA values implies that the data must be ordered.
            ret$data <- data.frame(time=x[timeorder],event=1)
        }
        if(is.data.frame(x)){
            if(!is.null(x$time) && !is.null(x$event)){
                if(opa$verbosity >= 2)message(match.call()[[1]],
                    ": Argument \"x\" is a dataframe with $time and $event ",
                        "columns...")
                if(any(is.na(x$time))) timeorder <- 1:length(x$time)
                else timeorder <- order(x$time)
                ret$data  <- as.data.frame(x[timeorder,])
#                ret$data$event <- 1
#                    # temporarily set event vector to 1
            }else{
                stop(": Argument \"x\" is missing $time and/or ",
                    "$event columns...")
            }
        }
    }else{
        if(!is.null(arg$rem$time)){
            if(is.vector(arg$rem$time)){
                if(opa$verbosity >= 2)message(match.call()[[1]],
                    ": Argument \"time\" is vector of complete (life-)time observations...")
                if(any(is.na(arg$rem$time))) timeorder <- 1:length(arg$rem$time)
                else timeorder <- order(arg$rem$time)
                ret$data  <- data.frame(time=arg$rem$time[timeorder],event=1)
            }
        }else{stop("No (life-)time observations were provided.")}
    }
    
    ### setting the event vector correctly ###
    if(!is.null(arg$rem$event) && !is.null(ret$data)){
        if(is.vector(arg$rem$event)){
            if(opa$verbosity >= 2)message(match.call()[[1]],
                ": Argument \"event\" is event vector...")
            ret$data$event <- arg$rem$event[timeorder]
        }
    }
    if("median" %in% opa$pp){
            if(opa$verbosity >= 2)message(match.call()[[1]],
            ": Adding exact median ranks to (life-)time observations ...")
        ret$data <- cbind(ret$data,rank.median=NA)
        ret$data[ret$data$event==1,'rank.median'] <-
            .Call("medianRank1",ret$data$event, PACKAGE= "pivotals")
#        lowest <- min(c(lowest, na.omit(ret$data$rank.median)))
    }
    if("bernard" %in% opa$pp){
            if(opa$verbosity >= 2)message(match.call()[[1]],
            ": Adding Bernards ranks to (life-)time observations ...")
        ret$data <- cbind(ret$data,rank.bernard=NA)
        ret$data[ret$data$event==1,'rank.bernard'] <-
            .Call("medianRank",ret$data$event, PACKAGE= "pivotals")
#        lowest <- min(c(lowest, na.omit(ret$data$rank.bernard)))
    }
    ret$options <- opa
        # always store a full copy of the options.abrem structure here
    ret
    # TODO: check what to do with the automatically added row names that are sometimes out of order
}