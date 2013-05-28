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
    args <- list(...)
    ret <-list()
    class(ret) <- "abrem"
    timeorder <- c()
    if(!missing(x)){
        ret$data <- NULL
        if(is.vector(x)){
            # assuming vector of lifetimes
            timeorder <- order(x)
            ret$data <- data.frame(time=x[timeorder],event=1)
        }
        if(is.data.frame(x)){
            if(!is.null(x$time) && !is.null(x$event)){
                # dataframe is formatted appropriate
                # extra info is also copied
            timeorder <- order(x$time)
            ret$data <- x[timeorder,]
            }
        }
    }else{
        if(!is.null(args$time)){
            if(is.vector(args$time)){
                # assuming vector of lifetimes
                timeorder <- order(args$time)
                ret$data  <- data.frame(time=args$time[timeorder],event=1)
            }
        }else{stop("No lifetime data was provided.")}
    }
    if(!is.null(args$event) && !is.null(ret$data)){
        if(is.vector(args$event)){
            # assuming event vector
#            ret$data <- cbind(ret$data,event=args$event[order(timeorder)])
            ret$data$event <- args$event[timeorder]
        }
    }
    ret
    # TODO: check what to do with the automatically added row names that
    # are sometimes out of order
}