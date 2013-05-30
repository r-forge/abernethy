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
mrank.data <- function(x, ...){
    # TODO: error checking
    opa <- options.abrem()
    opa <- modifyList(opa, list(...))
    n <- length(x$time)
        # assuming the data is complete or right censored.
    if(prod(x[is.na(x$time),"event"])==FALSE){
        # the above is TRUE when any of the NA times has a censoring flag
        # (currently, 0 or FALSE).
        # Such data is supposed to be already ranked from low to high,
        # possibly originating from wbparams.to.ft()
        # If the above is FALSE, then the data just might contain
        # missing values (NA) that do not have any function. In that case,
        # the observations are not considered to be ranked from low to high.
        x$rank <- 1:n
    }else{
        x$rank <- rank(x$time,ties.method = "first",na.last="keep")
            # rank the unprocessed fatigue data, identical failure data are assigned
            # sequential order numbers.
    }
    x$rrank <- (n+1-x$rank)
        # reverse rank order
    sx <- x[order(x$rank),]
        # order data according to rank
    parank <- 0
    for (i in 1:n){
        if(!sx$event[i] || is.null(sx$event)){sx$arank[i] <- NA
            # trying to be compatible with Surv object (see 'survival' package)
            # recheck what the above line does exactly and if it is
            # compatible with the diffent methods of supplying observation
            # status.
        }else{
            sx$arank[i] <- (sx$rrank[i]*parank + n +1)/(sx$rrank[i]+1)
            parank <- sx$arank[i]
        }
            # adjusted_rank =
            # (reversed rank * previous adj. rank + n + 1)/(reversed rank + 1)
            # see "The new Weibull handbook, fifth edition" p. 2-7, formula 2-5        }
    }
    if("bernard" %in% tolower(opa$method)){
        if(opa$verbosity >= 1)message("mrr ranking using Bernards approximation...")
        #    mrank.observation(sx$arank,n,method=opa$method.rank)
        sx$mrank <- (sx$arank-0.3)/(n+0.4)
    }else{
        # assume either "qbeta" or "exact"
        if(opa$verbosity >= 1)message("mrr ranking using exact 'qbeta' method...")
        sx$mrank <- qbeta(0.5,sx$arank,n-sx$arank+1)
    }
    sx
}
