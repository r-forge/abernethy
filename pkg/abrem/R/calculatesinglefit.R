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
calculateSingleFit <- function(x,...){
    # x is a single Abrem object
    
    opadata <- x$options
    opafit <- modifyList(opadata,list(...))
    debug1 <- function(){
        if(opafit$verbosity >= 2)message(paste0(
            "calculateSingleFit: Attempting ",opafit$dist," (",
            paste0(opafit$method.fit,collapse=", "),
            "), pp = \"",opafit$pp,"\" fit..."))
    }
    debug2 <- function(){
        if(opafit$verbosity >= 2)message(paste0(
            "calculateSingleFit: Attempting ",opafit$dist," (",
            paste0(opafit$method.fit,collapse=", "),") fit..."))
    }
    neededcolumns <- function(ppp=NULL){
        rankcolumn <- function(colname,ppp){
            na <- unlist(strsplit(tolower(colname),".",TRUE))
            identical(na[1],"rank") && identical(na[2],ppp)
        }
        basis <- x$data[,c("time","event")]
        if(is.null(ppp)){
            basis
        }else{
            wh <- which(sapply(names(x$data),rankcolumn,ppp,USE.NAMES=FALSE))
            cbind(basis,rank=x$data[,wh])
        }
    }

    i <- 1
    atleastonefit <- FALSE
    if(is.null(x$fit)){
        if(opafit$verbosity >= 2)message(
            "calculateSingleFit: Creating the first fit in the abrem object...")
        i <- 1
        x$fit <- list()
    }else{
        if(opafit$verbosity >= 2)message(
            "calculateSingleFit: Appending a new fit to the existing abrem object...")
        i <- length(x$fit)+1
    }
    x$fit[[i]] <- list()
    op <- unique(c(names(x$options),names(opafit)))
        # this is needed to add options from opafit into li that
        # are NULL in x$options
        # TODO:tolower() needed?
    if(length(li <- opafit[sapply(op,function(y){
        !identical(x$options[[y]], opafit[[y]])})]) > 0){
        x$fit[[i]]$options <- li
        # the above enlists only options that are different from the abrems
        # 'main' options. This excludes options$dist and options$method.fit
    }
    x$fit[[i]]$n    <- length(x$data$time)
        # TODO: this assumes that any NA time (in any present
        # in the time column is there for a good reason:
        # accompanied with a censoring indicator (0 or FALSE)
        # TODO: check if the above code  is still valid!
    x$fit[[i]]$fail <- sum(x$data$event)
    x$fit[[i]]$cens <- x$fit[[i]]$n-x$fit[[i]]$fail

    if("rr" %in% tolower(opafit$method.fit)){
        # +-------------------+
        # |  rank regression  |
        # +-------------------+
        if(tolower(opafit$dist) %in% c("weibull","weibull2p",
            "weibull-2","weibull2p-2")){
            #prepfitlist()
            debug1()
            x$fit[[i]]$data <- neededcolumns(opafit$pp[1])
            times <- log(x$fit[[i]]$data$time)
            ranks <- log(qweibull(x$fit[[i]]$data$rank,1,1))
            if("xony" %in% tolower(opafit$method.fit)){
                x$fit[[i]]$options$method.fit <- c("rr","xony")
                if(any(c("weibull","weibull2p") %in% tolower(opafit$dist))){
                    atleastonefit <- TRUE
                    x$fit[[i]]$options$dist <- "weibull2p"
                    x$fit[[i]]$lm  <- lm(times ~ ranks,x$fit[[i]]$data)
                        # TODO: add error checking
                    x$fit[[i]]$beta <- 1/coef(x$fit[[i]]$lm)[[2]]
                    x$fit[[i]]$eta  <- exp(coef(x$fit[[i]]$lm)[[1]])
                }
                if(any(c("weibull-2","weibull2p-2") %in% tolower(opafit$dist))){
                    x$fit[[i]]$options$dist <- "weibull2p-2"
                    try(ret <- .Call("MRRw2pXonY",
                        x$fit[[i]]$data$time,
                        x$fit[[i]]$data$event,
                        method=1, PACKAGE= "pivotals")
                        # TODO: method=1 -> exact, method=0 -> bernard
                    )
                    if(!is.null(ret)){
                        atleastonefit       <- TRUE
                        x$fit[[i]]$eta      <- ret[[1]]
                        x$fit[[i]]$beta     <- ret[[2]]
                        x$fit[[i]]$gof      <- list()
                        x$fit[[i]]$gof$r2   <- ret[[3]]
                    }else{
                        warning("calculateSingleFit: Fitting failed.")
                    }
                }
            }
            if("yonx" %in% tolower(opafit$method.fit)){
                atleastonefit <- TRUE
                x$fit[[i]]$options$method.fit <- c("rr","yonx")
                x$fit[[i]]$options$dist <- "weibull2p"
                x$fit[[i]]$lm  <- lm(ranks ~ times,x$fit[[i]]$data)
                x$fit[[i]]$beta <- coef(x$fit[[i]]$lm)[[2]]
                x$fit[[i]]$eta  <-
                    exp(-coef(x$fit[[i]]$lm)[[1]]/coef(x$fit[[i]]$lm)[[2]])
            }
            x$fit[[i]]$gof <- list()
            x$fit[[i]]$gof$r2 <- cor(times, ranks, use = "complete.obs")^2
            atleastonefit <- TRUE
            if(identical(x$fit[[i]]$gof$r2,1)){
                if(opafit$verbosity >= 2)message("calculateSingleFit ",
                    ": r^2 is exactly 1, bypassing prr and ccc^2 calculations...")
                    x$fit[[i]]$gof$prr <- Inf
            }else{
                if(opafit$verbosity >= 2)message("calculateSingleFit",
                    ": r^2 is lower than 1 ...")
                x$fit[[i]]$gof$S <- opafit$S
                # TODO: $S should be moved to a better location in the frame
                if(opafit$verbosity >= 2)message("calculateSingleFit",
                    ": Calculating prr and ccc^2...")
                prrval <- .Call("pivotalMCw2p",
                    # TODO: takes into account xony and yonx?
                    na.omit(x$fit[[i]]$data$rank),
                    c(x$fit[[i]]$gof$r2,0.0,1.0,1.0),S=x$fit[[i]]$gof$S,
                    seed=sample.int(.Machine$integer.max,1),
                    Bval=0.5,ProgRpt=FALSE,PACKAGE= "pivotals")
                x$fit[[i]]$gof$prr <- prrval[[1]]
                x$fit[[i]]$gof$ccc2 <- prrval[[2]]
            }
        }
        if(tolower(opafit$dist) %in% c("lognormal","lognormal2p",
            "lognormal-2","lognormal2p-2")){
            debug1()
#            message("EXPERIMENTAL CODE! -> NEEDS TO BE VERIFIED!")
            x$fit[[i]]$data <- neededcolumns(opafit$pp[[1]])
            times <- log(x$fit[[i]]$data$time)
            ranks <- log(qlnorm(x$fit[[i]]$data$rank, 0, 1))
            if("xony" %in% tolower(opafit$method.fit) &&
                any(c("lognormal","lognormal2p") %in% tolower(opafit$dist))){
                atleastonefit <- TRUE
                x$fit[[i]]$options$dist <- "lognormal2p"
                x$fit[[i]]$options$method.fit <- c("rr","xony")
                x$fit[[i]]$lm  <- lm(times ~ ranks,x$fit[[i]]$data)
                x$fit[[i]]$meanlog  <- coef(x$fit[[i]]$lm)[[1]]
                x$fit[[i]]$sdlog    <- coef(x$fit[[i]]$lm)[[2]]
            }
            if("yonx" %in% tolower(opafit$method.fit)){
                if(any(c("lognormal","lognormal2p") %in% tolower(opafit$dist))){
                    atleastonefit <- TRUE
                    x$fit[[i]]$options$dist <- "lognormal2p"
                    x$fit[[i]]$options$method.fit <- c("rr","yonx")
                    x$fit[[i]]$lm  <- lm(ranks ~ times,x$fit[[i]]$data)
                    x$fit[[i]]$meanlog  <- -coef(x$fit[[i]]$lm)[[1]]/
                        coef(x$fit[[i]]$lm)[[2]]
                    x$fit[[i]]$sdlog    <- 1/coef(x$fit[[i]]$lm)[[2]]
                }
                if(any(c("lognormal-2","lognormal2p-2") %in% tolower(opafit$dist))){
                    x$fit[[i]]$options$dist <- "lognormal2p-2"
                    x$fit[[i]]$options$method.fit <- c("rr","yonx")
                    try(ret <- .Call("MRRln2pYonX",
                        x$fit[[i]]$data$time,
                        x$fit[[i]]$data$event,
                        method=1, PACKAGE= "pivotals")
                        # TODO: method=1 -> exact, method=0 -> bernard
                    )
                    if(!is.null(ret)){
                        atleastonefit <- TRUE
                        x$fit[[i]]$meanlog  <- ret[[1]]
                        x$fit[[i]]$sdlog    <- ret[[2]]
                        x$fit[[i]]$gof      <- list()
                        x$fit[[i]]$gof$r2   <- ret[[3]]
                    }else{
                        warning("calculateSingleFit: Fitting failed.")
                    }
                }
            }
            x$fit[[i]]$gof <- list()
            x$fit[[i]]$gof$r2 <- cor(ranks, times, use = "complete.obs")^2
            atleastonefit <- TRUE
            if(identical(x$fit[[i]]$gof$r2,1)){
                if(opafit$verbosity >= 2)message("calculateSingleFit",
                    ": r^2 is exactly 1, bypassing prr and ccc^2 calculations...")
                    x$fit[[i]]$gof$prr <- Inf
            }else{
                if(opafit$verbosity >= 2)message("calculateSingleFit",
                    ": r^2 is lower than 1 ...")
                x$fit[[i]]$gof$S <- opafit$S
                # TODO: $S should be moved to a better location in the frame
                if(opafit$verbosity >= 2)message("calculateSingleFit",
                    ": Calculating prr and ccc^2...")
                prrval <- .Call("pivotalMCln2p",
                    na.omit(x$fit[[i]]$data$rank),
                    c(x$fit[[i]]$gof$r2,0.0,1.0,1.0),S=x$fit[[i]]$gof$S,
                    seed=sample.int(.Machine$integer.max,1),
                    Bval=0.5,ProgRpt=FALSE,PACKAGE= "pivotals")
                x$fit[[i]]$gof$prr <- prrval[[1]]
                x$fit[[i]]$gof$ccc2 <- prrval[[2]]
            }
        }
        if("xony" %in% tolower(opafit$method.fit)){
            if(tolower(opafit$dist) %in% "weibull3p"){
                #prepfitlist()
                debug1()
                x$fit[[i]]$options$dist <- "weibull3p"
                x$fit[[i]]$options$method.fit <- c("rr","xony")
                x$fit[[i]]$data <- neededcolumns(opafit$pp[1])
                ret <- NULL
                fa <- x$fit[[i]]$data$time[x$fit[[i]]$data$event==1]
                su <- x$fit[[i]]$data$time[x$fit[[i]]$data$event==0]
                debug1()
                try(ret <- MRRw3pxy(fa,s=su))
                if(!is.null(ret)){
                    atleastonefit       <- TRUE
                    x$fit[[i]]$beta     <- ret[[2]]
                    x$fit[[i]]$eta      <- ret[[1]]
                    x$fit[[i]]$t0       <- ret[[3]]
                    x$fit[[i]]$gof      <- list()
                    x$fit[[i]]$gof$r2   <- ret[[4]]
                }else{
                    warning("calculateSingleFit: Fitting failed.")
                }
            }
        }
    }
    if(any(c("mle","mle-rba") %in% tolower(opafit$method.fit))){
        # +----------------------+
        # |  Maximum Likelihood  |
        # +----------------------+
        if(tolower(opafit$dist) %in% c("weibull","weibull2p")){
            #prepfitlist()
            debug2()
            x$fit[[i]]$data <- neededcolumns()
            fa <- x$fit[[i]]$data$time[x$fit[[i]]$data$event==1]
            su <- x$fit[[i]]$data$time[x$fit[[i]]$data$event==0]
            try(ret <- MLEw2p_abrem(fa,s=su))
            if(!is.null(ret)){
                atleastonefit <- TRUE
                x$fit[[i]]$options$dist <- "weibull2p"
                x$fit[[i]]$options$method.fit <- "mle"
                x$fit[[i]]$beta <- ret[[2]]
                x$fit[[i]]$eta  <- ret[[1]]
                x$fit[[i]]$gof <- list()
                x$fit[[i]]$gof$loglik <- ret[[3]]
                if("mle-rba" %in% tolower(opafit$method.fit)){
                    x$fit[[i]]$options$method.fit <- "mle-rba"
                    if(opafit$verbosity >= 2)message("calculateSingleFit",
                    ": Applying Abernethy's Median Bias Reduction ...")
                    x$fit[[i]]$beta <- ret[[2]]*debias::RBAw(length(fa))
                }
            }else{
                warning("calculateSingleFit: Fitting failed.")
            }
        }
        if(tolower(opafit$dist) %in% c("weibull3p")){
            #prepfitlist()
            debug2()
            x$fit[[i]]$data <- neededcolumns()
            fa <- x$fit[[i]]$data$time[x$fit[[i]]$data$event==1]
            su <- x$fit[[i]]$data$time[x$fit[[i]]$data$event==0]
            try(ret <- MLEw3p_secant(fa,s=su))
            if(!is.null(ret)){
                atleastonefit <- TRUE
                x$fit[[i]]$options$dist <- "weibull3p"
                x$fit[[i]]$options$method.fit <- "mle"
                x$fit[[i]]$beta <- ret[[2]]
                x$fit[[i]]$eta  <- ret[[1]]
                x$fit[[i]]$t0   <- ret[[3]]
                x$fit[[i]]$gof  <- list()
                x$fit[[i]]$gof$loglik <- ret[[4]]
                # TODO: here, ret[[4]] is positive LL?
#                if("mle-rba" %in% tolower(opafit$method.fit)){
                    # TODO: check if this is appropriate with WB3P
#                    x$fit[[i]]$options$method.fit <- "mle-rba"
#                    if(opafit$verbosity >= 2)message("calculateSingleFit",
#                    ": Applying Abernethy's Median Bias Reduction ...")
#                    x$fit[[i]]$beta <- ret[[2]]*debias::RBAw(length(fa))
#                }
            }else{
                warning("calculateSingleFit: Fitting failed.")
            }
        }
        
    }
    if(!atleastonefit){
        message("*** calculateSingleFit: Nothing has been fitted. ***\n",
        "    Does \"method.fit\" include sensible options?")
        # x$fit[[i]] <- NULL
    }
    x
    # return a single Abrem object
}