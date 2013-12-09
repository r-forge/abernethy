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
    # TODO: WAY to much duplicated code!
    #########################
    #  auxiliary functions  #
    #########################
    opadata <- x$options
    opafit <- modifyList(opadata,list(...))
    vm <- function(vlevel,mess)if(opafit$verbosity >= vlevel)message(mess)
    debug1 <- function()vm(2,paste0(
            "calculateSingleFit: Attempting ",opafit$dist," (",
            paste0(opafit$method.fit,collapse=", "),
            "), pp = \"",opafit$pp,"\" fit..."))
    debug2 <- function()vm(2,paste0(
            "calculateSingleFit: Attempting ",opafit$dist," (",
            paste0(opafit$method.fit,collapse=", "),") fit..."))
    #ds <- function(level,mess)if(opafit$verbosity >= level)stop(mess)
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
    goodness_of_fit <- function(){
        if(is.null(x$fit[[i]]$gof)){
            vm(2,"calculateSingleFit: calculating r^2 using cor()...")
            x$fit[[i]]$gof <<- list()
            x$fit[[i]]$gof$r2 <<- cor(times, ranks, use = "complete.obs")^2
        }else vm(2,"calculateSingleFit: r^2 was already set...")
            # if !NULL, then it was already set by the cpp version of the fitting method

        if(identical(x$fit[[i]]$gof$r2,1)){
            vm(2,"calculateSingleFit: r^2 is exactly 1, bypassing prr and ccc^2 calculations...")
                x$fit[[i]]$gof$prr <<- Inf
        }else{
            vm(2,"calculateSingleFit: r^2 is lower than 1 ...")
            x$fit[[i]]$gof$S <<- opafit$S
                # TODO: $S should be moved to a better location in the frame
            vm(2,"calculateSingleFit: Calculating prr and ccc^2...")
            prrval <- .Call("pivotalMCw2p",
                # TODO: takes into account xony and yonx?
                na.omit(x$fit[[i]]$data$rank),
                c(x$fit[[i]]$gof$r2,0.0,1.0,1.0),S=x$fit[[i]]$gof$S,
                seed=sample.int(.Machine$integer.max,1),
                Bval=0.5,ProgRpt=FALSE,PACKAGE= "pivotals")
            x$fit[[i]]$gof$prr  <<- prrval[[1]]
            x$fit[[i]]$gof$ccc2 <<- prrval[[2]]
        }
    }
    
    ########################
    #  main function body  #
    ########################
    i <- 1
    atleastonefit <- FALSE
    if(is.null(x$fit)){
        vm(2,"calculateSingleFit: Creating the first fit in the abrem object...")
        i <- 1
        x$fit <- list()
    }else{
        vm(2,"calculateSingleFit: Appending a new fit to the existing abrem object...")
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
        #  ____             _                                       _
        # |  _ \ __ _ _ __ | | __  _ __ ___  __ _ _ __ ___  ___ ___(_) ___  _ __
        # | |_) / _` | '_ \| |/ / | '__/ _ \/ _` | '__/ _ \/ __/ __| |/ _ \| '_ \
        # |  _ < (_| | | | |   <  | | |  __/ (_| | | |  __/\__ \__ \ | (_) | | | |
        # |_| \_\__,_|_| |_|_|\_\ |_|  \___|\__, |_|  \___||___/___/_|\___/|_| |_|
        #                                   |___/
        if(tolower(opafit$dist) %in% c("weibull","weibull2p","weibull-2","weibull2p-2")){
            # __        __   _ _           _ _
            # \ \      / /__(_) |__  _   _| | |
            #  \ \ /\ / / _ \ | '_ \| | | | | |
            #   \ V  V /  __/ | |_) | |_| | | |
            #    \_/\_/ \___|_|_.__/ \__,_|_|_|
            #prepfitlist()
            debug1()
            x$fit[[i]]$data <- neededcolumns(opafit$pp[1])
            times <- log(x$fit[[i]]$data$time)
            ranks <- log(qweibull(x$fit[[i]]$data$rank,1,1))
            rr_weibull2p <- function(is_xony){
                x$fit[[i]]$options$method.fit <<- c("rr",ifelse(is_xony,"xony","yonx"))
                if(any(c("weibull","weibull2p") %in% tolower(opafit$dist))){
                    atleastonefit <<- TRUE
                    x$fit[[i]]$options$dist <<- "weibull2p"
                    if(is_xony) x$fit[[i]]$lm  <<- lm(times ~ ranks,x$fit[[i]]$data)
                    else        x$fit[[i]]$lm  <<- lm(ranks ~ times,x$fit[[i]]$data)
                        # TODO: add error checking
                    B <- coef(x$fit[[i]]$lm)[[2]]
                    A <- coef(x$fit[[i]]$lm)[[1]]
                    x$fit[[i]]$beta <<- ifelse(is_xony,1/B,B)
                    x$fit[[i]]$eta  <<- ifelse(is_xony,exp(A),exp(-A/B))
                }
                if(any(c("weibull-2","weibull2p-2") %in% tolower(opafit$dist))){
                    x$fit[[i]]$options$dist <<- "weibull2p-2"
                    ret <- NULL
                    try(ret <- .Call(ifelse(is_xony,"MRRw2pXonY","MRRw2pYonX"),
                        x$fit[[i]]$data$time,
                        x$fit[[i]]$data$event,
                        method=1, PACKAGE= "pivotals"))
                        # TODO: method=1 -> exact, method=0 -> bernard
                    if(!is.null(ret)){
                        atleastonefit       <<- TRUE
                        x$fit[[i]]$eta      <<- ret[[1]]
                        x$fit[[i]]$beta     <<- ret[[2]]
                        x$fit[[i]]$gof      <<- list()
                        x$fit[[i]]$gof$r2   <<- ret[[3]]
                    }#else warning("calculateSingleFit: Fitting failed.")
                }
            }
            if("xony" %in% tolower(opafit$method.fit)) rr_weibull2p(TRUE)
            if("yonx" %in% tolower(opafit$method.fit)) rr_weibull2p(FALSE)
                # TODO: MRRw2pYonX does not exists, so an error will be generated.
                # TODO: this is also called when x$fit[[i]]$options$dist <- "weibull2p-2"
            goodness_of_fit()
        }
        if(tolower(opafit$dist) %in% c("lognormal","lognormal2p","lognormal-2","lognormal2p-2")){
            #  _                                                 _
            # | |    ___   __ _ _ __   ___  _ __ _ __ ___   __ _| |
            # | |   / _ \ / _` | '_ \ / _ \| '__| '_ ` _ \ / _` | |
            # | |__| (_) | (_| | | | | (_) | |  | | | | | | (_| | |
            # |_____\___/ \__, |_| |_|\___/|_|  |_| |_| |_|\__,_|_|
            #             |___/
            debug1()
#            message("EXPERIMENTAL CODE! -> NEEDS TO BE VERIFIED!")
            x$fit[[i]]$data <- neededcolumns(opafit$pp[[1]])
            times <- log(x$fit[[i]]$data$time)
            ranks <- log(qlnorm(x$fit[[i]]$data$rank, 0, 1))

            rr_lognormal2p <- function(is_xony){
                x$fit[[i]]$options$method.fit <<- c("rr",ifelse(is_xony,"xony","yonx"))
                if(any(c("lognormal","lognormal2p") %in% tolower(opafit$dist))){
                    atleastonefit <<- TRUE
                    x$fit[[i]]$options$dist <<- "lognormal2p"
                    
                    if(is_xony) x$fit[[i]]$lm  <<- lm(times ~ ranks,x$fit[[i]]$data)
                    else        x$fit[[i]]$lm  <<- lm(ranks ~ times,x$fit[[i]]$data)
                    A <- coef(x$fit[[i]]$lm)[[1]]
                    B <- coef(x$fit[[i]]$lm)[[2]]
                    if(is_xony){
                        x$fit[[i]]$meanlog  <<- A
                        x$fit[[i]]$sdlog    <<- B
                    }else{
                        x$fit[[i]]$meanlog  <<- -A/B
                        x$fit[[i]]$sdlog    <<- 1/B
                    }
                }
                if(any(c("lognormal-2","lognormal2p-2") %in% tolower(opafit$dist))){
                    x$fit[[i]]$options$dist <<- "lognormal2p-2"
                    ret <- NULL
                    try(ret <- .Call(ifelse(is_xony,"MRRln2pXonY","MRRln2pYonX"),
                        x$fit[[i]]$data$time,
                        x$fit[[i]]$data$event,
                        method=1, PACKAGE= "pivotals"))
                        # TODO: method=1 -> exact, method=0 -> bernard
                    if(!is.null(ret)){
                        atleastonefit       <<- TRUE
                        x$fit[[i]]$meanlog  <<- ret[[1]]
                        x$fit[[i]]$sdlog    <<- ret[[2]]
                        x$fit[[i]]$gof      <<- list()
                        x$fit[[i]]$gof$r2   <<- ret[[3]]
                    }#else warning("calculateSingleFit: Fitting failed.")
                }
            }
            if("xony" %in% tolower(opafit$method.fit)) rr_lognormal2p(TRUE)
            if("yonx" %in% tolower(opafit$method.fit)) rr_lognormal2p(FALSE)
            goodness_of_fit()
        }
        if(tolower(opafit$dist) %in% "weibull3p"){
            # __        __   _ _           _ _ _____
            # \ \      / /__(_) |__  _   _| | |___ / _ __
            #  \ \ /\ / / _ \ | '_ \| | | | | | |_ \| '_ \
            #   \ V  V /  __/ | |_) | |_| | | |___) | |_) |
            #    \_/\_/ \___|_|_.__/ \__,_|_|_|____/| .__/
            #                                       |_|
            rr_weibull3p <- function(is_xony){
                x$fit[[i]]$options$method.fit <<- c("rr",ifelse(is_xony,"xony","yonx"))
                #prepfitlist()
                debug1()
                x$fit[[i]]$options$dist <<- "weibull3p"
                x$fit[[i]]$data <<- neededcolumns(opafit$pp[1])
                ret <- NULL
                try(ret <- .Call(ifelse(is_xony,"MRRw3pXonY","MRRw3pYonX"),
                    x$fit[[i]]$data$time,
                    x$fit[[i]]$data$event,limit = 1e-5, PACKAGE= "pivotals"))
                    ## TODO: incorporate LIMIT argument in another way
                if(!is.null(ret)){
                    atleastonefit       <<- TRUE
                    x$fit[[i]]$beta     <<- ret[[2]]
                    x$fit[[i]]$eta      <<- ret[[1]]
                    x$fit[[i]]$t0       <<- ret[[3]]
                    x$fit[[i]]$gof      <<- list()
                    x$fit[[i]]$gof$r2   <<- ret[[4]]
                }#else warning("calculateSingleFit: Fitting failed.")
            }
            if("xony" %in% tolower(opafit$method.fit)) rr_weibull3p(TRUE)
            if("yonx" %in% tolower(opafit$method.fit))
                vm(0,"calculateSingleConf: Currently, c(\"rr\", \"yonx\") for three-parameter Weibull is not supported.")
        }
    }
    if(any(c("mle","mle-rba") %in% tolower(opafit$method.fit))){
        #  __  __ _     _____
        # |  \/  | |   | ____|
        # | |\/| | |   |  _|
        # | |  | | |___| |___
        # |_|  |_|_____|_____|

        debug2()
        x$fit[[i]]$data <- neededcolumns()
        x$fit[[i]]$options$method.fit <- "mle"
        fa <- x$fit[[i]]$data$time[x$fit[[i]]$data$event==1]
        su <- x$fit[[i]]$data$time[x$fit[[i]]$data$event==0]
        ret <- NULL
        is_3p <- FALSE
        if(tolower(opafit$dist) %in% c("weibull","weibull2p")){
            x$fit[[i]]$options$dist <- "weibull2p"
            try(ret <- debias::MLEw2p_abrem(fa,s=su))
        }
        if(tolower(opafit$dist) %in% c("weibull3p")){
            is_3p <- TRUE
            x$fit[[i]]$options$dist <- "weibull3p"
            try(ret <- debias::MLEw3p_secant(fa,s=su))
        }
        if(!is.null(ret)){
            atleastonefit <- TRUE
            x$fit[[i]]$beta <- ret[[2]]
            x$fit[[i]]$eta  <- ret[[1]]
            if(is_3p){
                x$fit[[i]]$t0   <- ret[[3]]
                x$fit[[i]]$gof  <- list()
                x$fit[[i]]$gof$loglik <- ret[[4]]
            }else{
                x$fit[[i]]$gof <- list()
                x$fit[[i]]$gof$loglik <- ret[[3]]
            }
            if("mle-rba" %in% tolower(opafit$method.fit)){
                x$fit[[i]]$options$method.fit <- "mle-rba"
                vm(2,"calculateSingleFit: Applying Abernethy's Bias Reduction ...")
                x$fit[[i]]$beta <- ret[[2]]*debias::RBAbeta(length(fa))
                    # TODO: set the option: median or mean bias reduction
            }
        }#else warning("calculateSingleFit: Fitting failed.")

        if(tolower(opafit$dist) %in% c("lognormal","lognormal2p")){
            x$fit[[i]]$options$dist <- "lognormal2p"
            try(ret <- debias::MLEln2p_cpp(fa,s=su))
            if(!is.null(ret)){
                atleastonefit <- TRUE
                x$fit[[i]]$meanlog  <- ret[[1]]
                x$fit[[i]]$sdlog    <- ret[[2]]
                x$fit[[i]]$gof      <- list()
                x$fit[[i]]$gof$loglik <- ret[[3]]
                if("mle-rba" %in% tolower(opafit$method.fit)){
                    x$fit[[i]]$options$method.fit <- "mle-rba"
                    vm(2,"calculateSingleFit: Applying Abernethy's Median Bias Reduction ...")
                    x$fit[[i]]$sdlog <- ret[[2]]*debias::RBAsigma(length(fa))
                        # with RBAsigma, there are no options...
                }
            }#else warning("calculateSingleFit: Fitting failed.")
        }
    }
    if(!atleastonefit){
        message("*** calculateSingleFit: Nothing has been fitted.  ***\n",
                "*** Does \"method.fit\" include sensible options?   ***")
        # x$fit[[i]] <- NULL
    }
    x
    # return a single Abrem object
}