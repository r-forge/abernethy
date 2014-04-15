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

calculateSingleConf <- function(fit,opadata,datarange,...){
    # fit is a single fit
    arg <- list(...)
    if(!is.null(list(...)$threshold))
        message("calculateSingleConf: Currently, passing the \'threshold\' argument to abrem.conf  is not supported. Proceeding...")
    if(missing(fit)){
        stop("Argument \"fit\" is missing.")
    }else{
#            if(!is.null(abrem$fit) && any(sapply(abrem$fit,function(fi)!is.null(fi)))){
        if(!is.null(fit) && any(sapply(fit,function(fi)!is.null(fi)))){
            if(!is.null(fit$options))
                opafit <- modifyList(opadata,fit$options)
            opafit$importfile <- NULL
                # never use the importfile from abrem.fit; only use it when
                # explicitly supplied as a function argument
            opaconf <- modifyList(opafit,arg)
            if(!is.null(fit$options$dist)) {
                if(tolower(fit$options$dist) %in% c("weibull","weibull2p")){
                    if(is.null(fit$beta) || is.null(fit$eta)){
                        stop("Beta and/or Eta are not provided.")
                    }else{
                        if(opaconf$verbosity >= 1)
                            message("calculateSingleConf: Found Weibull 2P distribution.")
                        if("blives" %in% tolower(opaconf$conf.what)){
                            if(opaconf$verbosity >= 1)
                                message("calculateSingleConf: Calculating B-lives confidence bounds.")
                            mini <- min(c(opaconf$ylim[1]/10,datarange$yrange[1]/10),0.001)
                            maxi <- max(c((1-(1-opaconf$ylim[2])/10),
                                (1-(1-datarange$yrange[2])/10),0.999))
                            unrel <- c(F0(seq(F0inv(mini),F0inv(maxi),
                                    # TODO: this isn't right...
    #                        unrel <- c(F0(seq(F0inv(1e-3),
    #                            F0inv(0.999),
                                length.out=opaconf$unrel.n -
                                length(opaconf$unrel+2))),
                                opaconf$unrel,0.5,F0(0))
                                # effectively ignoring any ylim
                                # setting per fit.
                            unrel <- unique(signif(unrel[order(unrel)]))
                                # signif() needed for eliminating
                                # identical looking unreliability
                                # levels that differ only at place far
                                # from the decimal point
                                # unrel <- c(F0(seq(par('usr')[3],par('usr')[4],
                            if(is.null(fit$conf)){
                                fit$conf <- list()
    #                            if(opaconf$verbosity >= 2)message(
    #                                "calculateSingleConf: Creating the first ",
    #                                "B-life confidence calculation in the fit...")
    #                            i <- 1
    #                            fit$conf <- list()
                            }
                            atLeastOneBLifeConf <- FALSE
                            if(is.null(fit$conf$blives)){
                                if(opaconf$verbosity >= 2)message(
                                    "calculateSingleConf: Creating the first ",
                                    "B-life confidence calculation in the fit...")
                                i <- 1
                                fit$conf$blives <- list()
                            }else{
                                if(opaconf$verbosity >= 2)message(
                                    "calculateSingleConf: Appending a new ",
                                    "B-life confidence calculation to the fit...")
                                i <- length(fit$conf$blives)+1
                            }
                            fit$conf$blives[[i]] <- list()
                            
                            
                            
                            if(!is.null(opaconf$importfile)){
                                #                            ____  __  __ ___ _____ _   _
                                #  ___ _   _ _ __   ___ _ __/ ___||  \/  |_ _|_   _| | | |
                                # / __| | | | '_ \ / _ \ '__\___ \| |\/| || |  | | | |_| |
                                # \__ \ |_| | |_) |  __/ |   ___) | |  | || |  | | |  _  |
                                # |___/\__,_| .__/ \___|_|  |____/|_|  |_|___| |_| |_| |_|
                                #           |_|
                                if(opaconf$verbosity >= 1)message("calculateSingleConf : ",
                                    "importing confidence bounds from superSMITH report file:\n",opaconf$importfile)
                                try(fi <- file(opaconf$importfile))
                                if(!is.null(fi)){
                                    try(re <- readLines(fi))
                                    if(!is.null(re)){
                                        #message("MATCH OUTPUT:")
                            #                print(re)
                                        atLeastOneBLifeConf <- TRUE
                                        fit$conf$blives[[i]]$type   <- "superSMITH"
                                        fit$conf$blives[[i]]$source  <- re
                                        #fit$conf$blives[[i]]$S      <- opaconf$S
                                        #fit$conf$blives[[i]]$seed   <- opaconf$seed
                                        #fit$conf$blives[[i]]$rgen   <- opaconf$rgen
                                        #fit$conf$blives[[i]]$cl     <- opaconf$cl
                                        #fit$conf$blives[[i]]$sides  <- opaconf$conf.blives.sides
                                        #fit$conf$blives[[i]]$unrel <- opaconf$unrel
                                        extract <- function(string)
                                            na.omit(as.numeric(unlist(strsplit(gsub(",",".",string)," "))))
                                        bounds <- data.frame(do.call("rbind",lapply(re[12:length(re)],extract)))
                                        bounds[,1] <- bounds[,1]/100
                                        names(bounds) <- c("unrel","Lower","Datum", "Upper")
                                        fit$conf$blives[[i]]$bounds <- bounds
                                    }
                                    close(fi)
                                }
                                op <- unique(c(names(opafit),names(opaconf)))
                                if(length(li <- opaconf[sapply(op,function(y){
                                    !identical(opafit[[y]], opaconf[[y]])})]) > 0){
                                    fit$conf$blives[[i]]$options <- li
                                }
                                return(fit)
                            }else{
                                if("bbb" %in% tolower(opaconf$method.conf.blives)){
                                    #  ____  ____  ____
                                    # | __ )| __ )| __ )
                                    # |  _ \|  _ \|  _ \
                                    # | |_) | |_) | |_) |
                                    # |____/|____/|____/
    
                                    if(opaconf$verbosity >= 1)message(
                                        "calculateSingleConf: Calculating bbb confidence bounds.")
                                    ### bbb is unsupported as long as adjusted ranks are unavailable
                                    fit$conf$blives[[i]]$type <- "bbb"
                                    fit$conf$blives[[i]]$cl <- opaconf$cl
                                    fit$conf$blives[[i]]$sides <- opaconf$conf.blives.sides
    
                                    ### calculate adjusted ranks, just for these BB bounds
                                    sx <- fit$data
                                    if(is.null(fit$data$rank)){
                                        # no ranks available, likely because the fit was mle
                                        if("median" %in% opaconf$pp) ty <- "medianRank1"
                                        if("benard" %in% opaconf$pp) ty <- "medianRank"
                                        if(opaconf$verbosity >= 1)
                                            message('calculateSingleConf: creating ranks for calculating \"bbb\" bounds...')
                                        ra <- .Call(ty,fit$data$event, PACKAGE= "pivotals")
                                        sx$rank[sx$event] <- ra
                                            # this assumes fit$data and thus sx is ordered
                                            # TODO: shouldn't I use the ranks that are part of the
                                            # data since they will ALWAYS be part of
                                            # an abrem object?
                                            # 
                                        # TODO: makes sense to have BBB bounds with mle?
                                    }
                                    sx <- sx[order(sx$rank),]
                                        # order data according to rank
                                    sx <- cbind(sx,arank=NA)
                                    sx$rrank <- (fit$n+1-order(sx$rank))
                                        # TODO: does order() completely replace x$rank? (NA?)
                                        # reverse rank order
                                        # TODO: keep the rrank and arank in fit$data or discard?
                                    parank <- 0
                                    for (j in 1:fit$n){
                                        if(!sx$event[j] || is.null(sx$event)){
                                            sx$arank[j] <- NA
                                        }else{
                                            sx$arank[j] <- (sx$rrank[j]*parank + fit$n +1)/(sx$rrank[j]+1)
                                            parank <- sx$arank[j]
                                        }
                                        # adjusted_rank =
                                        # (reversed rank * previous adj. rank + n + 1)/(reversed rank + 1)
                                        # see "The new Weibull handbook, fifth edition" p. 2-7, formula 2-5
                                    }
                                    da <- data.frame(
                                        unrel= sx$rank,
                                        Lower= bbb(sx$arank,fit$n,(1-opaconf$cl)/2,fit$beta,fit$eta),
                                        Upper= bbb(sx$arank,fit$n,1-(1-opaconf$cl)/2,fit$beta,fit$eta))
                                    lo <- approxfun(F0inv(sx$rank),log(da$Lower))
                                    up <- approxfun(F0inv(sx$rank),log(da$Upper))
                                    bl <- F0inv(unrel)
                                    da <- rbind(da,data.frame(unrel=unrel,Lower=exp(lo(bl)),Upper=exp(up(bl))))
                                        # TODO: warning: bounds don't look correct, has to do with F0inv and F0
                                    da <- da[order(da$unrel),]
                                    da <- da[!duplicated(da$unrel),]
                                    fit$conf$blives[[i]]$bounds <- da
                                    op <- unique(c(names(opafit),names(opaconf)))
                                        # this is needed to add options from opafit into li that
                                        # are NULL in opafit
                                        # TODO:tolower() not needed?
                                    if(length(li <- opaconf[sapply(op,function(y){
                                        !identical(opafit[[y]], opaconf[[y]])})]) > 0){
                                        fit$conf$blives[[i]]$options <- li
                                    }
                                }
                                if("lrb" %in% tolower(opaconf$method.conf.blives)){
                                    #  _     ____  ____
                                    # | |   |  _ \| __ )
                                    # | |   | |_) |  _ \
                                    # | |___|  _ <| |_) |
                                    # |_____|_| \_\____/
    
                                    if(opaconf$verbosity >= 1)message(
                                        "calculateSingleConf: Calculating Likelihood Ratio confidence bounds.")
                                    fail <- fit$data$time[fit$data$event==1]
                                    susp <- fit$data$time[fit$data$event==0]
                                        # this implies only right censoring !
                                    fit$conf$blives[[i]]        <- list()
                                    fit$conf$blives[[i]]$type   <- "lrb"
                                    fit$conf$blives[[i]]$cl     <- opaconf$cl
                                    fit$conf$blives[[i]]$sides  <- opaconf$conf.blives.sides
                                    fit$conf$blives[[i]]$unrel <- opaconf$unrel
                                    is_debias <- ifelse("mle" %in% tolower(fit$options$method.fit),FALSE,TRUE)
                                    ret <- NULL; con <- NULL
                                    try(con <- debias::MLEw2pContour(fail,susp,CL=opaconf$cl,debias=is_debias,show=FALSE))
                                    if(!is.null(con)){
                                        fit$conf$blives[[i]]$MLEXContour <- list()
                                        fit$conf$blives[[i]]$MLEXContour[[1]] <- con
                                        
                                        retfit <- abrem.fit(Abrem(fail=fail,susp=susp),method.fit=ifelse(is_debias,"mle-rba","mle"))
                                        fit$conf$blives[[i]]$MLEXContour$MLEpoint <-
                                            data.frame(Eta=retfit$fit[[1]]$eta,Beta=retfit$fit[[1]]$beta)
        #                                MLEpoint <- data.frame(Eta=fit$Eta,Beta=fit$Beta)
        #                                fit$conf$blives[[i+1]] <- list()
        #                                fit$conf$blives[[i+1]]$MLEXContour <-
        #                                    list(Upper=MLEpoint,Lower=MLEpoint,Right=MLEpoint,Left=MLEpoint)
        #                                    # TODO: this hack should produce a point at the MLE location
                                        try(ret <- debias::MLEw2pBounds(fail,susp,Blives=unrel,MLEcontour=con,debias=is_debias,show=FALSE))
                                            # debias is true in all cases except for regulaer MLE
                                        if(!is.null(ret)){
                                            atLeastOneBLifeConf <- TRUE
                                            fit$conf$blives[[i]]$bounds <- cbind(unrel,exp(ret[,-1]))
                                        }
                                    }
                                        # TODO: request Jacob Odmerod for cleaning up his return dataframe
                                        # UPDATE: possible in debias >= 0.1.9, the dataframe is quite sane ->
                                        # eliminate above comment
                                    op <- unique(c(names(opafit),names(opaconf)))
                                        # this is needed to add options from opafit into li that
                                        # are NULL in opafit
                                        # TODO:tolower() not needed?
                                    if(length(li <- opaconf[sapply(op,function(y){
                                        !identical(opafit[[y]], opaconf[[y]])})]) > 0){
                                        fit$conf$blives[[i]]$options <- li
                                    }
                                }
                                if(any(c("mcpivotals","mcpivotal") %in% tolower(opaconf$method.conf.blives))){
                                    #                       _            _        _
                                    #  _ __ ___   ___ _ __ (_)_   _____ | |_ __ _| |___
                                    # | '_ ` _ \ / __| '_ \| \ \ / / _ \| __/ _` | / __|
                                    # | | | | | | (__| |_) | |\ V / (_) | || (_| | \__ \
                                    # |_| |_| |_|\___| .__/|_| \_/ \___/ \__\__,_|_|___/
                                    #                |_|
    
                                    if(opaconf$verbosity >= 1)message(
                                        "calculateSingleConf: Calculating Monte Carlo Pivotal confidence bounds.")
        #                            i <- length(fit$conf$blives)
        #                            if(length(fit$conf$blives[[i]]) > 0){
        #                                i <- i+1
        #                            }
                                    dx <- params.to.ob("weibull",beta=1,eta=1,
                                        event=fit$data$event)
                                    r1 <- abrem.fit(Abrem(dx[dx$event==1,]),dist=fit$options$dist,
                                        method.fit=fit$options$method.fit)
                                        # TODO: what happens whent the above are NULL?
                                    fit$conf$blives[[i]]        <- list()
                                    fit$conf$blives[[i]]$type   <- "mcpivotals"
                                    fit$conf$blives[[i]]$S      <- opaconf$S
                                    fit$conf$blives[[i]]$seed   <- opaconf$seed
                                    fit$conf$blives[[i]]$rgen   <- opaconf$rgen
                                    fit$conf$blives[[i]]$cl     <- opaconf$cl
                                    fit$conf$blives[[i]]$sides  <- opaconf$conf.blives.sides
                                    fit$conf$blives[[i]]$unrel <- opaconf$unrel
                                    ret <- NULL
                                    if(fit$cens != 0){
                                        warning(
                                        "calculateSingleConf: Currently, MC Pivotal bounds for (heavily) censored data\n",
                                        "are still experimental and probably too optimistic.")
                                    }
                                    if(is.null(fit$data$rank)){
                                        message("calculateSingleConf: Currently, only rank regression is supported.")
                                    }else{
                                        try(ret <- .Call("pivotalMCw2p",na.omit(fit$data$rank),
                                            c(R2=0.0,CI=opaconf$cl,Eta=r1$fit[[1]]$eta,
                                            Beta=r1$fit[[1]]$beta),opaconf$S,sample.int(
                                            .Machine$integer.max,1),unrel,FALSE,
                                            PACKAGE = "pivotals"))
                                    }
                                    if(!is.null(ret)){
                                        atLeastOneBLifeConf <- TRUE
                                        fit$conf$blives[[i]]$bounds <- cbind(unrel,
                                            exp(log(fit$eta)+ ret/fit$beta))
                                        names(fit$conf$blives[[i]]$bounds) <- c("unrel","Lower","Datum", "Upper")
                                        op <- unique(c(names(opafit),names(opaconf)))
                                            # this is needed to add options from opafit into li that
                                            # are NULL in opafit
                                            # TODO:tolower() not needed?
                                        if(length(li <- opaconf[sapply(op,function(y){
                                            !identical(opafit[[y]], opaconf[[y]])})]) > 0){
                                            fit$conf$blives[[i]]$options <- li
                                        }
                                    }else{
                                        message("calculateSingleConf: Confidence calculation failed.")
                                        fit$conf$blives[[i]] <- NULL
                                    }
                                }
                                if(any(c("exp1","exp-1") %in% tolower(opaconf$method.conf.blives))){
                                    # experimental R based pivotals code
                                    #                  _
                                    #   _____  ___ __ / |
                                    #  / _ \ \/ / '_ \| |
                                    # |  __/>  <| |_) | |
                                    #  \___/_/\_\ .__/|_|
                                    #           |_|
                                    if(opaconf$verbosity >= 1)message(
                                        "calculateSingleConf: Calculating EXP-1 confidence bounds.")
                                    dx <- params.to.ob("weibull",beta=1,eta=1,
                                        event=fit$data$event)
                                    r1 <- abrem.fit(Abrem(dx[dx$event==1,]),dist=fit$options$dist,
                                        method.fit=fit$options$method.fit)
                                        # TODO: what happens whent the above are NULL?
                                        # TODO: no problems with NA?
                                    fit$conf$blives[[i]]        <- list()
                                    fit$conf$blives[[i]]$type   <- "exp1"
                                    fit$conf$blives[[i]]$S      <- opaconf$S
                                    fit$conf$blives[[i]]$seed   <- opaconf$seed
                                    fit$conf$blives[[i]]$rgen   <- opaconf$rgen
                                    fit$conf$blives[[i]]$cl     <- opaconf$cl
                                    fit$conf$blives[[i]]$sides  <- opaconf$conf.blives.sides
                                    fit$conf$blives[[i]]$unrel <- opaconf$unrel
                                    ret <- NULL
                                    daevent <- fit$data$event
                                    MCfun <- function(){
                                        d2 <- data.frame(time=NA,event=daevent)
                                        d2[daevent == 1,'time'] <- sort(
                                            rweibull(length(daevent[daevent==1]),
                                            r1$fit[[1]]$beta,r1$fit[[1]]$eta))
                                        try(ret <- .Call("MRRw2pXonY",
                                            d2$time,d2$event,
                                            method=1, PACKAGE= "pivotals"))
                                        # TODO: method=1 -> exact, method=0 -> benard
                                        if(!is.null(ret))return(
                                            c(u_hat=log(ret[1]),b_hat=1/ret[2]))
                                        else stop()
                                    }
                                    piv <- as.data.frame(t(replicate(opaconf$S,MCfun())))
                                    wp  <- log(qweibull(unrel,1,1))
                                        #wp  <- F0inv(unrel) # identical as the above line
                                    Zp  <- function(wp)((piv$u_hat-wp)/piv$b_hat)
                                        # calculate the pivotal quantities for each u_hat and b_hat...
                                    piv <- cbind(piv,sapply(wp,Zp))
                                        # ... and add them to the dataframe
                                    names(piv) <- c("u_hat","b_hat",signif(unrel))
    
                                    fit$conf$blives[[i]]$bounds <- data.frame(unrel=unrel,row.names=unrel)
                                    Tp <- function(Zp,cl)exp(log(fit$eta)-quantile(Zp,cl)/fit$beta)
                                    fit$conf$blives[[i]]$bounds <-
                                        cbind(fit$conf$blives[[i]]$bounds,
                                        Lower =sapply(piv[,c(-1,-2)],Tp,1-(1-opaconf$cl)/2),
                                        Datum =sapply(piv[,c(-1,-2)],Tp,0.5),
                                        Upper =sapply(piv[,c(-1,-2)],Tp,(1-opaconf$cl)/2))
                                    op <- unique(c(names(opafit),names(opaconf)))
                                        # this is needed to add options from opafit into li that
                                        # are NULL in opafit
                                        # TODO:tolower() not needed?
                                    if(length(li <- opaconf[sapply(op,function(y){
                                        !identical(opafit[[y]], opaconf[[y]])})]) > 0){
                                        fit$conf$blives[[i]]$options <- li
                                    }
                                }
                            }
                        }
                    }
                }
                if("weibull3p" %in% tolower(fit$options$dist)){
                    if(is.null(fit$beta) || is.null(fit$eta) || is.null(fit$t0)){
                        stop("Beta, Eta and/or t0 are not provided.")
                    }else{
                        message("calculateSingleConf: Currently, confidence bounds for Weibull 3P are not supported.")
                    }
                }
                if(tolower(fit$options$dist) %in% c("lognormal","lognormal2p")){
                    message("calculateSingleConf: Currently, confidence bounds for Lognormal are not supported.")
    #                if(is.null(fit$meanlog) || is.null(fit$sdlog)){
    #                    stop("meanlog and/or sdlog are not provided.")
    #                }else{
    #                    if(opaconf$verbosity >= 1)message("calculateSingleConf: ",
    #                        "Found Lognormal 2P distribution.")
    #                    if("blives" %in% tolower(opaconf$conf.what)){
    #                        if(opaconf$verbosity >= 1)message(
    #                            "calculateSingleConf: Calculating ",
    #                                "B-lives confidence bounds.")
    #                        mini <- min(c(opaconf$ylim[1]/10,datarange$yrange[1]/10),0.001)
    #                        maxi <- max(c((1-(1-opaconf$ylim[2])/10),
    #                            (1-(1-datarange$yrange[2])/10),0.999))
    #                        unrel <- c(F0(seq(F0inv(mini),F0inv(maxi),
    #                            length.out=opaconf$unrel.n -
    #                            length(opaconf$unrel+2))),
    #                            opaconf$unrel,0.5,F0(0))
    #                        unrel <- unique(signif(unrel[order(unrel)]))
    #                            # signif() needed for eliminating
    #                            # identical looking unreliability
    #                            # levels that differ only at place far
    #                            # from the decimal point
    #                        if(is.null(fit$conf)){
    #                            fit$conf <- list()
    #                        }
    #                        atLeastOneBLifeConf <- FALSE
    #                        if(is.null(fit$conf$blives)){
    #                            if(opaconf$verbosity >= 2)message(
    #                                "calculateSingleConf: Creating the first ",
    #                                "B-life confidence calculation in the fit...")
    #                            i <- 1
    #                            fit$conf$blives <- list()
    #                        }else{
    #                            if(opaconf$verbosity >= 2)message(
    #                                "calculateSingleConf: Appending a new ",
    #                                "B-life confidence calculation to the fit...")
    #                            i <- length(fit$conf$blives)+1
    #                        }
    #                        fit$conf$blives[[i]] <- list()
    #                        if("mcpivotals" %in% tolower(opaconf$method.conf.blives)){
    #                            if(opaconf$verbosity >= 1)message(
    #                                "calculateSingleConf: Calculating Monte Carlo ",
    #                                "Pivotal confidence bounds.")
    #                            dx <- params.to.ob("lognormal",meanlog=1,sdlog=1,
    #                                event=fit$data$event)
    #                            r1 <- abrem.fit(Abrem(dx[dx$event==1,]),dist=fit$options$dist,
    ##                                method.fit=fit$options$method.fit)
    #                                method.fit=opaconf$method.fit)
    #                                # TODO: what happens whent the above are NULL?
    #                            fit$conf$blives[[i]]        <- list()
    #                            fit$conf$blives[[i]]$type   <- "mcpivotals"
    #                            fit$conf$blives[[i]]$S      <- opaconf$S
    #                            fit$conf$blives[[i]]$seed   <- opaconf$seed
    #                            fit$conf$blives[[i]]$rgen   <- opaconf$rgen
    #                            fit$conf$blives[[i]]$cl     <- opaconf$cl
    #                            fit$conf$blives[[i]]$sides  <- opaconf$conf.blives.sides
    #                            fit$conf$blives[[i]]$unrel <- opaconf$unrel
    #                            ret <- NULL
    #                            try(ret <- .Call("pivotalMCln2p",na.omit(fit$data$rank),
    #                                c(R2=0.0,CI=opaconf$cl,Mu=exp(r1$fit[[1]]$meanlog),
    #                                Sigma=exp(r1$fit[[1]]$sdlog)),opaconf$S,sample.int(
    #                                .Machine$integer.max,1),unrel,FALSE,
    #                                PACKAGE = "pivotals"))
    #                                # TODO: the above funtion is buggy in pivotals 0.1.4
    #                            if(!is.null(ret)){
    #                                fit$conf$blives[[i]]$bounds <- cbind(unrel,
    #                                    #exp(log(fit$eta)+ ret/fit$beta))
    #                                    exp(fit$meanlog + ret/fit$sdlog)
    ##                                    exp(fit$meanlog - ret/fit$sdlog))
    #                                    # TODO: the above probably is comploete nonsense!!
    #                                op <- unique(c(names(opafit),names(opaconf)))
    #                                if(length(li <- opaconf[sapply(op,function(y){
    #                                    !identical(opafit[[y]], opaconf[[y]])})]) > 0){
    #                                    fit$conf$blives[[i]]$options <- li
    #                                }
    #                            }else{
    #                                warning("calculateSingleConf: Confidence calculation failed.")
    #                                fit$conf$blives[[i]] <- NULL
    #                            }
    #                        }
    #                    }
    #                }
                }
            }else{
                stop("Distribution type was not provided.")
            }
        }else{
            if(opadata$verbosity >= 1)
                # TODO: using opadata since no other location of $verbosity is available.
                message("calculateSingleConf: The fit argument is empty or contains no fits.")
        }
    }
    fit
}
