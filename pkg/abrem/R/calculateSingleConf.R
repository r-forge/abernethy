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
calculateSingleConf <- function(fit,opadata,datarange,...){
    # fit is a single fit
    arg <- list(...)
#    ra <- findranges(x,opa$verbosity)
##    xlimits <- range(ra$xrange,na.rm=TRUE)
#    ylimits <- range(ra$yrange,na.rm=TRUE)
#     if(!is.null(opp$ylim)){
#        if(ylimits[1] < opp$ylim[1]) opp$ylim[1] <- ylimits[1]
#        # TODO: this will not have the desirable effect because abrem.conf does not support lists of abrem objects...
#        # also: do not care about the upper limit
#    }else{opp$ylim <- ylimits}
    if(missing(fit)){
        stop("Argument \"fit\" is missing.")
    }else{
        if(!is.null(fit$options))
            opafit <- modifyList(opadata,fit$options)
        opaconf <- modifyList(opafit,arg)
        if(!is.null(fit$options$dist)){
            if(tolower(fit$options$dist) %in% c("weibull","weibull2p")){
                if(is.null(fit$beta) || is.null(fit$eta)){
                    stop("Beta and/or Eta are not provided.")
                }else{
                    if(opaconf$verbosity >= 1)message("calculateSingleConf: ",
                        ": Found Weibull 2P distribution.")
                    if("blives" %in% tolower(opaconf$conf.what)){
                        if(opaconf$verbosity >= 1)message(
                            "calculateSingleConf: Calculating ",
                                "B-lives confidence bounds.")
                        mini <- min(c(opaconf$ylim[1]/10,datarange$yrange[1]/10),0.001)
                        maxi <- max(c((1-(1-opaconf$ylim[2])/10),
                            (1-(1-datarange$yrange[2])/10),0.999))
                        unrel <- c(F0(seq(F0inv(mini),F0inv(maxi),
                                # TODO: this isn't right...
#                        unrel <- c(F0(seq(F0inv(1e-3),
#                            F0inv(0.999),
                            length.out=opaconf$conf.n -
                            length(opaconf$blives+2))),
                            opaconf$blives,0.5,F0(0))
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
                        if("bbb" %in% tolower(opaconf$method.conf.blives)){
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
                                if("bernard" %in% opaconf$pp) ty <- "medianRank"
                                sx$rank <- .Call("medianRank",fit$data$event, PACKAGE= "pivotals")
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
                                Lower= bbb(sx$arank,fit$fail,(1-opaconf$cl)/2,fit$beta,fit$eta),
                                Upper= bbb(sx$arank,fit$fail,1-(1-opaconf$cl)/2,fit$beta,fit$eta))
                            lo <- approxfun(F0inv(sx$rank),log(da$Lower))
                            up <- approxfun(F0inv(sx$rank),log(da$Upper))
                            bl <- F0inv(unrel)
                            da <- rbind(da,data.frame(unrel=unrel,Lower=exp(lo(bl)),Upper=exp(up(bl))))
                                # warning: bounds don't look correct, has to do with FOinv and FO
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
                        if("mcpivotals" %in% tolower(opaconf$method.conf.blives)){
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
                            fit$conf$blives[[i]]$blives <- opaconf$blives
                            ret <- NULL
                            if(is.null(fit$data$rank)){
                                warning("calculateSingleConf: Currently, only rank regression is supported.")
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
                                op <- unique(c(names(opafit),names(opaconf)))
                                    # this is needed to add options from opafit into li that
                                    # are NULL in opafit
                                    # TODO:tolower() not needed?
                                if(length(li <- opaconf[sapply(op,function(y){
                                    !identical(opafit[[y]], opaconf[[y]])})]) > 0){
                                    fit$conf$blives[[i]]$options <- li
                                }
                            }else{
                                warning("calculateSingleConf: Confidence calculation failed.")
                                fit$conf$blives[[i]] <- NULL
                            }
#                            if(!is.null(fit$t0)){
#                                # t0 means weibull 3p -> shift all times
#                                # TODO: these are NOT the correct bounds!!
#                                fit$conf$blives[[i]]$bounds[,-1] <-
#                                    fit$conf$blives[[i]]$bounds[,-1] + fit$t0
#                                warning("Confidence bounds for Weibull 3p are experimental and should not be relied upon!")
#                            }
                        }
                    }
                }
            }
            if("weibull3p" %in% tolower(fit$options$dist)){
                if(is.null(fit$beta) || is.null(fit$eta) || is.null(fit$t0)){
                    stop("Beta, Eta and/or t0 are not provided.")
                }else{
                    warning("calculateSingleConf: Currently, MC pivotal CB for Weibull 3P are not supported.")
                }
            }
            if(tolower(fit$options$dist) %in% c("lognormal","lognormal2p")){
                warning("calculateSingleConf: Currently, MC pivotal CB for Lognormal are not supported.")
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
#                            length.out=opaconf$conf.n -
#                            length(opaconf$blives+2))),
#                            opaconf$blives,0.5,F0(0))
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
#                            fit$conf$blives[[i]]$blives <- opaconf$blives
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
    }
    fit
}
