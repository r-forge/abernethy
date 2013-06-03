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
abrem.conf <- function(x,which="last",...){
    args <- splitargs(list(...))
    opp <- options.abremplot()
    opp <- modifyList(opp, args$opp)
    opa <- options.abrem()
    opa <- modifyList(opa, args$opa)
#    opp <- ifelse(is.null(args$opp),NULL,modifyList(opp, args$opp))
    # TODO: check for argument "add": this shoudn't be supplied here
    if(is.null(args$opp)){opp <- NULL
    }else{opp <- modifyList(opp, args$opp)}
        # opp is NULL when no graphical parameters
        # have been passed through the function.
    opa <- modifyList(opa, args$opa)
    if(!missing(x)){
        if(identical(class(x),"abrem")){
            if(!is.null(x$fit)){
                conf <- function(fit){
                    if(!is.null(fit$options$dist)){
                        if(tolower(fit$options$dist) %in%
                            c("weibull","weibull2p","weibull3p")){
                            if(!is.null(fit$beta) && !is.null(fit$eta)){
                                if(opa$verbosity >= 1)message("    ",match.call()[[1]],": Found weibull distribution with beta and eta.")
                                # this is a weibull fit
                                if("blives" %in% tolower(opa$conf.what)){
                                    # calculate blives conf
                                    if(opa$verbosity >= 1)message("    ",match.call()[[1]],": Calculating B-lives confidence bounds.")
                                    unrel <- c(F0(seq(F0inv(opp$ylim[1]/10),
                                        F0inv(1-(1-opp$ylim[2])/10),
                                            length.out=opa$conf.n-
                                                length(opa$blives+2))),
                                        opa$blives,0.5,F0(0))
                                    unrel <- unique(signif(unrel[order(unrel)]))
                                        # signif() needed for eliminating identical looking
                                        # unreliability levels that differ only at place far from
                                        # the decimal point
                                        # unrel <- c(F0(seq(par('usr')[3],par('usr')[4],
                                    if(is.null(fit$conf))
                                        fit$conf <- list()
                                    if(is.null(fit$conf$blives)){
                                        fit$conf$blives <- list()
                                        fit$conf$blives[[1]] <- list()
                                    }
                                    if("bbb" %in% tolower(opa$method.conf.blives)){
                                        if(opa$verbosity >= 1)message("        ",
                                            match.call()[[1]],": Calculating bbb confidence bounds.")
                                        i <- length(fit$conf$blives)
                                        if(!(length(fit$conf$blives[[i]]) == 0)){
                                            i <- i+1}
                                        fit$conf$blives[[i]] <- list()
                                        fit$conf$blives[[i]]$type <- "bbb"
                                        fit$conf$blives[[i]]$cl <- opa$cl
                                        fit$conf$blives[[i]]$sides <- opa$conf.blives.sides
                                        if(!is.null(args$opp) && length(args$opp)>0){
                                            fit$conf$blives[[i]]$options.abremplot <- args$opp
                                        }
                                        da <- data.frame(
                                            unrel= fit$data$mrank,
                                            Lower= bbb(fit$data$arank,fit$n,(1-opa$cl)/2,fit$beta,fit$eta),
                                            Upper= bbb(fit$data$arank,fit$n,1-(1-opa$cl)/2,fit$beta,fit$eta))
                                        lo <- approxfun(F0inv(fit$data$mrank),log(da$Lower))
                                        up <- approxfun(F0inv(fit$data$mrank),log(da$Upper))
                                        bl <- F0inv(unrel)
                                        da <- rbind(da,data.frame(unrel=unrel,Lower=exp(lo(bl)),Upper=exp(up(bl))))
                                            # warning: bounds don't look correct, has to do with FOinv and FO
                                        da <- da[order(da$unrel),]
                                        da <- da[!duplicated(da$unrel),]
                                        fit$conf$blives[[i]]$bounds <- da
                                    }
                                    if("mcpivotal" %in% tolower(opa$method.conf.blives)){
                                        if(opa$verbosity >= 1)message("        ",
                                            match.call()[[1]],": Calculating mcpivotal confidence bounds.")
                                        i <- length(fit$conf$blives)
                                        if(!(length(fit$conf$blives[[i]]) == 0)){
                                            i <- i+1}
                                            dx <- params.to.ft("weibull",beta=1,eta=1,
                                                event=fit$data$event)
                                        r1 <- abrem.fit(Abrem(dx[dx$event==1,]),
                                            methods.fit=fit$method.fit)
                                        fit$conf$blives[[i]] <- list()
                                        fit$conf$blives[[i]]$type <- "mcpivotals"
                                        fit$conf$blives[[i]]$S <- opa$S
                                        fit$conf$blives[[i]]$seed <- opa$seed
                                        fit$conf$blives[[i]]$rgen <- opa$rgen
                                        fit$conf$blives[[i]]$cl <- opa$cl
                                        fit$conf$blives[[i]]$sides <- opa$conf.blives.sides
                                        if(!is.null(args$opp) && length(args$opp)>0){
                                            fit$conf$blives[[i]]$options.abremplot <- args$opp
                                        }
                                        fit$conf$blives[[i]]$bounds <- cbind(unrel,exp(
                                            log(fit$eta)+pivotals::CBpiv(
                                            x=fit$data$event,
                                            CI=opa$cl,
                                            S=opa$S,
                                            Bval=unrel,
                                            Eta=r1$fit[[1]]$eta,
                                            Beta=r1$fit[[1]]$beta,
                                            seed=sample.int(
                                                .Machine$integer.max,1))/fit$beta))
                                    }
                                }
                            }else{stop(match.call()[[1]],": Found no beta and/or eta in fit.")}
                        }
                    }else{stop(match.call()[[1]],": Distribution type was not provided.")}
                    fit
                }
                # if(identical(tolower(which),"all"))
                # lapply(x$fit,conf,type)
                x$fit[[length(x$fit)]] <- conf(x$fit[[length(x$fit)]])
            }else{
                if(opa$verbosity >= 1)
                    stop(match.call()[[1]],": Object does not contain anything under \"$fit\".")
            }
        }else{stop(match.call()[[1]],": Input data is not of class \"abrem\".")}
    }else{stop(match.call()[[1]],": No lifetime data provided.")}
    x
}

#    d1 <- wbparams.to.ft(beta=1,eta=1,event=event <- x$data$event)
#        # careful with assuming the time data is ordered!
#        # step 1: generate a dataset with beta, eta = 1
#    r1 <- get.wbparams(d1[d1[,dim(d1)[2]]==1,])
#    pivbounds <- cbind(unrel,exp(
#        log(x$fit[[1]]$eta)+pivotals::CBpiv(
#            x=event,CI=cl,S=R,Bval=unrel,
#            Eta=r1[['eta']],Beta=r1[['beta']],
#            seed=sample.int(.Machine$integer.max,1))/x$fit[[1]]$beta))
#                # need to check what the value of the seed can be
#    x$fit[[1]]$conf <- list()
#    x$fit[[1]]$conf$blives <- list()
#    x$fit[[1]]$conf$blives[[1]] <- list()
#    x$fit[[1]]$conf$blives[[1]]$type <- "mcpivotal"
#    x$fit[[1]]$conf$blives[[1]]$R <- R
#    x$fit[[1]]$conf$blives[[1]]$cl <- cl
#    x$fit[[1]]$conf$blives[[1]]$sides <- "double"
#    x$fit[[1]]$conf$blives[[1]]$bounds <- pivbounds
#    cl2 <- 0.95
#    x$fit[[1]]$conf$blives[[2]] <- list()
#    x$fit[[1]]$conf$blives[[2]]$type <- "bbb"
#    x$fit[[1]]$conf$blives[[2]]$cl <- cl2
#    x$fit[[1]]$conf$blives[[2]]$sides <- "double"
#    da2 <- data.frame(
##            unrel= sort(c(fit$data$mrank,options.abrem("Blives"))),
#        unrel= fit$data$mrank,
#        Lower= bbb(fit$data$arank,fit$n,(1-cl2)/2,fit$beta,fit$eta),
#        Upper= bbb(fit$data$arank,fit$n,1-(1-cl2)/2,fit$beta,fit$eta))
#    lo <- approxfun(F0inv(fit$data$mrank),log(da2$Lower))
#    up <- approxfun(F0inv(fit$data$mrank),log(da2$Upper))
#    bl <- F0inv(options.abrem("Blives"))
#    da2 <- rbind(da2,data.frame(unrel=F0(bl),Lower=exp(lo(bl)),Upper=exp(up(bl))))
#    # warning: bounds don't look correct, has to do with FOinv and FO
#    da2 <- da2[order(da2$unrel),]
#    da2 <- da2[!duplicated(da2$unrel),]
#
#    x$fit[[1]]$conf$blives[[2]]$bounds <- da2