# R package 'abremout'
# output methods for the abrem object
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
plot.abrem <- function(x,...){

    args <- abrem:::splitargs(list(...))
        # why abrem::: needed? because of the fact that splitargs is not in
        # abrem's NAMESPACE file?
        # TODO: check effect on c(...) or (...)
    opp <- options.abremplot()
    opp <- modifyList(opp, args$opp)
    opa <- options.abrem()
    opa <- modifyList(opa, args$opa)

#    lolegend <- list()
        # get the current options, possibly modified with specified parameters
        # NEEDS ADDITIONAL WORK to make it foolproof!
    plotsingle <- function(x,is.add,...){
        if(!is.null(x$fit)){
            plots <- ifelse(length(x$fit)==0,1,1:length(x$fit))
        }else{if(opa$verbosity >= 1)
            message(match.call()[[1]],": Object does not contain anything under \"$fit\".")}
        if(!is.null(x$data)){
            range <- range(x$data$time,na.rm=TRUE)
            if(is.null(opp$xlim))
                opp$xlim <- c(10^(floor(log10(range[1]))-1),
                    10^(ceiling(log10(range[2]))+1))
                # TODO: modify the xlim code:
                # - if there is a fit or fits, get xlim form the largest span
                #   of these fits
                # - if there is no fit but only data: base the xlim on the data
            if(!is.add && !is.null(x$data$time)){
                oppnames <- names(opp)
                plotargs <- c(list(x=NA,axes=FALSE),
                    opp[oppnames %in% abrem:::plot_default_args()])
                        # TODO: is abrem::: really needed?
                if(!is.null(plotargs$ylim))
                    plotargs$ylim <- abrem:::F0inv(plotargs$ylim)
                do.call(plot.default,plotargs)
    #            par(ask=TRUE)
                if(opp$is.plot.grid){
                    abline(
                        h=abrem:::F0inv(abrem:::seq.wb(opp$ylim[1]/10,1-(1-opp$ylim[2])/10)),
                        v=abrem:::seq.log(opp$xlim[1]/10,opp$xlim[2]*10,seq(0,10,1)),
                        col = opp$col.grid)
                }
                l <- 0.0 #?
                r <- abrem:::seq.log(opp$xlim[1]/10,opp$xlim[2]*10,c(1,5))
                for(t in c(1,3)){
                    axis(t,at=abrem:::seq.log(opp$xlim[1]/10,opp$xlim[2]*10,seq(0,10,0.2)),
                    labels=NA,line=l,tcl=-0.25)
                    # plot top and bottom axis tickmarks
                    # BUG: when the labels of the x-axis are written using
                    # exponential notation, the excessive tickmarks and labels
                    # are not clipped and are printed outside the plotting region
                    axis(t,at=r,labels=r,line=l,tcl=-0.75)
                    # plot top and bottom axis labels
                }
                r <- c(abrem:::seq.wb(opp$ylim[1]/10,1-(1-opp$ylim[2])/10,c(1,2,5)),0.9)
                for(t in c(2,4)){
                    ## rewrite as do.call() or apply()
                    axis(t,at=abrem:::F0inv(abrem:::seq.wb(opp$ylim[1]/10,1-(1-opp$ylim[2])/10)),
                        labels=NA,line=l,tcl=-0.25)
                        # plot left and right axis tickmarks
                    axis(t,at=abrem:::F0inv(r),
                        labels=r*100,line=l,tcl=-0.75)
                        # plot left and right axis labels
                }
                abline(h=0,lty = 3)
                # plot the 63.2 % median rank line (used to determine eta
                # graphically)
            }else{
                if(is.null(x$data$time))
                    stop(match.call()[[1]],": $data contains no \"$time\" column -> ",
                    "cannot create plot canvas.")
            }
        }else{stop(match.call()[[1]],": Object does not contain anything under \"$data\".")}
        if(opp$is.plot.legend){
            # construct all legends in a legend list, but do not plot them yet...
            buildlegend <- function(fit){
                if(!is.null(fit$options.abremplot)){
                    oppfit <- modifyList(opp,fit$options.abremplot)
                }else{oppfit <- opp}
                    # TODO: further examination needed
                si <- function(number)signif(number,oppfit$signif)
                    # shorter writing form for signif()
                #ccc2 <- CCC2wb2(fit$fail)$CCC2
                #ccc2s <- CCC2wb2(fit$fail)$signif
                li <- list()
                li[[10]] <- paste0("(",oppfit$xlab," ; ",oppfit$ylab,")")
                li[[20]] <- paste0(fit$options$dist," (",
                    paste0(fit$options$method.fit,collapse=", "),")")
                    # TODO: what happens if $method is a character vector?
                li[[40]] <- ifelse(is.null(fit$rate),NA,
                    paste0("rate = ",si(fit$rate)))
                li[[50]] <- ifelse(is.null(fit$meanlog),NA,
                    paste0("mean(log) = ",si(exp(fit$meanlog))," (",
                        si(fit$meanlog),")"))
                li[[60]] <- ifelse(is.null(fit$sdlog),NA,
                    paste0("sd(log) = ",si(exp(fit$sdlog))," (",
                        si(fit$sdlog),")"))
                li[[70]] <- ifelse(is.null(fit$beta),NA,
                    paste0("beta = ",si(fit$beta)))
                li[[80]] <- ifelse(is.null(fit$eta),NA,
                    paste0("eta = ",si(fit$eta)))
                li[[90]] <- paste0("n (fail | cens.) = ",fit$n,
                    " (",fit$fail," | ",fit$cens,")")
                #li[[100]] <- ifelse(is.null(fit$rho2),NA,
                #    paste0("r^2 | CCC^2 = ",signif(fit$rho2,ccc2s)," | ",
                #        signif(ccc2,ccc2s),if(!is.na(ccc2))
                #        ifelse(fit$rho2>=ccc2," (good)"," (BAD)")))
                    # dropped displayin of CCC2, will be replaced with
                    # prr and/or pve
                li[[110]] <- ""
                li[[130]] <- legendconf(fit,"Blives",opa,opp)
                li[[120]] <- legendconf(fit,"params",opa,opp)

                le <- list()
                le$text <- na.omit(unlist(li))
                le$ls   <- length(le$text)
                le$llty <- rep(NA,le$ls);le$llty[2] <- oppfit$lty
                le$lpch <- rep(NA,le$ls)
                if(!is.null(fit$data) &&
                    !identical(tolower(class(fit$data)),"surv"))
                    if(!is.null(fit$data$mrank))le$lpch[1] <- oppfit$pch
                        # if mrank is present then datapoints were plotted
                le$lcol <- rep(NA,le$ls);le$lcol[c(1,2)] <- c(rep(oppfit$col,2))
                    # TODO: add support for displaying individual colors,
                    # linetypes and
                le$llwd <- rep(NA,le$ls);le$llwd[c(1,2)] <-
                    c(oppfit$lwd.points,oppfit$lwd)
                le$rect <- legend(
                    "bottomright",
    #                "topright",
                    legend=le$text,
                    title=opp$legend.title,
                    cex = opp$legend.text.size,
                    inset=0.02,
    #                merge = TRUE,
                    plot=FALSE)$rect
                list(le)
            }
        if(!is.null(x$fit)){
            lolegend <<- sapply(x$fit,buildlegend)
        }else{
            if(!is.null(x$data)){
                lolegend <<- NULL
            }else{
                lolegend <<- NULL
            }
        }
                # list of legends to be displayed
            #sapply(lolegend,function(x)print(x$rect))
            #,lpos =
            #    ifelse(length(x$fit)==0,1,1:length(x$fit)))
        }
        if(!is.null(x$fit)){
            for.each.fit <- function(fit){
                # - plot CB and datum
                # - plot fitted line
                # - plot datapoints if any
                # - plot legend
                if(!is.null(fit$options.abremplot)){
                    oppfit <- modifyList(opp,fit$options.abremplot)
                }else{oppfit <- opp}
                if(!is.null(fit$options$dist)){
                    # TODO: the above is not foolproof, rewrite
                    if(opa$verbosity >= 1)message(match.call()[[1]],": ",paste0("Found \"",fit$options$dist,"\" fit..."))
                }else{warning(match.call()[[1]],": Found unnamed distribution fit.")}
                if(!is.null(fit$conf$blives)){
                    for.each.blc <- function(blc){
                        if(!is.null(blc$options.abremplot)){
                            conffit <- modifyList(oppfit,blc$options.abremplot)
                        }else{conffit <- oppfit}
                        if(oppfit$is.plot.cb && conffit$is.plot.cb){
                            if(!is.null(blc$bounds$Median))
                                lines(y=abrem:::F0inv(blc$bounds$unrel),
                                    x=blc$bounds$Median,
                                    col=conffit$col,lwd=1,lty=2)
                            if(!is.null(blc$bounds$Lower))
                                lines(y=abrem:::F0inv(blc$bounds$unrel),
                                    x=blc$bounds$Lower,col=conffit$col,
                                    lwd=conffit$lwd,lty=conffit$lty)
                            if(!is.null(blc$bounds$Upper))
                                lines(y=abrem:::F0inv(blc$bounds$unrel),
                                    x=blc$bounds$Upper,col=conffit$col,
                                    lwd=conffit$lwd,lty=conffit$lty)
                        }
                    }
                    lapply(fit$conf$blives,for.each.blc)
                }else{if(opa$verbosity >= 1)message("    ",match.call()[[1]],
                    ": No confidence calculations for B-lives found.")}
                if(oppfit$is.plot.fittedline){
                    if(!is.null(fit$beta) && !is.null(fit$eta)){
                        if(is.null(fit$lambda) && is.null(fit$alpha)){
                            ### weibul 2p ###
                            if(opa$verbosity >= 1)message("    ",
                                match.call()[[1]],
                                ": Adding weibull 2P fit ...")
                            curve(abrem:::F0inv(pweibull(x,fit$beta,fit$eta)),
                                add=TRUE,
                                col=oppfit$col,lwd=oppfit$lwd,lty=oppfit$lty,
                                xlim=c(oppfit$xlim[1]/10,oppfit$xlim[2]*10),
                            log=oppfit$log,n=1001)
                        }else{
                            message("    ",match.call()[[1]],
                                ": Currently, there is no support",
                                "for Weibull 3P.")
                        }
                    }
                    if(!is.null(fit$meanlog) && !is.null(fit$sdlog)){
                        ### lognormal ###
                        if(opa$verbosity >= 1)message("    ",match.call()[[1]],
                            ": Adding lognormal fit ...")
                        curve(abrem:::F0inv(plnorm(x,fit$meanlog,fit$sdlog)),
                            add=TRUE,
                            col=oppfit$col,lwd=oppfit$lwd,
                            xlim=c(oppfit$xlim[1]/10,oppfit$xlim[2]*10),
                            log=oppfit$log,n=1001)
                            # TODO: check it n=1001 is not too extreme
                    }
                    if(!is.null(fit$rate)){
                        ### exponential ###
                        if(opa$verbosity >= 1)if(opa$verbosity >= 1)
                            message("    ",match.call()[[1]],
                                ": Adding exponential fit ...")
                        curve(abrem:::F0inv(pexp(x,fit$rate)),add=TRUE,
                            col=oppfit$col,lwd=oppfit$lwd,
                            xlim=c(oppfit$xlim[1]/10,oppfit$xlim[2]*10),
                            log=oppfit$log,n=1001)
                    }
                }
                if(opp$is.plot.datapoints){
                    if(!is.null(fit$data) &&
                        !identical(tolower(class(fit$data)),"surv")){
                            # TODO: the above should be expanded to accomodate
                            # other types of data ...
                        if(!is.null(x <- fit$data$time) && !is.null(y <- fit$data$mrank)){
                            points(x,abrem:::F0inv(y),pch = oppfit$pch,
                                col = oppfit$col,lwd = oppfit$lwd.points)
                        }else{if(opa$verbosity >= 1)message("    ",
                            match.call()[[1]],
                            ": No plotable datapoints were generated",
                            " by this fit.")}
                    }else{if(opa$verbosity >= 1)message("    ",match.call()[[1]],
                        ": No plotable datapoints were generated by this fit.")
                    }
                }
            }
    #        mapply(fun1,x$fit,1:plots)
            lapply(x$fit,for.each.fit)
        }else{
            if(opa$verbosity >= 1)
                message(match.call()[[1]],
                    ": No distribution has been fitted to the data.")
        }
        if(opp$is.plot.legend){
            # construct all legends in a legend list, but do not plot them yet...
            addlegend <- function(le,x,y){
    #            oppfit <- modifyList(opp,fit$options.abremplot)
                legend(
    #                "topright",
    #                x=10*max(opp$xlim),
                    x=10^x,
                    y=y,
                    legend=le$text,
                    title=opp$legend.title,
                        # TODO change opp to something else
                    title.col=le$lcol,
                    cex = opp$legend.text.size,
                        # TODO change opp to something else
                    bg = "white",
                    lty = le$llty,
                    lwd = le$llwd,
                    pch = le$lpch,
                    col = le$lcol,
                    text.col = "black",
                    xpd=TRUE,
                    merge = TRUE
                    )
            }
            if(!is.null(lolegend)){
                x <- rep(lolegend[[1]]$rect$left,length(lolegend))
                y <- lolegend[[1]]$rect$top +
                    c(0,cumsum(sapply(lolegend,function(le)le$rect$h)[-1]))
                for(i in 1:length(lolegend)){
                    addlegend(lolegend[[i]],x[i],y[i])
                }
            }else{if(opa$verbosity >= 1)message(match.call()[[1]],
                ": There is no legend to plot.")}
        }
    }
    if(is.null(is.add <- as.list(substitute(list(...)))[-1L]$add))
        is.add <- FALSE
    if(!is.null(opp$add))if(opp$add) is.add <- TRUE
    if(max(opp$ylim) >= 1 || min(opp$ylim) <= 0)
        stop(match.call()[[1]],": ylim values must lie in the interval ]0,1[.")
    if(identical(class(x),"abrem")){
        ## abrem.preprocess()
        ret <-plotsingle(x,is.add=is.add,...)
    }else{
        if(all(sapply(x,function(x)identical(class(x),"abrem")))){
            ## abrem.preprocess()
            ret <- plotsingle(x[[1]],is.add=is.add,...)
            message(match.call()[[1]],": Plotting multiple abrem objects is not (yet) supported;\n",
                " -> Plotted only the first object.\n")
            #ret <- list(ret,lapply(x,plotsingle,is.add=TRUE,...))
            
        }else{
            stop(match.call()[[1]],": Data is not of class \"abrem\" or ",
                "a list of \"abrem\" objects.")
        }
    }
    invisible()
        # TODO: return tha abrem object with updated graphical options
}