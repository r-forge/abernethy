# R package 'abrem'
# Abernethy Reliability Methods
# Implementations of lifetime data analysis methods described in
# 'The New Weibull Handbook, Fifth edition' by Dr. Robert B. Abernethy.
# November 2013, Jurgen Symynck
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

contour.abrem <- function(x,...){
    # +------------------------------+
    # |  move abrem objects to list  |
    # |      of abrem objects        |
    # +------------------------------+
    if(identical(class(x),"abrem")) x <- list(x)
    if(!all(sapply(x,function(x)identical(class(x),"abrem")))){
        stop("Argument \"x\" is not of class \"abrem\" or ",
        "a list of \"abrem\" objects.")
    }
    # as of this point, x is always a list of one or more abrem objects
    
    # +------------------------------------+
    # |  create default options arguments  |
    # +------------------------------------+
#    arg <- splitargs(list(...))
        # TODO: check effect on c(...) or (...)
    #arg <- list(...)
    opa <- x[[1]]$options
    opa <- modifyList(opa, list(...))


    # +--------------------------+
    # |  create new plot canvas  |
    # +--------------------------+
    contourRanges <- findContourRanges(x,opa$verbosity)
    if(!is.null(contourRanges)){
        xlimits <- range(contourRanges[,1])
        ylimits <- range(contourRanges[,2])
        opanames <- names(opa)
        plotargs <- c(list(x=NA,axes=TRUE),
            opa[opanames %in% plot_default_args()])
        plotargs$xlim <- xlimits
        plotargs$ylim <- ylimits
        plotargs$main <- opa$main.contour
        plotargs$sub  <- opa$sub.contour
        plotargs$log <- ""
        plotargs$xlab <- "Eta"
        plotargs$ylab <- "Beta"
            # TODO: add support for meanlog vs. sdlog contours

        #if(tolower(opafit$dist) %in% c("weibull","weibull2p","weibull-2","weibull2p-2","weibull3p")){
            # TODO: the above not needed here?
        do.call("plot.default",plotargs)
        if(opa$is.plot.grid){
            abline(
                h=pretty(contourRanges[,2],10),
                v=pretty(contourRanges[,1],10),
                col = opa$col.grid)
                # TODO: add userchoice in grid density here
        }
    }else message("contour.abrem: No contours available in (list of) abrem objects.")
#    r <- seq.log(opa$xlim[1]/10,opa$xlim[2]*10,c(1,5))

    # +------------------+
    # |  plot contours   |
    # +------------------+
    # TODO: differentiate between contour plots based on mean vs. sdlog and eta vs. beta

    plotContours <- function(abrem){
        if(!is.null(abrem$fit)){
            plotContours2 <- function(fit){
                if(!is.null(fit$options)){
                    opafit <- modifyList(abrem$options,fit$options)
                }else{opafit <- abrem$options}
                is_MLE <- any(c("mle","mle-rba") %in% tolower(fit$options$method.fit))
                if(!is.null(fit$conf$blives)){
                    plotContours3 <- function(blicon){
                        if(!is.null(blicon$options)){
                            opaconf <- modifyList(opafit,blicon$options)
                        }else{opaconf <- opafit}
                        opaconf <- modifyList(opaconf,list(...))
                        if(!is.null(blicon$MLEXContour)){
                            con <- rbind(blicon$MLEXContour$Lower,
                                blicon$MLEXContour$Right,
                                blicon$MLEXContour$Upper[nrow(blicon$MLEXContour$Upper):1,],
                                blicon$MLEXContour$Left[nrow(blicon$MLEXContour$Left):1,])
                                # shuffeling quadrant names and reversing the rows
                            #con2 <- lapply(con,function(x)do.call("rbind",x))
    #                            if(!("mle-rba" %in% tolower(opafit$method.fit))){
                                # draw MLE-RBA beta and eta
                            if(!is_MLE)
                                points(blicon$MLEXContour$MLEpoint,pch=20,col=abrem$options$col)
                                # if MLE or MLE-RBA was used to calculate the distribution
                                # parameters, the omit plotting the MLEpoint. In all other cases,
                                # plot the MLEpoint because the distribution parameters will not match exactly
                                # the MLE point
                            if(all(c(!is.null(fit$beta),!is.null(fit$eta))))
                                points(x=fit$eta,y=fit$beta,pch=abrem$options$pch,col=abrem$options$col,
                                    lwd=abrem$options$lwd.points,cex=abrem$options$cex.points)
                            points(con,type="l",lwd=opaconf$lwd,lty=opaconf$lty,col=opaconf$col)
                        }
                    }
                    #mtrace(plotContours3)
                    do.call("rbind",lapply(fit$conf$blives,plotContours3))
                        # combine the ranges from all MLEXContours
                        # found in the list of blicons
                }
            }
            do.call("rbind",lapply(abrem$fit,plotContours2))
                # combine the ranges from all MLEXContours
                # found in the list of fits
        }
    }
    if(!is.null(contourRanges)) lapply(x,plotContours)

    # +----------------+
    # |  plot legends  |
    # +----------------+
#    # TODO: much of this code can be merged with legend code from plot.abrem
#    lolegends <- NULL
#    buildListOfLegends <- function(abrem){
#        #opadata <- modifyList(x$options, arg)
#        if(!is.null(abrem$fit)){
#            ret <- lapply(abrem$fit,buildSingleContourLegend,opadata=abrem$options,...)
#        }else{
#            ret <- NULL
#            if(!is.null(opa)) if(opa$verbosity >= 1)message(
#                "contour.abrem:::buildListOfLegends: This Abrem object contains no fits.")
#        }
#        ret
#    }
#    lolegends <- unlist(lapply(x,buildListOfLegends),FALSE)
#    if(opa$is.plot.legend){
#        plotSingleContourLegend <- function(le,x,y){
#            if(identical(label <- le$label,""))label <- NULL
#            legend(
#                x=x,
#                y=y,
#                legend=le$legend,
#                title=label,
##                title.col=le$lcol,
#                cex = le$legend.text.size,
#                    # TODO change opp to something else
#                bg = "white",
#                lty = unlist(le$lty),
#                lwd = unlist(le$lwd),
#                pch = unlist(le$pch),
#                col = unlist(le$col),
#                text.col = "black",
#                xpd=TRUE,
#                )
#                # TODO: Warning: unlist coerces numeric colors to character!
#        }
#        if(!is.null(lolegends)){
#            lx <- rep(lolegends[[1]]$rect$left,length(lolegends))
#            ly <- lolegends[[1]]$rect$top +
#                c(0,cumsum(sapply(lolegends,function(le)le$rect$h)[-1]))
#            if(opa$log %in% c("x","xy","yx")) lx <- 10^lx
#            if(opa$log %in% c("y","xy","yx")) ly <- 10^ly
#                # TODO: F0(ly): looks very suspicious that this works -> investigate!
#            for(i in 1:length(lolegends)){
#                plotSingleContourLegend(lolegends[[i]],lx[i],ly[i])
#                # TODO: replace with lapply
#            }
#        }else{
#            if(opa$verbosity >= 1)message(
#                "contour.abrem: There is no legend to plot.")
#        }
#    }
    invisible()
}
