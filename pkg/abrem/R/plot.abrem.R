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

plot.abrem <- function(x,...){
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
    ra <- findMaxDataRange(x,opa$verbosity)
    xlimits <- range(ra$xrange,na.rm=TRUE)
    ylimits <- range(ra$yrange,na.rm=TRUE)
#    if(is.null(opa$xlim)){
#        opa$xlim <- c(10^(round(log10(xlimits[1]))-1),
#            10^(round(log10(xlimits[2]))+1))
#        # TODO: rewrite in function of the width of the range
#    }
    if(is.null(opa$xlim)){
        opa$xlim <- c(10^(log10(xlimits[1])-0.5),
            10^(log10(xlimits[2])+1))
        # TODO: rewrite in function of the width of the range
    }
    if(is.null(opa$ylim)){
        if(ylimits[1] < 0.01) opa$ylim <- c(signif(ylimits[1],1),0.99)
        else opa$ylim <- c(0.01,0.99)
        # do not care about the upper limit
        # TODO: a problem remains with ylim and calculating confidence bounds
    }
    opanames <- names(opa)
    plotargs <- c(list(x=NA,axes=FALSE),
        opa[opanames %in% plot_default_args()])
    if(!is.null(plotargs$ylim)){
        plotargs$ylim <- F0inv(plotargs$ylim,opa$log)
    }
    do.call(plot.default,plotargs)
    if(opa$is.plot.grid){
        abline(
            h=F0inv(seq.wb(opa$ylim[1]/10,1-(1-opa$ylim[2])/10),opa$log),
            v=seq.log(opa$xlim[1]/10,opa$xlim[2]*10,seq(0,10,1)),
            col = opa$col.grid)
    }
    r <- seq.log(opa$xlim[1]/10,opa$xlim[2]*10,c(1,5))
    lin <- 0.0
    for(t in c(1,3)){
        axis(t,at=seq.log(opa$xlim[1]/10,opa$xlim[2]*10,seq(0,10,0.2)),
        labels=NA,line=lin,tcl=-0.25)
        # plot top and bottom axis tickmarks
        # BUG: when the labels of the x-axis are written using
        # exponential notation, the excessive tickmarks and labels
        # are not clipped and are printed outside the plotting region
        axis(t,at=r,labels=r,line=lin,tcl=-0.75)
        # plot top and bottom axis labels
    }
    r <- c(seq.wb(opa$ylim[1]/10,1-(1-opa$ylim[2])/10,c(1,2,5)),0.9)
    for(t in c(2,4)){
        # TODO: rewrite as do.call() or apply()
        axis(t,at=F0inv(seq.wb(opa$ylim[1]/10,1-(1-opa$ylim[2])/10),
            opa$log),labels=NA,line=lin,tcl=-0.25)
            # plot left and right axis tickmarks
        axis(t,at=F0inv(r,opa$log),
            labels=r*100,line=lin,tcl=-0.75)
            # plot left and right axis labels
    }
    abline(h=0,lty = 3,col = opa$col.grid)
    # plot the 63.2 [%] rank line

    # +--------------------------+
    # |  plot confidence bounds  |
    # +--------------------------+
    plotConfsInAbrem <- function(abrem){
        #opadata <- modifyList(x$options, arg)
        if(!is.null(abrem$fit)){
            ret <- lapply(abrem$fit,plotConfsInFit,opadata=abrem$options,...)
        }else{
            if(!is.null(opa)) if(opa$verbosity >= 1)message(
                "plotConfsInAbrem: This Abrem object contains no fits ",
                "or confidence calculations.")
        }
    }
    lapply(x,plotConfsInAbrem)

    # +-----------------------+
    # |  plot plot positions  |
    # +-----------------------+
    plotSingleDataSet <- function(x){
        if(opa$is.plot.pp){
            # TODO: possibly, this does not allow much flexibility in plotting.
            opadata <- modifyList(x$options,list(...))
            if(!is.null(x$data) &&
                !is.null(ti <- x$data$time) &&
                !is.null(ra <- x$data[,paste0("rank.",opa$pp[1])])){
                # TODO: add support for plotting all rank columns, not just the first one
                points(ti,F0inv(ra,opa$log),pch = opadata$pch,
                    col = opadata$col,lwd = opadata$lwd.points,cex=opadata$cex.points)
                    # option "log" should only be set and read from either
                    # the arguments of plot.abrem
                    # Other instances should be ignored
            }else{stop("This Abrem object contains no probability plot positions.")}
        }
    }
    lapply(x,plotSingleDataSet)
    
    # +-------------+
    # |  plot fits  |
    # +-------------+
    plotFitsInAbrem <- function(abrem){
        opadata <- modifyList(abrem$options,list(opa$xlim,opa$ylim))
        if(!is.null(abrem$fit)){
            ret <- lapply(abrem$fit,plotSingleFit,
                opadata=opadata,...)
        }else{
            if(!is.null(opa)) if(opa$verbosity >= 1)message(
                "plotFitsInAbrem: This Abrem object contains no fits.")
        }
    }
    lapply(x,plotFitsInAbrem)

    # +----------------+
    # |  plot legends  |
    # +----------------+
    lolegends <- NULL
    buildListOfLegends <- function(abrem){
        #opadata <- modifyList(x$options, arg)
        if(!is.null(abrem$fit)){
#            ret <- unlist(lapply(x$fit,buildSingleFitLegend,
#                opadata=x$options,...),FALSE)
            ret <- lapply(abrem$fit,buildSingleFitLegend,
                opadata=abrem$options,...)
        }else{
            ret <- NULL
            if(!is.null(opa)) if(opa$verbosity >= 1)message(
                "buildListOfLegends: This Abrem object contains no fits.")
        }
        ret
    }
    lolegends <- unlist(lapply(x,buildListOfLegends),FALSE)

    if(opa$is.plot.legend){
        plotSingleLegend <- function(le,x,y){
            if(identical(label <- le$label,""))label <- NULL
            legend(
                x=x,
                y=y,
                legend=le$legend,
                title=label,
#                title.col=le$lcol,
                cex = le$legend.text.size,
                    # TODO change opp to something else
                bg = "white",
                lty = unlist(le$lty),
                lwd = unlist(le$lwd),
                pch = unlist(le$pch),
                col = unlist(le$col),
#                inset=0.1,
                text.col = "black",
                xpd=TRUE,
#                merge = TRUE
                )
                # TODO: Warning: unlist coerces numeric colors to character!
        }
        if(!is.null(lolegends)){
            lx <- rep(lolegends[[1]]$rect$left,length(lolegends))
            ly <- lolegends[[1]]$rect$top +
                c(0,cumsum(sapply(lolegends,function(le)le$rect$h)[-1]))
            if(opa$log %in% c("x","xy","yx")) lx <- 10^lx
            if(opa$log %in% c("y","xy","yx")) ly <- 10^ly
                # TODO: F0(ly): looks very suspicious that this works -> investigate!
            for(i in 1:length(lolegends)){
                plotSingleLegend(lolegends[[i]],lx[i],ly[i])
                # TODO: replace with lapply
            }
        }else{
            if(opa$verbosity >= 1)message(
                "plot.abrem: There is no legend to plot.")
        }
    }
#    if(opa$log == "x") legend("top",legend=NA,title="Weibull",bg="white")
#    if(opa$log == "xy") legend("top",legend=NA,title="Lognormal",bg="white")
#    if(opa$log %in% c("","y")) legend("top",legend=NA,title="xxx",bg="white")
    invisible()
        # TODO: return the abrem object with updated graphical options
        # TODO: check if this makes sense when supplying a list
}
