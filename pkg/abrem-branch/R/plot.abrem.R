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
    opa <- x[[1]]$options
    opa <- modifyList(opa, list(...))

    # +--------------------------------------+
    # |  dealing with  threshold parameters  |
    # +--------------------------------------+

    if(!is.null(list(...)$threshold))
        message("Currently, passing the \'threshold\' argument to plot.abrem is not supported. Proceeding...")
    #T0 <- unlist(lapply(x,function(x)return(x$threshold)))
        # a list of thresholds to be applied to the pp
  #x$threshold <- tail(findThresholds(x,opa$verbosity),1)
    
    # +--------------------------+
    # |  create new plot canvas  |
    # +--------------------------+
    ra <- findMaxDataRange(x,opa$verbosity,opa$log)
        # NA values can be part of ra, when log scales are to be used
        # and there are negative failure times
    xlimits <- range(ra$xrange,na.rm=TRUE)
    ylimits <- range(ra$yrange,na.rm=TRUE)
    if(is.null(opa$xlim)){
        opa$xlim <- c(10^(log10(xlimits[1])-0.5),
            10^(log10(xlimits[2])+1))
    }
    if(is.null(opa$ylim)){
        if(ylimits[1] < 0.01) opa$ylim <- c(signif(ylimits[1],1),0.99)
        else opa$ylim <- c(0.01,0.99)
        # do not care about the upper limit
    }
    opanames <- names(opa)
    plotargs <- c(list(x=NA,axes=FALSE),
        opa[opanames %in% plot_default_args()])
    if(!is.null(plotargs$ylim)){
        plotargs$ylim <- F0inv(plotargs$ylim,opa$log)
    }
    plotargs$main <- NULL
        # do not plot "main" just yet...
    if(!is.null(opa$mar))par(mar=opa$mar)
    if(!is.null(opa$mai))par(mai=opa$mai)
    do.call(plot.default,plotargs)
    if(opa$is.plot.grid){
        abline(
            h=F0inv(seq.wb(opa$ylim[1]/10,1-(1-opa$ylim[2])/10),opa$log),
            v=seq.log(opa$xlim[1]/10,opa$xlim[2]*10,seq(0,10,1)),
            col = opa$col.grid)
    }
    r <- seq.log(opa$xlim[1]/10,opa$xlim[2]*10,c(1,5))
    #lin <- 0.0
    for(t in c(1,3)){
        axis(t,at=seq.log(opa$xlim[1]/10,opa$xlim[2]*10,seq(0,10,0.2)),
            labels=NA,tcl=-0.25)#,line=0.0
            # plot top and bottom axis tickmarks
        axis(t,at=r,labels=r,tcl=-0.75)#,line=0.0
            # plot top and bottom axis labels
    }
    r <- c(seq.wb(opa$ylim[1]/10,1-(1-opa$ylim[2])/10,c(1,2,5)),0.9)
    for(t in c(2,4)){
        # TODO: rewrite as do.call() or apply()
        axis(t,at=F0inv(seq.wb(opa$ylim[1]/10,1-(1-opa$ylim[2])/10),
            opa$log),labels=NA,tcl=-0.25)#,line=0.0
            # plot left and right axis tickmarks
        axis(t,at=F0inv(r,opa$log),
            labels=r*100,tcl=-0.75)#,line=0.0
            # plot left and right axis labels
    }
    abline(h=0,lty = 3,col = opa$col.grid)
    title(main=opa$main,line=3)
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

    # +-----------------------+
    # |  plot plot positions  |
    # +-----------------------+
    plotSingleDataSet <- function(x){
        if(opa$is.plot.pp){
            # TODO: possibly, this does not allow much flexibility in plotting.
            opadata <- modifyList(x$options,list(...))
            if(!is.null(x$data) &&
                !is.null(ti <- x$data$time) &&
                !is.null(ra <- x$data[,paste0("rank.",opadata$pp[1])])){
                # TODO: add support for plotting all rank columns, not just the first one
#                if(opadata$log %in% c("x","xy","yx")){
#                    replace
#                }
                t0 <- 0
                if(is.logical(opadata$threshold))if(opadata$threshold)
                    warning ("opadata$threshold is a logical value but numeric value was expected. Proceeding...")
                if(is.numeric(opadata$threshold))t0 <- opadata$threshold
                points(ti-t0,F0inv(ra,opadata$log),pch = opadata$pch,
                    col = opadata$col,lwd = opadata$lwd.points,cex=opadata$cex.points)
                    # option "log" should only be set and read from either
                    # the arguments of plot.abrem
                    # Other instances should be ignored
            }else{stop("This Abrem object contains no probability plot positions.")}
        }
    }
    lapply(x,plotSingleDataSet)

    # +----------------+
    # |  plot legends  |
    # +----------------+
    lolegends <- NULL
    buildListOfLegends <- function(abrem){
        ret <- NULL
#        if(abrem$options$is.plot.legend && opa$is.plot.legend){
        if(!is.null(abrem$fit) && any(sapply(abrem$fit,function(fi)!is.null(fi)))){
                # TODO:
#            if(!is.null(abrem$fit)){
            # check if any non-NULL list holds only NULL items
            # this is needed for deamling with failed fit attempts
            # that currently take the form of
            # abrem$fit[i] <- list(NULL)
    #            ret <- unlist(lapply(x$fit,buildSingleFitLegend,
    #                opadata=x$options,...),FALSE)
            ret <- lapply(abrem$fit,buildSingleFitLegend,
                opadata=abrem$options,...)
        }else{
            if(abrem$options$is.plot.legend && opa$is.plot.legend){
                ret <- list(buildSingleDataLegend(abrem,opadata=abrem$options,...))
                if(!is.null(opa)) if(opa$verbosity >= 1)message(
                    "buildListOfLegends: This Abrem object contains no fits.")
            }
        }
        ret
    }
    lolegends <- unlist(lapply(x,buildListOfLegends),FALSE)
        # TODO: likely, unlist is NOT the best way to go here, investigate
    lolegends <- lolegends[sapply(lolegends,function(lol)!is.null(lol))]
        # omit any list entries that contain only NULL

#    if(opa$is.plot.legend){
    plotSingleLegend <- function(le,x,y){
        if(identical(label <- le$label,""))label <- NULL
        if(is.null(le$legend))le$legend <- ""
        legend(
            x=x,
            y=y,
            legend=le$legend,
            title=label,
#                title.col=le$lcol,
            cex = le$legend.text.size,
            bg = "white",
            lty = unlist(le$lty),
            lwd = unlist(le$lwd),
            pch = unlist(le$pch),
            col = unlist(le$col),
#                inset=0.1,
            text.col = "black",
            xpd=TRUE
#                merge = TRUE
            )
            # TODO: Warning: unlist coerces numeric colors to character!
    }
    #if(!is.null(lolegends)){
    if(!is.null(lolegends) && any(sapply(lolegends,function(lol)!is.null(lol)))){
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
#    }
    }
#    if(opa$log == "x") legend("top",legend=NA,title="Weibull",bg="white")
#    if(opa$log == "xy") legend("top",legend=NA,title="Lognormal",bg="white")
#    if(opa$log %in% c("","y")) legend("top",legend=NA,title="xxx",bg="white")
    invisible()
        # TODO: return the abrem object with updated graphical options
        # TODO: check if this makes sense when supplying a list
}
