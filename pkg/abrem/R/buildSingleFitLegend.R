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
buildSingleFitLegend <- function(fit,opadata,...){
    arg <- list(...)
    if(!is.null(fit$options)){
        opafit <- modifyList(opadata,fit$options)
    }else{opafit <- opadata}
    opafit <- modifyList(opafit,list(...))

    si <- function(number)signif(number,opafit$signif)
        # TODO: opafit or oppconf?
        # shorter writing form for signif()
    li <- list()
#    li[[10]]$legend <- paste0("(",opafit$xlab," ; ",opafit$ylab,")")
#        # TODO: opafit, opp or oppdata?
    li[[10]]    <- bsll(legend=paste0("ranks = ",opafit$pp[1]),
        col=opadata$col,pch=opadata$pch,lwd=opadata$lwd.points)
        # TODO: add support for more pp?
    li[[20]]    <- bsll(legend = paste0(fit$options$dist," (",
        paste0(fit$options$method.fit,collapse=", "),")"),
        col=opafit$col,lwd=opafit$lwd,lty=opafit$lty)
    li[[30]]    <- bsll(legend=ifelse(is.null(fit$rate),NA,
            paste0("rate = ",si(fit$rate))))
    li[[40]]    <- bsll(legend=ifelse(is.null(fit$meanlog),NA,
            paste0("mean(-log) = ",si(exp(fit$meanlog))," (",
            si(fit$meanlog),")")))
    li[[50]]    <- bsll(legend=ifelse(is.null(fit$sdlog),NA,
            paste0("sd(-log) = ",si(exp(fit$sdlog))," (",
            si(fit$sdlog),")")))
    li[[60]]    <- bsll(legend=ifelse(is.null(fit$beta),NA,
            paste0("beta = ",si(fit$beta))))
    li[[70]]    <- bsll(legend=ifelse(is.null(fit$eta),NA,
            paste0("eta = ",si(fit$eta))))
    li[[80]]    <- bsll(legend=ifelse(is.null(fit$t0),NA,
            paste0("t0 = ",si(fit$t0))))
    li[[90]]    <- bsll(legend=paste0("n (fail | cens.) = ",fit$n,
            " (",fit$fail," | ",fit$cens,")"))
    if(!is.null(fit$gof) && opafit$is.legend.gof){
        if(!is.null(fit$gof$r2)){
            if(!is.null(fit$gof$ccc2)){
                li[[100]]    <- bsll(legend=paste0("r^2 | CCC^2 = ",
                    si(fit$gof$r2)," | ",si(fit$gof$ccc2),
                    ifelse(fit$gof$r2>=fit$gof$ccc2," (good)"," (BAD)")))
            }else{
                li[[100]]    <- bsll(legend=paste0("r^2 = ",si(fit$gof$r2)))
            }
        }
        if(!is.null(fit$gof$loglik)){
            li[[110]]    <- bsll(legend=paste0("loglik = ",si(fit$gof$loglik)))
        }
        li[[120]]    <- bsll(
            legend=ifelse(is.null(fit$gof$prr),NA,
                paste0("prr = ",si(fit$gof$prr)," (S=",
                ifelse(is.null(fit$gof$S),"NA",fit$gof$S),")")))
    }
    #leconfpos <- length(na.omit(unlist(li))) + 1
        # where displaying confidence info begins
    leconf <- legendconf(fit,"blives",opadata=opadata,...)
    if(!is.null(leconf)) li[[130]]    <- bsll(legend="")
    li <- c(li,leconf)
    #li[[13]] <- lapply(leconf,function(z)z[[1]])
        # extract the actual legend lines

    #li[[12]] <- legendconf(fit,"params",opa,opafit,oppdata)
        # TODO: check if all the above args are actually needed
    removeBadLegendEntries <- function(e){
        if(!is.null(e))!is.na(e$legend) else FALSE
    }
    li <- li[sapply(li,removeBadLegendEntries)]
        # remove list items where the legend text = NA
    fu  <- function(x,i){if(i %in% names(x))x[[i]]}
    fu2 <- function(i,x){lapply(x,fu,i=i)}
    items <- c("legend","lty","lwd","pch","col")
    le  <- lapply(items,fu2,li)
    names(le) <- items
    if(identical(label <- opafit$label,""))label <- NULL
    le$rect <- legend(
        "bottomright",
#                "topright",
        legend=le$legend,
        title=label,
        cex = opafit$legend.text.size,
#        inset=0.1,
#        merge = TRUE,
        plot=FALSE)$rect
    le$label <- opafit$label
    le$legend.text.size <- opafit$legend.text.size
    le
}
