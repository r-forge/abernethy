objectlist <- ls() # for removing unused demo objects later
abrem.defaults   <- options.abrem()
# +-----------------------------------------------------------------------------
options.abrem(sub="https://r-forge.r-project.org/projects/abernethy",
    method.fit=c("rr","xony"),method.conf.blives="mcpivotals")
# +-----------------------------------------------------------------------------
data(p2.2,package="abrem")
da <- Abrem(p2.2)
da <- abrem.fit(da)
da <- abrem.conf(da)
da <- abrem.conf(da,method.conf.blives="bbb",lty=2)
plot(da,main="Problem 2.2 from 'The New Weibull Handbook', 5th edition\n")
#readline(prompt = "Hit <RETURN> to see next plot.\n")
# +-----------------------------------------------------------------------------
if(require(boot)){
    data(aircondit,aircondit7,package="boot")
    da1  <- abrem.fit(
        Abrem(time=aircondit$hours,label="aircondit dataset, package \"boot\""))
    da2  <- abrem.fit(
        Abrem(time=aircondit7$hours,label="aircondit 7 dataset",col="red",pch=4))
    plot.abrem(list(da1,da2),xlim=c(0.01,1e5),
        main="aircondit dataset\n",
        xlab="Time To failure (hours)")
}
# +-----------------------------------------------------------------------------
if(require(boot)){
    data(hirose,package="boot")
    names(hirose)  <- c("volt","time","event")
    da <- list()
    hc <- rev(rainbow(4,end=4/6))
    # create temperature-like colors
    da[[1]] <- abrem.fit(Abrem(subset(hirose,volt==5),
        label="Voltage= 5 [V]",col=hc[1],pch=0))
    da[[2]] <- abrem.fit(Abrem(subset(hirose,volt==7),
        label="Voltage= 7 [V]",col=hc[2],pch=1))
    da[[3]] <- abrem.fit(Abrem(subset(hirose,volt==10),
        label="Voltage= 10 [V]",col=hc[3],pch=2))
    da[[4]] <- abrem.fit(Abrem(subset(hirose,volt==15),
        label="Voltage= 15 [V]",col=hc[4],pch=5))

    def <- options.abrem()
    options.abrem(legend.text.size=0.5,xlim=NULL,
        main="\'Hirose\' dataset (package \'boot\')\n")

    lapply(da,plot.abrem)
    plot.abrem(da,xlim=c(1,1e6))
#    legend("topright",c("5 [V]","7 [V]","10 [V]","15 [V]"),
#        title="Test voltages:",bg="white",inset=0.05,
#        cex=0.7,col=hc,lty=1,lwd=2)
    invisible(options.abrem(def))
}
# +-----------------------------------------------------------------------------
if(require(MASS)){
#    def <- options.abrem()
#    data(motors,package="MASS")
#    names(motors) <- c("temp","time","event")
#    mo <-  list()
#    mo[[1]] <- abrem.fit(Abrem(subset(motors,temp==150),
#        label="Temp = 150"))
#    mo[[2]] <- abrem.fit(Abrem(subset(motors,temp==170),
#        label="Temp = 170"))
#    mo[[3]] <- abrem.fit(Abrem(subset(motors,temp==190),
#        label="Temp = 190"))
#    mo[[4]] <- abrem.fit(Abrem(subset(motors,temp==220),
#        label="Temp = 220"))
#        # note that mo.150 has no failures,
#        # so it cannot be analysed by this
#        # package yet.
#    hc <- rev(rainbow(3,end=4/6))
#        # create temperature-like colors
#    mo.170 <- abrem.conf(abrem.fit(Abrem(time=mo.170$time,event=mo.170$status),
#        col=hc[1],pch=0))
#    mo.190 <- abrem.conf(abrem.fit(Abrem(time=mo.190$time,event=mo.190$status),
#        col=hc[2],pch=1))
#    mo.220 <- abrem.conf(abrem.fit(Abrem(time=mo.220$time,event=mo.220$status),
#        col=hc[3],pch=2))
#    plot(mo.170,xlim=c(1,1e5),main="\'Motors\' dataset (package \'MASS\')\n")
#    plot(mo.190,add=TRUE,is.plot.legend=TRUE)
#    plot(mo.220,add=TRUE)
#    legend("topleft",inset=0.05,bg="white",
#        c("temp = 170","temp = 190","temp = 220"),lwd=2,col=hc[1:3])
#    options.abrem(def)
}
# +-----------------------------------------------------------------------------
data(p2.2,package="abrem")

da <- abrem.fit(Abrem(p2.2),dist="weibull")
da <- abrem.fit(da,dist="weibull3p", method.fit=c("rr","xony"),col="green3")
da <- abrem.fit(da,dist="lognormal", method.fit=c("rr","yonx"),col="red")

plot(da,main="Comparison between Weibull (2P and 3P)  and Lognormal 2P\n")
#legend("topleft",c("curve (Surv) using MLE"),lwd=2,bg="white",cex=0.7,
#   col="red",inset=0.025)
# +-----------------------------------------------------------------------------
#data(p13.6,package="abrem")
#da <- abrem.fit(Abrem(p13.6),dist="weibull",method.fit=c("mrr","xony","qbeta"))
#for(i in 1:20){
#    da <- abrem.conf(da,method.conf.blives="mcpivotals",S=1000,col="red",lwd=1)
#}
#options.abrem(is.plot.legend=FALSE)
#m <- "Variability in \"mcpivotals\" B-life confidence for S=1000.\n"
#plot(da,xlim=c(10,500),main=m)

# +-----------------------------------------------------------------------------
#da <- params.to.ob("weibull",beta=3,eta=1000,event=c(1,1,1,rep(0,10)))
#da <- abrem.fit(Abrem(da),dist="weibull",method.fit=c("mrr","xony","qbeta"))
#for(i in 1:20){
#    da <- abrem.conf(da,method.conf.blives="mcpivotals",S=1000,col="red",lwd=1)
#}
#options.abrem(is.plot.legend=FALSE)
#m <- "Variability in \"mcpivotals\" B-life confidence for S=1000.\n"
#plot(da,xlim=c(5,5000),main=m)
## +-----------------------------------------------------------------------------
#da <- params.to.ob("weibull",beta=3,eta=1000,event=c(1,1,1,rep(0,10)))
#da <- abrem.fit(Abrem(da),dist="weibull",method.fit=c("mrr","xony","qbeta"))
#for(i in 1:10){
#    da <- abrem.conf(da,method.conf.blives="mcpivotals",S=1e5,col="green3",lwd=1)
#}
#options.abrem(is.plot.legend=FALSE)
#m <- "Variability in \"mcpivotals\" B-life confidence for S=1e5.\n"
#plot(da,xlim=c(5,5000),main=m)
# +----------------------------------------------------------------------------
#data(p13.6)
#d <- Surv(p13.6$time,p13.6$event)
#plot.wb(d,CL=0.99,legend.position="topleft",
#	main="Relation between confidence levels, upper and lower bounds.\n")
#   options.wb(add=TRUE)
#plot.wb(d,CL=0.90,col="red",legend.position="bottomleft")
##plot.wb(d,CL=0.95,sides="lower",col="blue",legend.position="topright")
##plot.wb(d,CL=0.95,sides="upper",col="green",legend.position="bottomright")
## +----------------------------------------------------------------------------
invisible(options.abrem(abrem.defaults))
   # resetting the all abrem options.
rm(list = ls()[!(ls() %in% objectlist)])
   # removing demo objects
