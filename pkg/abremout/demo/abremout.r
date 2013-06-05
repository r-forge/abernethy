objectlist <- ls() # for removing unused demo objects later
abrem.defaults   <- options.abrem()
abrem.plot.defaults   <- options.abremplot()
# +-----------------------------------------------------------------------------
options.abremplot(sub="https://r-forge.r-project.org/projects/abernethy")
options.abrem(method.fit=c("mrr","xony","qbeta"),method.conf.blives="mcpivotal")
# +-----------------------------------------------------------------------------
da <- Abrem(params.to.ft("weibull",beta=3,eta=1000,n=5))
da <- abrem.fit(da,dist="weibull")
da <- abrem.conf(da)
plot(da)
# +-----------------------------------------------------------------------------
da <- Abrem(params.to.ft("weibull",beta=3,eta=1000,
    event=c(1,1,0,0,0,0,1,0,0,0)))
da <- abrem.fit(da,dist="weibull")
da <- abrem.conf(da)
plot(da)
# +-----------------------------------------------------------------------------
da  <- Abrem(params.to.ft("lognormal",meanlog=log(1000),sdlog=log(10),n=20))
    # TODO: check if calculation method for
    # lognormal time obserations is correct
da  <- abrem.fit(da,dist="lognormal",method.fit="surv",col="steelblue3")
da2 <- abrem.fit(da,dist="weibull")
plot(da)
plot(da2,add=TRUE,is.plot.legend=T,is.plot.fittedline=T)
# +-----------------------------------------------------------------------------
data(p2.2,package="abrem")
da <- Abrem(p2.2)
da <- abrem.fit(da,dist="weibull")
da <- abrem.conf(da,method.conf.blives=c("mcpivotal","bbb"))
plot(da,main="Problem 2.2 from 'The New Weibull Handbook', 5th edition\n")
#readline(prompt = "Hit <RETURN> to see next plot.\n")
# +-----------------------------------------------------------------------------
if(require(boot)){
    data(aircondit,aircondit7,package="boot")
    da1  <- abrem.fit(Abrem(time=aircondit$hours),
        legend.title="aircondit dataset")
    da2  <- abrem.fit(Abrem(time=aircondit7$hours),
        legend.title="aircondit 7 dataset",col="red")
    plot(da1,xlim=c(0.01,1e5),main="aircondit dataset\n")
    plot(da2,add=TRUE)
}
# +-----------------------------------------------------------------------------
if(require(boot)){
    data(hirose,package="boot")
    names(hirose)  <- c("volt","time","status")
    hi.5 <- subset(hirose,hirose$volt==5)
    hi.7 <- subset(hirose,hirose$volt==7)
    hi.10 <- subset(hirose,hirose$volt==10)
    hi.15 <- subset(hirose,hirose$volt==15)
    hc <- rev(rainbow(4,end=4/6))
        # create temperature-like colors
    def <- options.abremplot()
    options.abremplot(is.plot.legend=FALSE,is.plot.cb=FALSE)
    
    hi.5 <- abrem.fit(Abrem(time=hi.5$time,event=hi.5$status),
        col=hc[1],pch=0)
    hi.7 <- abrem.fit(Abrem(time=hi.7$time,event=hi.7$status),
        col=hc[2],pch=1)
    hi.10 <- abrem.fit(Abrem(time=hi.10$time,event=hi.10$status),
        col=hc[3],pch=2)
    hi.15 <- abrem.fit(Abrem(time=hi.15$time,event=hi.15$status),
        col=hc[4],pch=5)
    plot(hi.5,xlim=c(1,1e6),
        main="\'Hirose\' dataset (package \'boot\')\n")
    plot(hi.7,add=TRUE)
    plot(hi.10,add=TRUE)
    plot(hi.15,add=TRUE)
    legend("topright",c("5 [V]","7 [V]","10 [V]","15 [V]"),
        title="Test voltages:",bg="white",inset=0.05,
        cex=0.7,col=hc,lty=1,lwd=2)
    options.abremplot(def)
}
# +-----------------------------------------------------------------------------
if(require(MASS)){
    def <- options.abremplot()
    data(motors,package="MASS")
    names(motors) <- c("temp","time","status")
    mo.150 <- subset(motors,temp==150)
    mo.170 <- subset(motors,temp==170)
    mo.190 <- subset(motors,temp==190)
    mo.220 <- subset(motors,temp==220)
        # note that mo.150 has no failures,
        # so it cannot be analysed by this
        # package yet.
    hc <- rev(rainbow(3,end=4/6))
        # create temperature-like colors
    mo.170 <- abrem.conf(abrem.fit(Abrem(time=mo.170$time,event=mo.170$status),
        col=hc[1],pch=0))
    mo.190 <- abrem.conf(abrem.fit(Abrem(time=mo.190$time,event=mo.190$status),
        col=hc[2],pch=1))
    mo.220 <- abrem.conf(abrem.fit(Abrem(time=mo.220$time,event=mo.220$status),
        col=hc[3],pch=2))
    plot(mo.170,xlim=c(1,1e5),main="\'Motors\' dataset (package \'MASS\')\n")
    plot(mo.190,add=TRUE,is.plot.legend=TRUE)
    plot(mo.220,add=TRUE)
    legend("topleft",inset=0.05,bg="white",
        c("temp = 170","temp = 190","temp = 220"),lwd=2,col=hc[1:3])
    options.abremplot(def)
}
# +-----------------------------------------------------------------------------
data(p2.2,package="abrem")
da <- abrem.fit(Abrem(p2.2),dist="weibull",method.fit=c("mrr","xony","qbeta"))
da <- abrem.fit(da,dist="weibull",method.fit="surv",col="green3")
da <- abrem.fit(da,dist="lognormal",method.fit="surv",col="red")
plot(da,main="Comparison between Weibull (MRR and MLE) and Lognormal \n")
#legend("topleft",c("curve (Surv) using MLE"),lwd=2,bg="white",cex=0.7,
#   col="red",inset=0.025)
# +-----------------------------------------------------------------------------
#data(p13.6,package="abrem")
#da <- abrem.fit(Abrem(p13.6),dist="weibull",method.fit=c("mrr","xony","qbeta"))
#for(i in 1:20){
#    da <- abrem.conf(da,method.conf.blives="mcpivotal",S=1000,col="red",lwd=1)
#}
#options.abremplot(is.plot.legend=FALSE)
#m <- "Variability in \"mcpivotals\" B-life confidence for S=1000.\n"
#plot(da,xlim=c(10,500),main=m)

# +-----------------------------------------------------------------------------
da <- params.to.ft("weibull",beta=3,eta=1000,event=c(1,1,1,rep(0,10)))
da <- abrem.fit(Abrem(da),dist="weibull",method.fit=c("mrr","xony","qbeta"))
for(i in 1:20){
    da <- abrem.conf(da,method.conf.blives="mcpivotal",S=1000,col="red",lwd=1)
}
options.abremplot(is.plot.legend=FALSE)
m <- "Variability in \"mcpivotals\" B-life confidence for S=1000.\n"
plot(da,xlim=c(5,5000),main=m)
# +-----------------------------------------------------------------------------
da <- params.to.ft("weibull",beta=3,eta=1000,event=c(1,1,1,rep(0,10)))
da <- abrem.fit(Abrem(da),dist="weibull",method.fit=c("mrr","xony","qbeta"))
for(i in 1:10){
    da <- abrem.conf(da,method.conf.blives="mcpivotal",S=1e5,col="green3",lwd=1)
}
options.abremplot(is.plot.legend=FALSE)
m <- "Variability in \"mcpivotals\" B-life confidence for S=1e5.\n"
plot(da,xlim=c(5,5000),main=m)
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
invisible(options.abremplot(abrem.plot.defaults))
invisible(options.abrem(abrem.defaults))
   # resetting the all abrem options.
rm(list = ls()[!(ls() %in% objectlist)])
   # removing demo objects
