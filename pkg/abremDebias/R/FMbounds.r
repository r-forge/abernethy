FMbounds<-function(x, dist="weibull", CL=.95, unrel=NULL, debias=NULL, show=FALSE)  {	
	if(length(unrel)>0)  {
	dq<-unrel
	}else{
	## these descriptive quantiles match Minitab unchangeable defaults
	dq=c(seq(.01,.09,by=.01),seq(.10,.90,by=.10),seq(.91,.99, by=.01))
	}
	K<-qnorm(1-(1-CL)/2)
	##  validity checking of arguments will be performed in mlefit
	fit<-mlefit(x, dist=dist)
	
	if(tolower(dist)=="weibull" || tolower(dist)=="weibull2p")  {
		shape<-fit[2]
		scale<-fit[1]
		hessian<-optimHess(c(shape,scale),abremLoglike, x=x, dist="weibull", sign=-1)
		V<-solve(hessian)
		yp<-log(log(1/(1-dq)))
		xp<-scale*(log(1/(1-dq)))^(1/shape)
		Vt<-V[2,2]/scale^2+yp^2*V[1,1]/shape^4-2*yp*V[1,2]/(shape^2*scale)
		xp<-scale*(log(1/(1-dq)))^(1/shape)
		Lb<-log(scale)+yp/shape-K*sqrt(Vt)
		Ub<-log(scale)+yp/shape+K*sqrt(Vt)
		
		if(show==TRUE)  {
			plot(log(xp),yp, type="l")
			lines(Lb,yp, col="red")
			lines(Ub,yp, col="blue")
		}
	}
	
	if(tolower(dist)=="lognormal" || tolower(dist)=="lognormal2p")  {
		meanlog<-fit[1]
		sdlog<-fit[2]
		hessian<-optimHess(c(meanlog,sdlog),abremLoglike, x=x, dist="lognormal", sign=-1)
		V<-solve(hessian)
		yp<-qnorm(dq,0,1)
		Vt<-V[1,1] + yp^2*V[2,2] + 2*yp*V[1,2]
		lnxp<-yp*sdlog+meanlog
		Lb<-lnxp-K*sqrt(Vt)
		Ub<-lnxp+K*sqrt(Vt)
		xp<-exp(lnxp)
		
		if(show==TRUE)  {
			plot(lnxp,yp, type="l")
			lines(Lb,yp, col="red")
			lines(Ub,yp, col="blue")
		}
	}
	
	outDF<-data.frame(percentile=dq*100, lower=exp(Lb), datum=xp, upper=exp(Ub))
	return(outDF)
}	
