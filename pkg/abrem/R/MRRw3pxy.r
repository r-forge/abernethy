## This is a final re-work of the secant method that I had early hopes for, but had found a problem
## with the Wear Out distribution of f2c data.  It really only took a few items to repair.
## The t0 must be constrained on any test to be less than min(x).
## The two point derivative indicates direction of the step.  So differences between X1 and X0
## as well as differences between F(X1) and F(X0) need to be taken as absolute.
## finally I applied a 100 iteration maximum that some unstable data sets are known to hit.
## In those cases the t0 is trying to be extraordinarily large negative, while beta progresses to 
## extraoridarily large betas.  The final result of the unstable data is that the 3p fit is not to be accepted.

## This algorithm will be ported to C++ for fast execution.




MRRw3pxy<-	function (x, s = NULL,limit=10^-5, options=NULL,listout=FALSE)  {		
## two-point derivative function			
F<-function(X,data,event,limit)  {			
	fit1<-.Call("MRRw2pXonY", (data-(X)), event, PACKAGE = "pivotals")		
	fit2<-.Call("MRRw2pXonY",(data-(X-(0.1*limit))), event, PACKAGE = "pivotals")		
	dR2dx<-(fit1[3]-fit2[3])/(0.1*limit)		
	outlist<-list(dR2dx=dR2dx,fit=fit1)		
	return(outlist)		
}			
if(any(x<0))  {stop("negative values in failure data")}			
	    if (missing(s)) {		
		data <- sort(x)	
		event <- rep(1, length(x))	
	    }else{		
		data <- c(x, s)	
		event <- c(rep(1, length(x)), rep(0, length(s)))	
		prep_df <- data.frame(data = data, event = event)	
		NDX <- order(prep_df[, 1])	
		prep_df <- prep_df[NDX, ]	
		data <- prep_df$data	
		event <- prep_df$event	
	    }				
			
## This is the secant method.			
##  Tao Pang's original variable labels from FORTRAN are used where possible			
			DL<-limit
## Introduce constraints for the 3p Weibull			
			C1<-min(x)
			maxit<-100
			
## initial step is based on limit*10,000			
			DX<-limit*10^4
			X0<-0.0
			istep<-0
			X1<-X0+DX
			if(X1>C1) {X1<-X0+0.9*(C1-X0)}			
			fwd0<-F(X0,data,event,limit)
			fwd1<-F(X1,data,event,limit)
			FX0<-fwd0$dR2dx
			FX1<-fwd1$dR2dx
	## FX1 will contain slope sign information to be used only one time to find X2		
			D<- abs(FX1-FX0)
			X2<-X1+abs(X1-X0)*FX1/D
			if(X2>C1) {X2<-X1+0.9*(C1-X1)}
			X0<-X1
			X1<-X2
			DX<-X1-X0
			istep<-istep+1
	##  Detail output to be available with listout==TRUE		
	DF<-data.frame(steps=istep,root=X0,error=DX,deriv=FX1)		
		while(abs(DX)>DL&& istep<maxit)  {	
			FX0<-FX1
			fwd1<-F(X1,data,event,limit)
			
			FX1<-fwd1$dR2dx
			
	## FX1 will contain slope information only one time		
			D<- abs(FX1-FX0)
			X2<-X1+abs(X1-X0)*FX1/D
			if(X2>C1) {X2<-X1+0.9*(C1-X1)}			
			X0<-X1
			X1<-X2
			DX<-X1-X0
			istep<-istep+1			
			DFline<-data.frame(steps=istep,root=X0,error=DX,deriv=FX1)
			DF<-rbind(DF,DFline)
		}	
			
		outvec<-c(Eta=fwd1$fit[1],Beta=fwd1$fit[2],t0=X0,R2=fwd1$fit[3])	
			
		if(listout==TRUE)  {	
			outlist<-list(outvec,DF)
			return(outlist)
		}else{	
			return(outvec)
		}	
	}		
