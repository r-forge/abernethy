## MLEw3p_secant
##
## This is a prototype function pending port to C++ using RcppArmadillo.
## This function optimizes the MLE for the three parameter Weibull distribution
## for a given dataset.  Data may contain both failures and suspensions.
## The method of optimization is similar to the best method found to optimize the
## R_squared from MRR fitting for the third parameter.  A discrete Newton method, also called
## the secant method is used to identify the root of the derivative of the MLE~t0 function.
## In this case the derivative is numerically determined by a two point method.
## The 3-parameter Weibull MLE optimization is known to present instability with some
## data.  This is a particular challenge that this routine has been designed to handle.
## It is expected that when instability is encountered the function will terminate gracefully
## at the point at which MLE fitting calculations fail, providing output of last successful 
## calculation.
## 
## (C) David Silkworth 2013
##
## This program is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by the
## Free Software Foundation; either version 2, or (at your option) any
## later version.
##
## These functions are distributed in the hope that they will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
## GNU General Public License for more details.
##
##  You should have received a copy of the GNU General Public License
##  along with this program; if not, a copy is available at
##  http://www.r-project.org/Licenses/

require(debias)

MLEw3p_secant<-	function (x, s = NULL,limit=10^-5,listout=FALSE)  {		
## two-point derivative function			
dLLdx<-function(data,Nf,vstart,limit)  {			
			
	fit1<-.Call("MLEw2p", data, Nf,c(vstart, limit), PACKAGE = "debias")		
			
	fit2<-.Call("MLEw2p", data+0.1*limit, Nf,c(vstart, limit), PACKAGE = "debias")		
## must remove (-1) factor when MLEw2p returns LL			
	dLLdx<-(-1)*(fit1[3]-fit2[3])/(0.1*limit)		
	attr(dLLdx,"fitpar")<-fit1		
			
	dLLdx 		
}			
#	## a starting position is simply constructed here (quicker than MRR?)		
		data<-c(x,s)		
		            v <- var(log(data))	
		            shape <- 1.2/sqrt(v)			
		            vstart <- shape	
					Nf<-length(x)	
			
		warning=FALSE	
			
##  Tao Pang's original variable labels from FORTRAN are used where possible			
			DL<-limit
## Introduce constraints for the 3p Weibull			
			C1<-min(x)	
			maxit<-100
			
## initial step is based on min(x)*.1			
			DX<-C1*0.1
			X0<-0.0
			istep<-0
			X1<-X0+DX
			if(X1>C1) {X1<-X0+0.9*(C1-X0)}
			
			FX0<-dLLdx(data,Nf,vstart,limit)
## introduce a new start estimate based on last fit
			vstart<-attributes(FX0)[[1]][2]*0.5
## modify data by X1 for next slope reading			
			ms<-NULL
			mx<-x-X1
			if(length(s)>0)  {
			for(i in 1:length(s) )  {
			if((s[i]-X1)>0 )  {ms<-c(ms,s[i]-X1)}
			}
			}
			FX1<-dLLdx(c(mx,ms),Nf,vstart,limit)
			vstart<-attributes(FX1)[[1]][2]*0.5
			
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
		while(abs(DX)>DL&&istep<maxit)  {	
			FX0<-FX1
## modify data by X1 for next slope reading			
			ms<-NULL
			mx<-x-X1
			if(length(s)>0)  {
			for(i in 1:length(s) )  {
			if((s[i]-X1)>0 )  {ms<-c(ms,s[i]-X1)}
			}
			}
			FX1<-dLLdx(c(mx,ms),Nf,vstart,limit)
			if(is.nan(FX1))  {
			FX1<-FX0
			warning=TRUE
			break
			}
			vstart<-attributes(FX1)[[1]][2]*0.5
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
			
		fit<-attributes(FX1)[[1]]	
			
## must remove (-1) factor when MLEw2p returns LL			
		outvec<-c(Eta=fit[1],Beta=fit[2],t0=X0,LL=(-1)*fit[3])	
		if(warning==TRUE)  {	
			warn="optimization unstable"
			attr(outvec,"warning")<-warn
		}	
		if(listout==TRUE)  {	
			outlist<-list(outvec,DF)
			return(outlist)
		}else{	
			return(outvec)
		}	
	}		
