## MLEw2_abrem.r
## This is an implementation of the MLE optimization obtained by identifying the root of the derivative
## with respect to Beta of the Weibull 2-parameter likelihood function as presented in The Weibull Handbook, Fifth Edition,
## by Robert B. Abernethy.
## 
## (C) Jacob T. Ormerod 2013
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
##
## This function is consistent with The Weibull Handbook, Fifth Edition and SuperSMITH software.

MLEw2p_abrem<-function(x, s=NULL,MRRfit=NULL,limit=1.0e-6, listout=FALSE)  {			
					
## We solve for the root of this function (when G(x,s,Bhat)==0)			
G<-function(x,s,Bhat)  {			
	fps<-c(x,s)		
	num<-sum(fps^Bhat*log(fps))		
	den<-sum(fps^Bhat)		
	num/den-sum(log(x))/length(x)-1/Bhat		
}			
			
if(missing(MRRfit))  {			
## a starting position is simply constructed here (quicker than MRR)			
## a starting position is simply constructed here (quicker than MRR)			
##	data<-c(x,s)
## temporary fix for Joni Ainasoja at Kone in Finland/(and Sweden?)
			data<-x
	            v <- var(log(data))		
	            shape <- 1.2/sqrt(v)		
	            vstart <-shape			
}else{			
	vstart<-MRRfit[2]		
}			
			
		DL<-limit	
##  First step can be quite small			
		DX<-0.02	
		X0<-vstart[1]	
		istep<-0	
		X1<-X0+DX	
		outDF<-data.frame(steps=istep,root=X0,error=DX,GB=G(x,s,X0))	
			
	while(abs(DX)>DL)  {		
		GX0<-G(x,s,X0)	
		GX1<-G(x,s,X1)	
			
		D<- GX1-GX0	
		X2<-X1-(X1-X0)*GX1/D	
		X0<-X1	
		X1<-X2	
		DX<-X1-X0	
		istep<-istep+1	
			
		DFline<-data.frame(steps=istep,root=X0,error=DX,GB=GX1)	
		outDF<-rbind(outDF,DFline)	
	}		
			
	Eta<-(sum(c(x,s)^X0)/length(x))^(1/X0)		
## Now calculate the negative log-likelihood			
	suscomp<-0		
	failcomp<- (-1)*sum(dweibull(x,X0,Eta,log=TRUE))		
	if(length(s)>0)  {		
	suscomp<- (-1)*sum(pweibull(s,X0,Eta,lower.tail=FALSE,log.p=TRUE))		
	}		
	negLL<-failcomp+suscomp		
			
	outvec<-c(Eta=Eta,Beta=X0,LL=-negLL)
	if(listout==TRUE)  {
		outlist<-list(outvec,outDF)
		return(outlist)
	}else{
		return(outvec)
	}
			
}			
