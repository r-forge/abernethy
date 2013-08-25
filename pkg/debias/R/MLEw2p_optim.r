## MLEw2_optim.r
## This is a direct implemenetation of the function calls made by package surv to calculate
## the MLE for the 2-parameter Weibull distribution.  The key call is to stats::optim, which
## implements the Nelder-Mead Simplex algorithm to optimize for the two parameters simultaneously.
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

MLEw2p_optim<-function(x, s=NULL, MRRfit=NULL)  {		
##  This is the negative log-likelihood function we will optimize		
	minusLLweibull2<-function(par,x,s)  {	
		suscomp<-0
		failcomp<- (-1)*sum(dweibull(x,par[1],par[2],log=TRUE))
		if(length(s)>0)  {
		suscomp<- (-1)*sum(pweibull(s,par[1],par[2],lower.tail=FALSE,log.p=TRUE))
		}
	return(failcomp+suscomp)	
	}	
		
if(missing(MRRfit))  {		
## a starting position is simply constructed here (quicker than MRR)		
	data<-c(x,s)	
	            m <- mean(log(data))	
	            v <- var(log(data))	
	            shape <- 1.2/sqrt(v)	
	            scale <- exp(m + 0.572/shape)	
	            vstart <- c(shape, scale)	
}else{
				vstart<- c(MRRfit[2],MRRfit[1])
}		
	optout<-optim(vstart,minusLLweibull2,x=x,s=s)	
		
	outvec<- c(Eta=optout$par[2], Beta=optout$par[1], LL=-optout$value)	
		
outvec		
}		
