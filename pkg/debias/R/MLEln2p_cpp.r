## MLEln2_cpp.r
## This is  a wrapper function calling a C++ implementation of the MLE optimization obtained by the 
## direct Nelder-Meade simplex method optimizing the negative log-likelihood function for the lognormal 
## 2-parameter distribution consistent with the default R function optim. This code has been streamlined 
## for the simplest 2 parameter case, rather  than built in a generalized format for the solution of more 
## than 2 parameters at one time. 
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
## Althought The Weibull Handbook, Fifth Edition does not demonstrate this calculation, this function 
## is consistent with SuperSMITH software.

MLEln2p_cpp<-function(x, s=NULL,MRRfit=NULL)  {			
			
if(missing(MRRfit))  {			
## a starting position is simply constructed here (quicker than MRR)			
	data<-c(x,s)		
	ndata <- length(data)
	ldata <- log(data)
	sd0 <- sqrt((ndata - 1)/ndata) * sd(ldata)
	ml <- mean(ldata)
	vstart <- c(meanlog=ml, sdlog=sd0)
	
}else{			
	vstart<-MRRfit[2]		
}

Data<-c(x,s)
Nf<-length(x)
limit<-1.0e-6


result<-.Call("MLEln2p",Data,Nf,vstart,limit,PACKAGE="debias")
nr<-length(result[,1])
outvec<-c(Mulog=result[nr,1],Sigmalog=result[nr,2],negLL=result[nr,3])

outvec
}
