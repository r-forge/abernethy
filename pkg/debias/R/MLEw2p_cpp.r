## MLEw2_cpp.r
## This is  a wrapper function calling a C++ implementation of the MLE optimization obtained by identifying the root of the
## derivative with respect to Beta of the Weibull 2-parameter likelihood function as presented in The Weibull Handbook,
## Fifth Edition, by Robert B. Abernethy.
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

MLEw2p_cpp<-function(x, s=NULL,MRRfit=NULL)  {			
			
if(missing(MRRfit))  {			
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

Data<-c(x,s)
Nf<-length(x)
limit<-1.0e-6
controlvec<-c(vstart,limit)

resultvec<-.Call("MLEw2p",Data,Nf,controlvec,PACKAGE="debias")

outvec<-c(Eta=resultvec[1],Beta=resultvec[2],LL=resultvec[3])

outvec
}
