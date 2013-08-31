##  MLEw2pBounds.r
##
##  This function generates log(Eta) values corresponding to an input Blives vector
##  for the lower and upper likelihood ratio confidence bounds along with Datum points
##  in a format like that used by function pivotals::CBpiv.  The confidence bounds are constructed
##  on the Weibull plot from a type of pivotal analysis of the points on the likelihood ratio contour.
##  The likelihood contour is obtained by execution of MLEw2pContour.  A call to this function may
##  precede the call to MLEw2pBounds for purposes of storing and/or viewing the contour, so an optional argument
##  has been provided to accept the output list from such a call, but is performed without error checking.
##  Optionally this function may call for implementation of the "JLF bias adjustment" to the contour 			
##  as postulated in separate papers by Dr. Abernethy and Wes Fulton, but only to the extent that the
##  call to MLEw2pContour will be initiated from this function code (not by-passed by direct input of the
##  MLEcontour list output).
##  Comparisons with test cases on SuperSMITH software have proven exact replication with
##  degrees of freedom set at 1 for the Chi-squared statistic.  Subsequent discussion with statistics
##  specialist Gianluca Bonitta have resolved that DF=1 is appropriate for this type of constrained model 
##  used for likelihood ratio bounds.
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

MLEw2pBounds<-function(x, s=NULL, CL=0.9, Blives=(c(1,5,10,20,30,40,50,60,80,90,95,99)/100),  MLEcontour=NULL, debias=FALSE,show=FALSE)  {					
					
	if(missing(MLEcontour))  {				
		MLEcontour<-MLEw2pContour(x, s, CL,debias=debias)			
	}				
					
	ypts<-log(qweibull(Blives,1,1))				
		i=1			
		j=1			
					
	yval<-c(Blife=ypts)				
					
	Eta<-MLEcontour[[i]][j,1]				
	Beta<-MLEcontour[[i]][j,2]				
		xvals=NULL			
	for(k in 1:length(ypts) ) {				
					
		xval<-ypts[k]/Beta+log(Eta)			
		xvals<-c(xvals,xval)			
		names(xvals)<-NULL			
	}				
					
	outmat<-rbind(yval,xlo=xvals, Eta=rep(Eta,length(ypts)),Beta=rep(Beta,length(ypts)),				
		xhi=xvals, Eta=rep(Eta,length(ypts)),Beta=rep(Beta,length(ypts)))			
	for(i in 1:4)  {				
			clen=length(MLEcontour[[i]][,1])		
		for(j in 1:clen)  {			
			Eta<-MLEcontour[[i]][j,1]		
			Beta<-MLEcontour[[i]][j,2]		
				xvals=NULL	
			for(k in 1:length(ypts) ) {		
					
				xval<-ypts[k]/Beta+log(Eta)	
					
				if(xval<outmat[2,k])  {	
					outmat[2,k]=xval
					outmat[3,k]=Eta
					outmat[4,k]=Beta
				}	
				if(xval>outmat[5,k])  {	
					outmat[5,k]=xval
					outmat[6,k]=Eta
					outmat[7,k]=Beta
				}	
			}		
		}			
	}				
					
## calculate the Datum vector					
	MLEfit<-MLEw2p_cpp(x,s)				
	Eta<-MLEfit[1]				
	Beta<-MLEfit[2]				
		xvals=NULL			
	for(k in 1:length(ypts) ) {				
					
		xval<-ypts[k]/Beta+log(Eta)			
		xvals<-c(xvals,xval)			
		names(xvals)<-NULL			
	}				
					
	outDF<-data.frame(ypts=ypts,Lower=outmat[2,],Datum=xvals, Upper=outmat[5,])				
					
	if(show==TRUE)  {				
		plot(xvals,ypts,type="l")			
		lines(outmat[2,],outmat[1,],col="red")			
		lines(outmat[5,],outmat[1,],col="blue")			
	}				
					
outDF					
}					

