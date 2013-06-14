## MRRln2pyx.r
## This is a wrapper function intended only for testing the MRRw2pYonX function in the
## compiled shared library in package pivotals.
## A bit of flexibility was added to permit more casual input of data, with correct
## entry of arguments generated for MRRw2pXonY()
## 
## (C) Jacob Ormerod 2013
##
## This program is free software; you can redistribute it and/or modify ittest
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

MRRln2pyx<-function(x, options=NULL)  {	
  
if(!identical(class(x),"abrem")){	
	stop("input is not of class abrem")
}
  
  ## Default point estimation method for pivotals package is Benard's approximation for median ranks			
	method=0		
	if(!missing(options))  {		
## An example means for testing the options.abrem list to be passed from abrem package			
	    if(length(options$method.fit)>0) {		
			if(options$method.fit[2]=="qbeta"){	
				method=1
			}	
	    }		
	}
  
  Lognormal<-.Call("MRRln2pYonX", x$data$time, x$data$event, method, PACKAGE= "pivotals")  
  params<-c("Mulog","Sigmalog","R_squared") 
  names(Lognormal)<-params
Lognormal
}

  
