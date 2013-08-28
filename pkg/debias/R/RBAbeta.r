## RBAbeta.r
## This is an implementation of the Reduced Bias Adjustement for Weibull MLE beta on small samples.
##  As Dr. Abernethy explains, the prefereed basis is the MEDIAN bias (C4^3.5).  Many other sources 
## refer to correction for MEAN bias (C4^6).
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

RBAbeta<-function(Nx,basis="median")  {
## factorial(x) is simply gamma(1+x)
	num<-gamma(1+(Nx-2)/2)	
	den<-gamma(1+(Nx-3)/2)	
	C4<-sqrt(2/(Nx-1))*num/den	
	if(basis=="mean")  {
	return(C4^6)
	}else{
    return(C4^3.5)	
    }	
}		
