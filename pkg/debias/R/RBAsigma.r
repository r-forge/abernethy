## RBAsigma.r
## This is an implementation of the Reduced Bias Adjustement for Normal or Lognormal MLE sigma values
## for small samples. As Dr. Abernethy explains, for these symetrical distributionsthe mean and median bias are the same.
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

RBAsigma<-function(Nx)  {
## factorial(x) is simply gamma(1+x)
	num<-gamma(1+(Nx-2)/2)	
	den<-gamma(1+(Nx-3)/2)	
	C4<-sqrt(2/(Nx-1))*num/den	
return(sqrt(Nx/(Nx-1))/C4)		
}		
