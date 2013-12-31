## HRBU.r
## This is an implementation of the Hirose and Ross Beta Unbias functons for Weibull MLE on small samples.
##  These functions are used by ReliaSoft's Weibull++ product as presented in ReliaSoft Corporation,
##  Life Data Analysis Reference, Tucson, AZ: ReliaSoft Publishing, 2005.
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

HRBU<-function(Nx,Ns=NULL)  {
	r<-Nx
	n<-r+Ns
    if (length(Ns)==0) {
## This is the Hirose Beta Unbias factor for complete failure samples	
	BU<-1/( 1.00115+1.278/r+2.001/r^2+20.35/r^3-46.98/r^4 )
	}else{
## This is the Ross Beta Unbias factor for samples with suspensions	
	BU<- 1/(1+1.37/(r-1.92)*sqrt(n/r))
	}
BU	
}	


