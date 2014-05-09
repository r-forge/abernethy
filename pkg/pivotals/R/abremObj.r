## abremObj.r
## This is a temporary function to permit creation of an abrem class object within pivotals for testing purposes.
## The abrem object is formed from a vector of complete failures, or alternatively individual vectors for failures
## and suspensions.  MRR functions in pivotals will only accept an object of class abrem.  This function is expected
## to be depreciated in favor of function abrem::Abrem.
## 
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

abremObj<-function(x, s=NULL)  {		
	    ret <-list()	
	    class(ret) <- "abrem"	
		  if(missing(s)) {
	## this is simply a complete failure set	
	    time<-sort(x)	
	    event<-rep(1,length(x))	
		ret$data$time<-time
		ret$data$event<-event
	  }else{	
	## suspension data has been provided	
	    time<-c(x,s)	
	    event<-c(rep(1,length(x)),rep(0,length(s)))	
		timeorder <- order(time)
		ret$data$time<-time[timeorder]
		ret$data$event<-event[timeorder]
	  }	
	  ret	
  }		
