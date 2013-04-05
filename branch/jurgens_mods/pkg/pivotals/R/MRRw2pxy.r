## MRRw2pxy.r
## This is a wrapper function intended only for testing the MRRw2pXonY function in the
## compiled shared library in package pivotals.
## A bit of flexibility was added to permit more casual input of data, with correct
## entry of arguments generated for MRRw2pXonY()
## 
## (C) Jacob Ormerod 2013
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

MRRw2pxy<-function(x,s=NULL)  {
  require(pivotals)
  if(missing(s)) {
## this is simply a complete failure set
    data<-sort(x)
    event<-rep(1,length(x))
  }else{
## suspension data has been provided
    data<-c(x,s)
    event<-c(rep(1,length(x)),rep(0,length(s)))
    prep_df<-data.frame(data=data,event=event)
## now sort the dataframe on data values
    NDX<-order(prep_df[,1])
    prep_df<-prep_df[NDX,]
    data<-prep_df$data
    event<-prep_df$event
  }
  Weibull<-.Call("MRRw2pXonY", data, event, PACKAGE= "pivotals")  
## perhaps a future output would be more friendly     
## outputDF<-data.frame(Weibull)
## DFrows<-c("Eta","Beta","Rsquared") 	     
## row.names(outputDF)<-DFrows
## outputDF
## but for testing purposes just the vector from MRRw2pXonY is needed
Weibull
}

  
