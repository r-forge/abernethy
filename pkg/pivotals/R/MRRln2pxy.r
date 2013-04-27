MRRln2pxy<-function(x,s=NULL)  {			
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
 Lognormal<-.Call("MRRln2pXonY", data, event, PACKAGE= "pivotals")  			
## perhaps a future output would be more friendly     			
## outputDF<-data.frame(Lognormal)			
## DFrows<-c("Eta","Beta","Rsquared") 	     		
## row.names(outputDF)<-DFrows			
## outputDF			
## but for testing purposes just the vector from MRRw2pXonY is needed			
Lognormal			
}			
