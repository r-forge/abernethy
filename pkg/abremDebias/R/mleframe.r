mleframe<-function(x, s=NULL, interval=NULL)  {				
				
				
## interval dataframe validation				
	colname_error<-FALSE			
	if(class(interval)=="data.frame")  {			
## test names in first two columns				
	test_names<-names(interval)			
		if(test_names[1] !="left") {		
			colname_error<-TRUE	
		}		
		if(test_names[2] !="right") {		
			colname_error<-TRUE	
		}		
				
## add qty column if not provided				
		if(ncol(interval)<3)  {		
			interval<- cbind(interval, qty=c(rep(1,nrow(interval))))	
		}else{		
## assure that a "qty" column exists (and is only extra column used)				
			if(test_names[3] != "qty")  {	
				colname_error<-TRUE
			}	
## strip any extraneous columns				
			interval<-interval[,1:3]	
		}		
	if(colname_error==TRUE) {			
		stop("column name error in interval dataframe object")		
	}			
				
## any additional validations, such as positive numeric checking				
## removal of potential na's, etc. could take place here				
	if(anyNA(interval))  {			
	stop("NA not handled in interval data")			
	}			
				
	if(any(c(interval$left,interval$right)<0)) {			
	stop("negative values in interval data")			
	}			
				
	if(any((interval$right-interval$left)<=0))  {			
	stop("non-positive interval")			
	}			
## sort to permit consolidation of any duplicated entries				
	NDX<-order(interval$left,interval$right)			
	interval<-interval[NDX,]			
				
## finally, reject any other object type but NULL				
	}else{			
		if(length(interval)>0)  {		
			stop("error in interval argument type")	
		}		
	}			
				
				
## now build dataframes for failures and suspensions				
## could x be a dataframe with time and event columns??				
	suspensions<-NULL			
	if(is.vector(x))  {			
		if(anyNA(x))  {		
		stop("NA in failure data")		
		}		
		if(any(x<=0))  {		
		stop("non-positive values in failure/occurrence data")		
		}		
				
		x<-sort(x)		
		failures<-data.frame(left=x,right=x,qty=rep(1,length(x)))		
				
		if(length(s)>0)  {		
		if(anyNA(s))  {		
		stop("NA  in suspension data")		
		}		
		if(any(s<=0))  {		
		stop("non-positive values in failure/occurrence data")		
		}		
		s<-sort(s)		
		suspensions<-data.frame(left=s,right=-1,qty=rep(1,length(s)))		
		}		
	}else{			
## here a time-event dataframe can be evaluated, if provided as x				
		stop("time-event dataframe not yet supported")		
	}			
	DF<-rbind(failures,suspensions,interval)			
## assure all integers in qty				
	DF$qty<-floor(DF$qty)			
	outDF<-DF[1,]			
	outline<-2			
	for(line in 2:nrow(DF))  {			
		if(DF[line,1]-DF[line-1,1]+DF[line,2]-DF[line-1,2]==0)  {		
			outDF[outline-1,3]<-DF[line,3]+DF[line-1,3]	
		}else{		
			outDF<-rbind(outDF,DF[line,])	
			outline<-outline+1	
		}		
	}			
	attr(outDF,"fsiq")<-TRUE			
				
return(outDF)				
}				
