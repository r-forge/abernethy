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
		stop("non-positive values in suspension data")		
		}		
		s<-sort(s)		
		suspensions<-data.frame(left=s,right=-1,qty=rep(1,length(s)))		
		}		
	}else{			
	## here a time-event dataframe can be evaluated, if provided as x				
	## This is the support for a time-event dataframe 
		if (class(x) == "data.frame") {
			test_names <- names(x)
			if (test_names[1] != "time") {
				colname_error <- TRUE
			}
			if (test_names[2] != "event") {
				colname_error <- TRUE
			}
			if (colname_error == TRUE) {
				stop("column name error in event dataframe object")
			}

	## verify positive time values
			if (anyNA(x$time)) {
				stop("NA in failure or suspension data")
			}
			if (any(x$time<= 0)) {
				stop("non-positive values in failure or suspension data")
			}
	## verify 1's and 0's only in event
	## using Jurgen's validation code
			ev_info <- levels(factor(x$event))
			if(identical(ev_info,c("0","1")) || identical(ev_info,"1")){
			# okay x is holding event indicators
			}else{
			stop("event column not '1' or '0' ")
			}

			if(length(s)>0)  {
			warning("argument 's' ignored when time-event dataframe provided")
			}



			f<-x[which(x$event==1),1]
					failures <- data.frame(left = f, right = f, qty = rep(1, length(f)))
			if(identical(ev_info, c("0","1"))) {
			s<-x[which(x$event==0),1]
						suspensions <- data.frame(left = s, right = -1, qty = rep(1, length(s)))
			}
		}else {		
			if (length(x) > 0) {
				stop("error in x argument type")
			}
		}
	}			
	DF<-rbind(failures,suspensions,interval)			
## assure all integers in qty				
	DF$qty<-ceiling(DF$qty)
	
## futile attempt to consolidate duplicate data	
##	outDF<-DF[1,]			
##	outline<-2			
##	for(line in 2:nrow(DF))  {			
##		if(DF[line,1]-DF[line-1,1]+DF[line,2]-DF[line-1,2]==0)  {		
##			outDF[outline-1,3]<-DF[line,3]+DF[line-1,3]	
##		}else{		
##			outDF<-rbind(outDF,DF[line,])	
##			outline<-outline+1	
##		}		
##	}	

## plyr code to aggregate duplicate data
## http://stackoverflow.com/questions/10180132/consolidate-duplicate-rows
	outDF<-ddply(DF,c("left","right"),numcolwise(sum))

## don't know why but this bombed mlefit
##			attr(outDF,"fsiq")<-TRUE			
				
return(outDF)				
}				
