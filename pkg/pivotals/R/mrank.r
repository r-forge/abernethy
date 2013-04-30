mrank<-function(x, options=NULL)  {		
## The only valid entry for x is an event vecor of 1's and 0's			
	          for(i in 1:length(x)) {		
	            if(x[i]!=1&&x[i]!=0) {		
	              stop("Not an event vector")		
	              }		
	            }		
			
## Default point estimation method is Benard's approximation for median ranks			
	method=0		
	if(!missing(options))  {		
## An example means for testing wb.options used with Weibull toolkit			
	     if(length(options$method.rank)>0) {		
		if(options$method.rank=="qbeta"){	
			method=1
		}	
	     }		
	}		
			
	if(method==0)  {		
		outvec<-.Call("medianRank",x, PACKAGE= "pivotals")	
	}		
	if(method==1)  {		
		outvec<-.Call("medianRank1",x, PACKAGE= "pivotals")		
	}		
			
	outvec		
	}		
