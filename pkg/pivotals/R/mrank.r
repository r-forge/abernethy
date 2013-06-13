mrank<-function(x, options=NULL)  {		
## The only valid entry for x is an event vector of 1's and 0's			
	          for(i in 1:length(x)) {		
	            if(x[i]!=1&&x[i]!=0) {		
	              stop("Not an event vector")		
	              }		
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
			
	if(method==0)  {		
		outvec<-.Call("medianRank",x, PACKAGE= "pivotals")	
	}		
	if(method==1)  {		
		outvec<-.Call("medianRank1",x, PACKAGE= "pivotals")		
	}		
			
	outvec		
	}		
