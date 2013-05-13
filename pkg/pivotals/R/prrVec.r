prrVec<-function(x, S=10^4, model="w2", seed=1234, options=NULL, ProgRpt=FALSE)  {	
	## Test for valid x
      if(length(x)==1) {		
  	    N = as.integer(x)	
  	    if (N < 3) {	
  	        stop("Insufficient data points")             	
  	    }
  	    event<-rep(1,N)
  	    mranks<-mrank(event,options)
      }else{
      if(length(x)<3) {
            stop("Insufficient data points")        
        }else{
          for(i in 1:length(x)) {
            if(x[i]!=1&&x[i]!=0) {
              stop("Not an event vector")
              }
            }
          mranks<-mrank(x,options)
          }
      }
	
	## Test for valid S
	    S = as.integer(S/10)*10
	if(S<10^3)  {
	stop("Insufficient samples")
	}
	
	#seed=1234
	Bval=.5   ## just to be some value, not used
	CI=0.0	  ## sets condition for no confidence interval evaluation or return
	Rsqr=1.0  ## sets condition for prr vector to be returned
	P1=1.0
	P2=1.0
	
	## Test for model			
	if(model=="w2") {			
				
	outvec<-.Call("pivotalMCw2p", mranks, c(Rsqr,CI,P1,P2), S, seed, Bval, ProgRpt, PACKAGE= "pivotals")			
	}else{			
				
		if(model=="ln2"|| model=="n") {	
			outvec<-.Call("pivotalMCln2p", mranks, c(Rsqr,CI,P1,P2), S, seed, Bval, ProgRpt, PACKAGE= "pivotals")	
			}else{	
			stop("model not recognized")	
		}		
	}
	
	outvec

}	
