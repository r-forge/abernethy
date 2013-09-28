prrVal<-function(x, Rsqr, S=10^4, model="w2", seed=1234, options=NULL, ProgRpt=FALSE)  {				
				
	## Test for valid x			
      if(length(x)==1) {				
  	    N = as.integer(x)			
  	    if (N < 3) {			
  	        stop("Insufficient data points")             			
  	    }			
  	    event<-rep(1,N)			
  	    mranks<-mrank(event,options)			
      }else{				
      if(sum(x)<3) {				
            stop("Insufficient failure data points")        				
        }else{				
          for(i in 1:length(x)) {				
            if(x[i]!=1&&x[i]!=0) {				
              stop("Not an event vector")				
              }				
            }				
          mranks<-mrank(x,options)				
          }				
      }				
				
	## Test for valid Rsqr			
	if(Rsqr<=0 || Rsqr>=1.0) 	stop("Invalid Rsqr")		
				
				
	## Test for valid S			
	    S = as.integer(S/10)*10			
	if(S<10^3)  {			
	stop("Insufficient samples")			
	}
		if(S>4*10^9)   {
	stop("Samples beyond MAX_INT")
	}
				
	#seed=1234			
	Bval=.5   ## just to be some value, not used			
	CI=0.0  ## this disables Confidence band calculations			
	P1=1.0
	P2=1.0			
				
	modeldf<-data.frame(model=model)			
				
	## Test for model			
	if(model=="w2") {			
				
	outdf<-.Call("pivotalMCw2p", mranks, c(Rsqr,CI,P1,P2), S, seed, Bval, ProgRpt, PACKAGE= "pivotals")			
	}else{			
				
		if(model=="ln2"|| model=="n") {		
				
			outdf<-.Call("pivotalMCln2p", mranks, c(Rsqr,CI,P1,P2), S, seed, Bval, ProgRpt, PACKAGE= "pivotals")	
			}else{	
			stop("model not recognized")	
		}		
	}			
	outdf<-cbind(outdf,modeldf)			
	outdf			
				
}				

