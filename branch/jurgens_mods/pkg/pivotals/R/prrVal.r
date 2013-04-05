prrVal<-function(x, Rsqr, S=10^4, Eta=1.0, Beta=1.0, model="w2", ProgRpt=FALSE,seed=1234,method="exact",CI=0.0)  {	
	## Test for valid x
      if(length(x)==1) {		
  	    N = as.integer(x)	
  	    if (N < 3) {	
  	        stop("Insufficient data points")             	
  	    }
  	    event<-rep(1,N)
  	    mranks<-mrank(event)
      }else{
      if(length(x)<3) {
            stop("Insufficient data points")        
        }else{
          for(i in 1:length(x)) {
            if(x[i]!=1&&x[i]!=0) {
              stop("Not an event vector")
              }
            }
          mranks<-mrank(x)
          }
      }
	
	## Test for valid Rsqr
	if(Rsqr<=0 || Rsqr>=1.0) 	stop("Invalid Rsqr")

	
	## Test for valid S
	    S = as.integer(S/10)*10
	if(S<10^3)  {
	stop("Insufficient samples")
	}
	
	#seed=1234
	Bval=.5   ## just to be some value, not used
	#CI=0.0
	
	## Test for model
	if(model=="w2"|| model=="W2") {
	

	outdf<-.Call("pivotalMCw2p", mranks, c(Rsqr,CI,Eta,Beta), S, seed, Bval, ProgRpt, PACKAGE= "pivotals")
	}else{
	stop("model not recognized")
	}
	
	outdf

}	
