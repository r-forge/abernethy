prrVec<-function(x, model="w2", S=10^4, ProgRpt=FALSE)  {	
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
	
	## Test for valid S
	    S = as.integer(S/10)*10
	if(S<10^3)  {
	stop("Insufficient samples")
	}
	
	seed=1234
	Bval=.5   ## just to be some value, not used
	CI=0.0	  ## sets condition for no confidence interval evaluation or return
	Rsqr=1.0  ## sets condition for prr vector to be returned
	Eta=1.0
	Beta=1.0
	
	## Test for model
	if(model=="w2"|| model=="W2") {
	
	## rank<-seq(1:N)
	## pt_est<-qbeta(0.5, rank, N-rank+1)
	## mranks<-log(1/(1-pt_est))	

	outvec<-.Call("pivotalMCw2p", mranks, c(Rsqr,CI,Eta,Beta), S, seed, Bval, ProgRpt, PACKAGE= "pivotals")
	}else{
	stop("model not recognized")
	}
	
	outvec

}	
