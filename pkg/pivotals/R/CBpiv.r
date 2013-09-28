##
##
##




CBpiv<-function	(x, CI, S=10^4, Bval=mrank(rep(1,15)),Eta=1.0,Beta=1.0, model="w2", seed=1234, ProgRpt=FALSE) 
{
      if(length(x)==1) {		
  	    N = as.integer(x)	
  	    if (N < 2) stop("Insufficient data points")
  	    event<-rep(1,N)
  	    mranks<-mrank(event)
      }else{
          if(length(x)<2) {
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
      
	    if (CI <= 0|| CI>=1) stop("Invalid Confidence Interval")
	        	
	    
	S = as.integer(S/10)*10	
	if (S < 10^3) {	
	    stop("Insufficient samples")	
	   }	
	if(S>4*10^9)   {
		stop("Samples beyond MAX_INT")
	}		
		R2=0.0  ## this diables p-value calculations for this specific CB function run
		#seed=1234
		
	    if (model == "w2" || model == "W2") {	

		
	outdf<-.Call("pivotalMCw2p",mranks, c(R2,CI,Eta,Beta), S, seed, Bval, ProgRpt,	
	            PACKAGE = "pivotals")	
	    }	
	outdf	
}		
