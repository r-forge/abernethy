mlefit<-function(x, dist="weibull")  {				
## check basic parameters of x				
	if(class(x)!="data.frame") {stop("mlefit takes a structured dataframe input, use mleframe")}			
	if(ncol(x)!=3)  {stop("mlefit takes a structured dataframe input, use mleframe")}			
	xnames<-names(x)			
	if(xnames[1]!="left" || xnames[2]!="right"||xnames[3]!="qty")  {			
		 stop("mlefit takes a structured dataframe input, use mleframe")  }		
## test for any na's and stop, else testint below will be wrong				
				
				
## need this length information regardless of input object formation				
	testint<-x$right-x$left			
	failNDX<-which(testint==0)			
	suspNDX<-which(testint<0)			
	Nf<-length(failNDX)			
	Ns<-length(suspNDX)			
	discoveryNDX<-which(x$left==0)			
	Nd<-length(discoveryNDX)			
	intervalNDX<-which(testint>0)			
	interval<-x[intervalNDX,]			
	intervalsNDX<-which(interval$left>0)			
	Ni<-length(intervalsNDX)					
				
## further validate the input arguments for non-fsiq object				
	if(length(attributes(x)$fsiq)!=1)  {							
## stop if Nf+Ns+Ndi != nrow(x)				
	if( (Nf+Ns+Nd+Ni) != nrow(x))  {			
		stop("invalid input dataframe")		
	}				
## rebuild input vector from components, just to be sure				
	fsiq<-rbind(x[failNDX,], x[suspNDX,], x[discoveryNDX,], interval[intervalsNDX,])			
## end input validation code				
	}else{			
		fsiq<-x		
	}	

## Not sure what to place as restriction for C++ call	
##	if((Nf+Ni)<3)  {stop("insufficient failure data")}	

			
## now form the arguments for C++ call				
## fsdi is the time vector to pass into C++
## data_est is used to estimate the magnitude of data	
	fsd<-NULL
	if((Nf+Ns)>0)  {
		fsd<-fsiq$left[1:(Nf + Ns)]
	}
	if(Nd>0) {		
		fsd<-c(fsd,fsiq$right[(Nf + Ns + 1):(Nf +  Ns + Nd)])	
	}		
	if(Ni>0)  {		
		fsdi<-c(fsd, fsiq$left[(Nf + Ns + Nd + 1):nrow(fsiq)], 	
		fsiq$right[(Nf + Ns + Nd + 1):nrow(fsiq)])
		data_est<-c(fsd, (fsiq$left[(Nf + Ns + Nd + 1):nrow(fsiq)] + 
				 fsiq$right[(Nf + Ns + Nd + 1):nrow(fsiq)])/2)		  
	}else{
		fsdi<-fsd
		data_est<-fsd
	}
	
	q<-fsiq$qty			
## third argument will be c(Nf,Ns,Nd,Ni)				
	N<-c(Nf,Ns,Nd,Ni)
			
## establish distribution number
	if(tolower(dist)=="weibull"	)  {
		dist_num=1
		m <- mean(log(data_est))			
		v <- var(log(data_est))			
		shape <- 1.2/sqrt(v)			
		scale <- exp(m + 0.572/shape)			
		vstart <- c(shape, scale)

	}else{
		if(tolower(dist)=="lognormal")  {
			dist_num=2
			ml <- mean(log(data_est))
			sdl<- sd(log(data_est))
			vstart<-c(ml,sdl)
			
			
			
		}else{
			stop("distribution not resolved")
		}
	}
							
	outvec<-.Call("MLEsimplex",fsdi,q,N,vstart,dist_num, package="abremDebias")
	if(dist_num == 1)  {
		names(outvec)<-c("Eta","Beta","LL")
	}
		if(dist_num == 2)  {
		names(outvec)<-c("Mulog","Sigmalog","LL")
	}
		
				
outvec				
}				
