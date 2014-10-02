mlefit<-function(x, dist="weibull", debias=NULL, con=NULL)  {			
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
	data_est<-NULL
	if((Nf+Ns)>0)  {
		fsd<-fsiq$left[1:(Nf + Ns)]
## assure that data_est is a clone		
		data_est<-fsiq$left[1:(Nf + Ns)]
	}
	if(Nd>0) {		
		fsd<-c(fsd,fsiq$right[(Nf + Ns + 1):(Nf +  Ns + Nd)])
		data_est <- c(data_est, 0.5*(fsiq$right[(Nf + Ns + 1):(Nf + Ns + Nd)]))
	}		
	if(Ni>0)  {		
		fsdi<-c(fsd, fsiq$left[(Nf + Ns + Nd + 1):nrow(fsiq)], 	
		fsiq$right[(Nf + Ns + Nd + 1):nrow(fsiq)])
		data_est<-c(data_est, (fsiq$left[(Nf + Ns + Nd + 1):nrow(fsiq)] + 
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
	
## Optional optimization con list to be handled here				
		## vstart will be as estimated	
		limit<-1e-5	
		maxit<-100	
		listout<-FALSE	
			
	if(length(con)>0)  {		
		if(length(con$vstart>0))  {	
			vstart<-con$vstart
		}	
		if(length(con$limit)>0)  {	
			limit<-con$limit
		}	
		if(length(con$maxit)>0)  {	
			maxit<-con$maxit
		}	
		if(length(con$listout)>0)  {	
			listout<-con$listout
		}	
	}		

	MLEclassList<-list(fsdi=fsdi,q=q,N=N)
	SimplexList<-list(dist_num=dist_num, vstart=vstart,limit=limit,maxit=maxit)

	retlist<-.Call("MLEsimplex",MLEclassList, SimplexList, package="abremDebias")
	
	outvec<-retlist[[1]][1:3]
	
	if(dist_num == 1)  {
		names(outvec)<-c("Eta","Beta","LL")
	}
		if(dist_num == 2)  {
		names(outvec)<-c("Mulog","Sigmalog","LL")
	}
	if(retlist[[1]][4]>0)  {
		warn<-"likelihood optimization did not converge"
		attr(outvec,"warning")<-warn
	}
	
	if(listout==FALSE) {
		return(outvec)
	}else{
		optDF<-as.data.frame(retlist[[2]])
		names(optDF)<-c("beta_est", "eta_est", "negLL", "error")
		return(list(fit=outvec, opt=optDF))
	}
	
}				
