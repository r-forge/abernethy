## file RRMw.r
##
## This is an implementation of the ReliaSoft Ranking Method presented in the ReliaWiki:
##  http://reliawiki.org/index.php/Appendix:_Special_Analysis_Methods#ReliaSoft_Ranking_Method
##  as applied only to the Weibull distribution.
## 




RRMw<-function(x=NULL, s=NULL, interval=NULL)  {				
## The Weibull PDF to be integrated over intervals				
	PDFw<-function(t,eta,beta)  {			
		beta/eta*(t/eta)^(beta-1)*exp(-1*(t/eta)^beta)		
	}			
## t x PDF(t) is integrated over intervals	in step 1 to obtain a weighted mid-point			
	tPDFw<-function(t,eta,beta) {			
		t*beta/eta*(t/eta)^(beta-1)*exp(-1*(t/eta)^beta)		
	}			
				
## interpret the interval argument with validation tests						
	Ni<-1			
	Nd<-1			
	if(class(interval)=="data.frame")  {			
		interval<-interval[with(interval,order(left,right)),]		
		testinterval<-which(interval[,1]>0)		
		if(length(testinterval)>0)  {		
		icrow<-min(testinterval)		
		}else{  		
		icrow<-0		
	## there are only discoveries, no intervals			
		d<-interval[,2]		
		iDF<-NULL		
		Ni<-0		
		warning("no true_intervals to evaluate")		
		}		
		if(icrow>1)  {		
	## there are both discoveries and intervals			
		d<-interval[1:(icrow-1),2]		
		true_interval<-interval[icrow:dim(interval)[1],]		
		}		
		if(icrow==1)  {		
	## there are no discoveries, only intervals			
		d<-NULL		
		Nd<-0		
		}		
	}else{			
		Nd<-0		
		Ni<-0		
		warning("improper interval argument")		
	}			
				
## get length information for simple reference later				
	Nf<-length(x)			
	Ns<-length(s)			
	if(Nd>0) Nd<-length(d)			
	if(Ni>0)  Ni<-dim(true_interval)[1]			
	Nd			
	Ni			
				
	if((Nf+Ni)<3)  {			
		stop("insufficient fail data")		
	}			
								
##Generate dataframes for each of these classes of input storing fields of time and n (quantity of ties)				
	## fDF, sDF, dDf, iDF			
				
	fDF<-NULL			
	if(Nf>0) {			
	x<-sort(x)			
	for(entry in 1:Nf)  {			
		if(entry==1)  {		
		fDF<-data.frame(time=x[entry], n=1)		
		}else{		
			if(x[entry]!=x[entry-1])  {	
				DFline<-data.frame(time=x[entry], n=1)
				fDF<-rbind(fDF,DFline)
			}else{	
				fDF[length(fDF$n),2]<-fDF[length(fDF$n),2]+1
			}	
		}		
	}			
	}			
				
	sDF<-NULL			
	if(Ns>0) {			
	s<-sort(s)			
	for(entry in 1:Ns)  {			
		if(entry==1)  {		
		sDF<-data.frame(left=s[entry], n=1)		
		}else{		
			if(s[entry]!=s[entry-1])  {	
				DFline<-data.frame(left=s[entry], n=1)
				sDF<-rbind(sDF,DFline)
			}else{	
				sDF[length(sDF$n),2]<-sDF[length(sDF$n),2]+1
			}	
		}		
	}			
	}			
								
	dDF<-NULL			
	if(Nd>0) {			
	for(entry in 1:Nd)  {			
		if(entry==1)  {		
		dDF<-data.frame(right=d[entry], n=1)		
		}else{		
			if(d[entry]!=d[entry-1])  {	
				DFline<-data.frame(right=d[entry], n=1)
				dDF<-rbind(dDF,DFline)
			}else{	
				dDF[length(dDF$n),2]<-dDF[length(dDF$n),2]+1
			}	
		}		
	}			
	}			
								
	iDF<-NULL			
	if(Ni>0) {			
	for(entry in 1:Ni)  {			
		left_entry<-true_interval$left[entry]		
		right_entry<-true_interval$right[entry]		
		if(entry==1)  {		
		iDF<-data.frame(left=left_entry,right=right_entry, time=(left_entry+right_entry)/2 , n=1)		
		}else{		
			if(sum(true_interval[entry,]-true_interval[entry-1,])!=0)  {	
				DFline<-data.frame(left=left_entry,right=right_entry, time=(left_entry+right_entry)/2 , n=1)
				iDF<-rbind(iDF,DFline)
			}else{	
				iDF[length(iDF$n),4]<-iDF[length(iDF$n),4]+1
			}	
		}		
	}			
	}			
								
## get length information for simple reference later				
	if(Nf>0) NfDF<-dim(fDF)[1]			
	if(Ns>0) NsDF<-dim(sDF)[1]			
	if(Nd>0)  NdDF<-dim(dDF)[1]			
	if(Ni>0)  NiDF<-dim(iDF)[1]			
				
## Step 0				
## Initial parameter estimation				
## combine failures and intervals in one dataframe using mid-points of intervals for time field				
	interval_hat<-NULL			
	for(entry in 1:NiDF)  {			
		interval_hat<-c(interval_hat,rep(iDF$time[entry],iDF$n[entry]))		
	}			
	f_hat<-c(x,interval_hat)			
## get an lslr fit based on ppos="beta",ties="highest"				
	fit1<-lslr(getPPP(f_hat,ppos="beta",ties="highest"))			
				
## initialize a new data frame to hold the parameter values from such a fit				
	fitDF<-data.frame(eta=fit1[1], beta=fit1[2], delta=1)			
## set a line counter to use for reference in loop				
	parLine<-1			
	delta<-1			
	Tol=1e-4			
				
## The main loop starts here				
while(delta>Tol)  {				
## set eta and beta here for label simplicity (in C++ this will be a fill of Param struct				
	eta<-fitDF[parLine,1]			
	beta<-fitDF[parLine,2]	
	
## Step 1				
## Get weighted midpoint for all intervals				
				
	for( k in 1:NiDF)  {			
		iDF$time[k]<-integrate(tPDFw,eta, beta,lower=iDF[k,1],upper=iDF[k,2])[[1]]  / 		
		integrate(PDFw,eta, beta,lower=iDF[k,1],upper=iDF[k,2])[[1]] 		
	}			
				
## Step 2				
## combine the failures and weighted midpoints, preserving quantity information as f_hatDF				
	f_hatDF<-rbind(fDF,iDF[,3:4])			
## sort on time				
	f_hatDF<-f_hatDF[with(f_hatDF,order(time)),]			
								
## Step 3				
## fill an order increment matrix in a double nested loop accounting for left-censored and right-censored data				
## define the matrix fails + increments in rows x discoveries + suspensions in columns				
	incMat<-matrix(nrow=(NfDF+NiDF),ncol=(NdDF+NsDF))							
## Compute the rank  increments in a double nested loop				
	for( i in 1:(NfDF+NiDF))  {			
## calculate increments into a  matrix for expository view				
## set ti and tim1				
			ti<-f_hatDF$time[i]	
			if(i==1)  {	
				tim1<-0
			}else{	
				tim1<-f_hatDF$time[i-1]
			}	
	if(NdDF>0)  {			
		for (j in 1:NdDF)  {		
			t0<-dDF$right[j]	
			if(t0>tim1)  {	
				numerator<-integrate(PDFw,eta,beta,lower=tim1, upper=min(ti,t0))[[1]]
				denominator<-pweibull(t0,beta,eta)
				incMat[i,j]<-dDF$n[j]*(numerator/denominator)
			}else{	
				incMat[i,j]<-0
			}	
		}		
	}			
	## calculate increments into a  matrix for expository view			
	if(NsDF>0)  {			
		for(j in (NdDF+1):(NdDF+NsDF))  {		
			t0<-sDF$left[j-NdDF]	
			if(t0<ti)  {	
				numerator<-integrate(PDFw,eta,beta,lower=max(t0,tim1), upper=ti)[[1]]
				denominator<- 1-pweibull(t0,beta,eta)
				incMat[i,j]<-sDF$n[j-NdDF]*(numerator/denominator)
			}else{	
				incMat[i,j]<-0
			}	
		}		
	}			
	}			
								
## Step 4				
## Get row sums from the increment matrix				
	increments<-rowSums(incMat)			
								
## Step 5				
## calc MON's by adding the number of items plus the previous MON plus the current increment				
	MON<-NULL			
	for(k in 1:(NfDF+NiDF))  {			
		if(k==1)  {		
			MON<-f_hatDF$n[k]+increments[k]	
		}else{		
			MON<-c(MON,f_hatDF$n[k]+increments[k]+MON[k-1])	
		}		
	}			
								
## Step 6				
## use beta to get PPP from the MON				
	ppp<-qbeta(0.5,MON,(Nf+Ns+Nd+Ni)-MON+1)			
								
## Step 7				
## fit the failures and computed midpoints ppos="beta",ties="highest"
## the ReliaWiki indicated using "on X"; however, it has been demonstrated that results match with method="XonY", which is default here.				
	PPP_DF<-data.frame(time=f_hatDF$time,ppp=ppp,MON=MON)			
	fit1<-lslr(PPP_DF)							
	delta<-abs(fitDF$eta[parLine]+fitDF$beta[parLine]-fit1[1]-fit1[2])							
	fitDF<-rbind(fitDF,data.frame(eta=fit1[1], beta=fit1[2], delta=delta))			
	parLine<-parLine+1			
}				
## return to main loop				
				
outlist<-list(ppp=PPP_DF, trials=fitDF, increments=incMat)				
				
return(outlist)				
}				
