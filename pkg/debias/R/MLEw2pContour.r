##  MLEw2pContour.r			
##			
##  This function generates points for a display of the likelihood contour at given			
##  confidence limit for the 2-parameter Weibull distribution.  The contour is useful			
##  in a graphical presentation of the likelihood ratio test of two samples, and the points 			
##  are used for the generation of likelihood confidence intervals on the Weibull plot.			
##  Optionally this function implements the "JLF bias adjustment" to the contour 			
##  as postulated in separate papers by Dr. Abernethy and Wes Fulton and discussed in 			
##  Section 7.5.3 of The New Weibull Handbook,Fifth Edition.  The actual implementation			
##  of this "de-bias" feature is somewhat different than the specific JLF equation shown in the text.			
##  The function used here is:			
##  (log(ML(p1_hat,p2_hat))-log(RL(p1,p2))*FF - chisquare(CL,DF)/2=0, where ML is Maximum Likelihood for the data, 			
##  and RL is Ratioed Likelihood for the data at selected points for the contour.  			
##  Degrees of freedom can be set by arguement DF, which defaults to DF=1 for confidence bound use.			
##  The contour points (p1,p2) identified as satisfying the root of this equation are then modified by the 			
##  RBA (median basis) on the Beta. It is believed that this is the intent of the text as it appears to correlate with 			
##  the "modified LR Test".  The text provides no guidance on the "vertical adjustment" V|JLLF| other than to suggest 			
##  that it is insignificant (at least with respect to modification of the chi-square statistic).  Indeed, 			
##  the Fulton Factor, FF, is in reality an adjustment on the chi-square statistic, not the likelihood function itself.			
##  			
##  This is a fifth draft of the MLEcontour function in development.  Instability is known to exist at high values
##  of CL (such as 0.995) and with very large relative quantity of suspensions (like 30 times complete failures).			
##			
## (C) Jacob T. Ormerod 2013			
##			
## This program is free software; you can redistribute it and/or modify it			
## under the terms of the GNU General Public License as published by the			
## Free Software Foundation; either version 2, or (at your option) any			
## later version.			
##			
## These functions are distributed in the hope that they will be useful,			
## but WITHOUT ANY WARRANTY; without even the implied warranty of			
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the			
## GNU General Public License for more details.			
##			
##  You should have received a copy of the GNU General Public License			
##  along with this program; if not, a copy is available at			
##  http://www.r-project.org/Licenses/			
##			
			
MLEw2pContour<-function(x,s=NULL,CL=0.9,DF=1,MLEfit=NULL,ptDensity=100,RadLimit=1e-5,debias=FALSE,show=FALSE)  {		
	## limits for accuracy of parameter determination		
	##  RadLimit=1.0e-5		
			
			
	## Internal functions		
			
	LL<-function(x,s,Beta,Eta)  {		
		suscomp<-0	
		failcomp<-sum(dweibull(x,Beta,Eta,log=TRUE))	
		if(length(s)>0)  {	
			suscomp<-sum(pweibull(s,Beta,Eta,lower.tail=FALSE,log.p=TRUE))
		}	
		value<-failcomp+suscomp	
	}		
			
	RadialSecant<-function(RadEst,theta, Beta_hat, Eta_hat, RadLimit  )   {		
			
		DL<-RadLimit	
		X0<-RadEst*.7	
		if(CL>.9) {X0<-1-(1-CL)*5}	
		DX<-X0/100	
		istep<-0	
		X1<-X0+DX	
			
		Beta0<-(1+X0*cos(theta))*Beta_hat	
		Eta0<-(1+X0*sin(theta))*Eta_hat	
		GX0<-(MLLx-LL(x,s,Beta0,Eta0))*FF-qchisq(CL,DF)/2	
			
	while(abs(DX)>DL)  {		
		Beta1<-(1+X1*cos(theta))*Beta_hat	
	## assure that only positive Beta estimate is considered		
		if(Beta1<0)  {	
		X1<-X0+0.5*(Beta_hat/cos(theta)-X0)	
		Beta1<-(1+X1*cos(theta))*Beta_hat	
		}	
			
		Eta1<-(1+X1*sin(theta))*Eta_hat	
		if(is.nan(Eta1))  {	
		Eta1<-Eta0	
		Beta1<-Beta0	
		X1<-X0	
			
		warning=TRUE	
		break	
		}	
			
			
			
	## assure that only positive Eta estimate is considered		
		if(Eta1<0)  {	
		X1<-X0+0.2*(Eta_hat/sin(theta)-X0)	
		Beta1<-(1+X1*cos(theta))*Beta_hat	
		Eta1<-(1+X1*sin(theta))*Eta_hat	
		}	
			
		GX1<-(MLLx-LL(x,s,Beta1,Eta1))*FF-qchisq(CL,DF)/2	
		if(is.nan(GX1))  {	
		Eta1<-Eta0	
		Beta1<-Beta0	
		X1<-X0	
			
		break	
		}	
			
			
		D<- GX1-GX0	
		X2<-X1-(X1-X0)*GX1/D	
	## assure that only positive radius is considered		
		if(X2<0) {X2<-X1/10}	
		X0<-X1	
		GX0<-GX1	
		X1<-X2	
		DX<-X1-X0	
		istep<-istep+1	
	}		
	params<-c(Eta1, Beta1)		
	names(params)<-c("Eta","Beta")		
	radius<-X0		
	names(radius)<-NULL		
	outlist<-list(params,radius)		
	}		
			
## start of main procedure			
	if(missing(MLEfit))  {		
		MLEfit<-MLEw2p_cpp(x,s)	
	}		
		Beta_hat<-MLEfit[2]	
		Eta_hat<-MLEfit[1]	
			
	MLLx<-LL(x,s,Beta_hat,Eta_hat)		
	FF<-1		
	if(debias==TRUE)  {		
	Nf<-length(x)		
	FF<-(Nf-1)/(Nf+0.618)		
	}		
			
	theta<-pi		
	thisPt<-RadialSecant(0.5,theta, Beta_hat, Eta_hat, RadLimit)		
	RadEst<-thisPt[[2]]		
	Contour<-data.frame(t(thisPt[[1]]))		
			
	for( pt in 1:ptDensity)  {		
		theta<-theta+ 2*pi/ptDensity	
		thisPt<-RadialSecant(RadEst,theta, Beta_hat, Eta_hat, RadLimit)	
		RadEst<-thisPt[[2]]	
		Contour<-rbind(Contour,data.frame(t(thisPt[[1]])))	
	}		
			
	 		
	rba<-1		
	if(debias==TRUE)  {		
	rba<-RBAbeta(length(x))		
	Contour[2]<-Contour[2]*rba		
	}		
			
	if(show==TRUE)  {		
		maxBeta<-max(Contour[,2])	
		minBeta<-min(Contour[,2])	
		minEta<-min(Contour[,1])	
		maxEta<-max(Contour[,1])	
			
		ylo<-floor(minBeta)	
		yhi<-floor(maxBeta)+1	
			
		EtaDec<-10^(floor(log(minEta)/log(10))-1)	
		xlo<-EtaDec*(floor(minEta/EtaDec)-1)	
		xhi<-EtaDec*(  floor(maxEta/EtaDec)+1   )	
			
		plot(Eta_hat,Beta_hat*rba,xlim=c(xlo,xhi),ylim=c(ylo,yhi))	
		lines(Contour)	
	}		
Contour			
}
			
