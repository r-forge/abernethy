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
##  This is a fourth draft of the MLEcontour function in development.  Stability has been good so far.			
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
			
MLEw2pContour<-function(x, s=NULL, CL=0.9, DF=1,debias=FALSE,show=FALSE)  {			
## additional parameters taken as default for this example			
## degrees of freedom for Chi squared statistic, typical useage			
	##DF=1 for confidence bounds on single (constrained) model		
	##DF=2 for comparison of two models each with 2 parameters		
## limits for accuracy of parameter determination			
	BetaLimit=1.0e-4		
	EtaLimit=1.0e-4		
## Point density for contour development			
	PtDensity=10		
## axis adjustment factors on MLE point for countour development			
## ratio of Beta_hat to LowerContour for downward shift from Beta_hat			
	BetaShift=0.25		
## ratio of PtDensity to apply to right of Eta_hat			
##  RightExtend is now a calculated factor			
			
## Internal functions			
			
	LL<-function(x,s,Beta,Eta)  {		
		suscomp<-1	
		failcomp<-sum(dweibull(x,Beta,Eta,log=TRUE))	
		if(length(s)>0)  {	
			suscomp<-sum(pweibull(s,Beta,Eta,lower.tail=FALSE,log.p=TRUE))
		}	
		value<-failcomp+suscomp	
	}		
			
	getContourAtGivenBeta<-function(x,s,MLLx,FF,EtaEst,Beta,EtaLimit=1.0e-4)  {		
			
		DL<-EtaLimit	
		X0<-EtaEst	
		DX<-X0/10	
		istep<-0	
		X1<-X0+DX	
			
	## outDF<-data.frame(steps=istep,root=X0,error=DX,GB=(MLLx-LL(x,s,Beta,X0))^FF-qchisq(CL,DF)/2)		
			
		while(abs(DX)>DL)  {	
			GX0<-(MLLx-LL(x,s,Beta,X0))*FF-qchisq(CL,DF)/2
			GX1<-(MLLx-LL(x,s,Beta,X1))*FF-qchisq(CL,DF)/2
			D<- GX1-GX0
			X2<-X1-(X1-X0)*GX1/D
			X0<-X1
			X1<-X2
			DX<-X1-X0
			istep<-istep+1
			
	## DFline<-data.frame(steps=istep,root=X0,error=DX,GB=GX1)		
	## outDF<-rbind(outDF,DFline)		
		}	
			
	outvec<-c(X0,Beta)		
	names(outvec)<-c("Eta","Beta")		
			
	outvec		
	}		
			
			
			
	getContourAtGivenEta<-function(x,s,MLLx,FF,BetaEst,Eta,BetaLimit=1.0e-4)  {		
			
		DL<-BetaLimit	
		X0<-BetaEst	
		DX<-X0/10	
		istep<-0	
		X1<-X0+DX	
			
	## outDF<-data.frame(steps=istep,root=X0,error=DX,GB=(MLLx-LL(x,s,X0,Eta))^FF-qchisq(CL,DF)/2)		
			
		while(abs(DX)>DL)  {	
			GX0<-(MLLx-LL(x,s,X0,Eta))*FF-qchisq(CL,DF)/2
			GX1<-(MLLx-LL(x,s,X1,Eta))*FF-qchisq(CL,DF)/2
			D<- GX1-GX0
			X2<-X1-(X1-X0)*GX1/D
			X0<-X1
			X1<-X2
			DX<-X1-X0
			istep<-istep+1
			
	## DFline<-data.frame(steps=istep,root=X0,error=DX,GB=GX1)		
	## outDF<-rbind(outDF,DFline)		
		}	
			
	outvec<-c(Eta,Beta=X0)		
	names(outvec)<-c("Eta","Beta")		
			
	outvec		
	}		
			
			
			
	BetaRangeMidLowerContour<-function(x,s,MLLx,FF,Eta_hat,Beta_hat) {		
		b_inc<-Beta_hat/4	
		step=1	
		LastBeta=Beta_hat	
		Beta<-Beta_hat-b_inc	
		Eta<-Eta_hat	
		resid<-(MLLx-LL(x,s,Beta,Eta))*FF-qchisq(CL,DF)/2	
			
		while(resid<0&&step<3)  {	
			step=step+1
			LastBeta=Beta
			Beta=Beta-b_inc
			resid<-(MLLx-LL(x,s,Beta,Eta))*FF-qchisq(CL,DF)/2
		}	
			
	## if (step=3 &&resid<0) then there is more to do		
		if(step==3 &&resid<0) {	
			b_inc<-b_inc/4
			step=4
			LastBeta=Beta
			Beta=Beta-b_inc
			resid<-(MLLx-LL(x,s,Beta,Eta))*FF-qchisq(CL,DF)/2
			
		while(resid<0&&step<7)  {	
			step=step+1
			LastBeta=Beta
			Beta=Beta-b_inc
			resid<-(MLLx-LL(x,s,Beta,Eta))*FF-qchisq(CL,DF)/2
		}	
		}	
			
	## if (step=7 &&resid<0) At this point I think we need to stop		
		if (step==7 &&resid<0)  {	
		stop("lower Beta contour not found")	
		}	
			
	return(c(Beta,LastBeta))		
			
	}		
			
			
			
	EtaRangeLeftUpperContour<-function(x,s, MLLx,FF,Eta_hat,Beta_mod) {		
		e_inc<-Eta_hat/4	
		step=1	
		LastEta=Eta_hat	
		Eta<-Eta_hat-e_inc	
		Beta<-Beta_mod	
		resid<-(MLLx-LL(x,s,Beta,Eta))*FF-qchisq(CL,DF)/2	
			
		while(resid<0&&step<3)  {	
			step=step+1
			LastEta=Eta
			Eta<-Eta-e_inc
			resid<-(MLLx-LL(x,s,Beta,Eta))*FF-qchisq(CL,DF)/2
		}	
	## if (step=3 &&resid<0) then there is more to do		
		if(step==3 &&resid<0) {	
		e_inc<-Eta_hat/4	
		step=4	
		LastEta=Eta	
		Eta<-Eta_hat-e_inc	
		resid<-(MLLx-LL(x,s,Beta,Eta))*FF-qchisq(CL,DF)/2	
		while(resid<0&&step<7)  {	
			step=step+1
			LastEta=Eta
			Eta<-Eta-e_inc
			resid<-(MLLx-LL(x,s,Beta,Eta))*FF-qchisq(CL,DF)/2
		}	
		}	
			
	## if (step=7 &&resid<0) At this point I think we need to stop		
		if (step==7 &&resid<0)  {	
		stop("lower Eta contour not found")	
		}	
			
	return(c(Eta,LastEta))		
			
	}		
			
			
	EtaRangeRightUpperContour<-function(x,s, MLLx,FF,Eta_hat,Beta_mod) {		
## find lower Eta_c at Beta_mod		
		e_inc<-Eta_hat/4	
		step=1	
		LastEta=Eta_hat	
		Eta<-Eta_hat+e_inc	
		Beta<-Beta_mod	
		resid<-(MLLx-LL(x,s,Beta,Eta))*FF-qchisq(CL,DF)/2	
			
		while(resid<0)  {	
			step=step+1
			LastEta=Eta
			Eta<-Eta+e_inc
			resid<-(MLLx-LL(x,s,Beta,Eta))*FF-qchisq(CL,DF)/2
		}	
			
	return(c(Eta,LastEta))		
	}		
			
			
## start of main procedure		
				
	MLEfit<-MLEw2p_cpp(x,s)
	Beta_hat<-MLEfit[2]	
	Eta_hat<-MLEfit[1]	
	MLLx<-LL(x,s,Beta_hat,Eta_hat)		
	FF<-1		
	if(debias==TRUE)  {		
	Nf<-length(x)		
	FF<-(Nf-1)/(Nf+0.618)		
	}		
	BetaEst<-mean(BetaRangeMidLowerContour(x,s,MLLx,FF,Eta_hat,Beta_hat) )		
	MidLowerPt<-getContourAtGivenEta(x,s,MLLx,FF,BetaEst,Eta_hat)		
			
	Beta_mod<-Beta_hat-(Beta_hat-MidLowerPt[2])*BetaShift		
			
	EtaEst<-mean(EtaRangeLeftUpperContour(x,s,MLLx,FF,Eta_hat,Beta_mod))		
	StartPtLeftUpperContour<-getContourAtGivenBeta(x,s,MLLx,FF,EtaEst,Beta_mod)		
			
			
	EtaEst<-mean(EtaRangeRightUpperContour(x,s, MLLx,FF,Eta_hat,Beta_mod))		
	EndPtRightUpperContour<-getContourAtGivenBeta(x,s,MLLx,FF,EtaEst,Beta_mod)		
			
	Eta_inc<-(Eta_hat-StartPtLeftUpperContour[1])/PtDensity		
	RightEtaSpan<-EndPtRightUpperContour[1]-Eta_hat		
	RightExtend<-RightEtaSpan/(Eta_inc*PtDensity)		
			
	Eta<-StartPtLeftUpperContour[1]		
	BetaEst<-StartPtLeftUpperContour[2]		
	UpperContour<-data.frame(t(StartPtLeftUpperContour))		
## number of UpperContour point intervals			
	Nupts<-floor(PtDensity*(1+RightExtend))		
	for(k in 1:Nupts)  {		
		Eta<-Eta+Eta_inc	
		thisPt<-getContourAtGivenEta(x,s,MLLx,FF,BetaEst,Eta)	
		UpperContour<-rbind(UpperContour,data.frame(t(thisPt)))	
		BetaEst<-thisPt[2]	
	}		
			
	Eta<-Eta_hat		
	BetaEst<-MidLowerPt[2]		
	LowerContour<-data.frame(t(MidLowerPt))		
	for(k in 1:PtDensity)  {		
		Eta<-Eta-Eta_inc	
		thisPt<-getContourAtGivenEta(x,s,MLLx,FF,BetaEst,Eta)	
		LowerContour<-rbind(LowerContour,data.frame(t(thisPt)))	
		BetaEst<-thisPt[2]	
	}		
			
	Eta<-Eta_hat		
	BetaEst<-MidLowerPt[2]		
	for(k in 1:(Nupts-PtDensity))  {		
		Eta<-Eta+Eta_inc	
		thisPt<-getContourAtGivenEta(x,s,MLLx,FF,BetaEst,Eta)	
		LowerContour<-rbind(LowerContour,data.frame(t(thisPt)))	
		BetaEst<-thisPt[2]	
	}		
## sort LowerContour points for clean line plot			
	NDX<-order(LowerContour[1])		
	LowerContour<-LowerContour[NDX,]		
			
	LBeta_inc<-(StartPtLeftUpperContour[2]-LowerContour[1,2])/PtDensity		
			
	Beta<-LowerContour[1,2]		
	EtaEst<-LowerContour[1,1]		
	LeftContour<-LowerContour[1,]		
			
	for(k in 1:PtDensity)  {		
		Beta<-Beta+LBeta_inc	
		thisPt<-getContourAtGivenBeta(x,s,MLLx,FF,EtaEst,Beta)	
		LeftContour<-rbind(LeftContour,data.frame(t(thisPt)))	
		EtaEst<-thisPt[1]	
	}		
## Yes, I COULD get this information from PtDensity and Nupts			
	Nu<-length(UpperContour[,1])		
	Nl<-length(LowerContour[,1])		
	RBeta_inc<-(UpperContour[Nu,2]-LowerContour[Nl,2])/PtDensity		
			
	Beta<-LowerContour[Nl,2]		
	EtaEst<-LowerContour[Nl,1]		
	RightContour<-LowerContour[Nl,]		
			
	for(k in 1:PtDensity)  {		
		Beta<-Beta+RBeta_inc	
		thisPt<-getContourAtGivenBeta(x,s,MLLx,FF,EtaEst,Beta)	
		RightContour<-rbind(RightContour,data.frame(t(thisPt)))	
		EtaEst<-thisPt[1]	
	}		
			
			
	rba<-1		
	if(debias==TRUE)  {		
	rba<-RBAbeta(length(x))		
	UpperContour[2]<-UpperContour[2]*rba		
	LowerContour[2]<-LowerContour[2]*rba		
	LeftContour[2]<-LeftContour[2]*rba		
	RightContour[2]<-RightContour[2]*rba		
	}		
			
			
			
	outlist<-list(Upper=UpperContour,Lower=LowerContour,		
	Left=LeftContour,Right=RightContour)		
			
	if(show==TRUE)  {		
		maxBeta<-max(UpperContour[,2])	
		minBeta<-min(LowerContour[,2])	
		minEta<-min(LeftContour[,1])	
		maxEta<-max(RightContour[,1])	
			
		ylo<-floor(minBeta)	
		yhi<-floor(maxBeta)+1	
			
		EtaDec<-10^(floor(log(minEta)/log(10))-1)	
		xlo<-EtaDec*(floor(minEta/EtaDec)-1)	
		xhi<-EtaDec*(  floor(maxEta/EtaDec)+1   )	
			
		plot(Eta_hat,Beta_hat,xlim=c(xlo,xhi),ylim=c(ylo,yhi))	
		for(i in 1:4)  {	
		lines(outlist[[i]])	
		}	
		}	
			
outlist			
}			
