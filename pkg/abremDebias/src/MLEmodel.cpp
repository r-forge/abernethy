// traditional header file content - class declaration
#include "abremDebias.h"	
#include <math.h>	

    using namespace Rcpp ;	

class MLEmodel {

arma::colvec time;
arma::colvec qty;
Rcpp::NumericVector N;

double failcomp;
double suscomp;
double discomp;
double intcomp;
int endf;
int ends;
int endd;
int endil;
int endir;
arma::colvec fail;
arma::colvec nf;
arma::colvec susp;
arma::colvec ns;
arma::colvec disc;
arma::colvec nd;
arma::colvec left;
arma::colvec right;
arma::colvec ni;
  
public:
MLEmodel(SEXP);
double LogLike(arma::colvec, int, int);
SEXP MLEsimplex(SEXP);
};
// end of class declaration


// class implementation

MLEmodel::MLEmodel( SEXP arg1) {
	Rcpp::List L(arg1);
	time=Rcpp::as<arma::colvec>(L["fsdi"]);
	qty=Rcpp::as<arma::colvec>(L["q"]);
	N=L["N"];
// Provide a first non-sense element to front of time vector
// so that position math works when Nf is zero.
	time.insert_rows(0,1);
	qty.insert_rows(0,1);

	endf=N[0];
	ends=endf+N[1];
	endd=ends+N[2];
	endil=endd+N[3];
	endir=endil+N[3];

	if(N[0]>0)  {
		fail=time.rows(1,endf);
		nf=qty.rows(1,endf);
	}

			if(N[1]>0)  {
		susp=time.rows(endf+1,ends);
		ns=qty.rows(endf+1,ends);
	}
	
	if(N[2]>0)  {
		disc=time.rows(ends+1,endd);
		nd=qty.rows(ends+1,endd);
	}
	if(N[3]>0)  {
		left=time.rows(endd+1,endil);
		right=time.rows(endil+1,endir);
		ni=qty.rows(endd+1,endil);
	}

}



double MLEmodel::LogLike(arma::colvec par, int sign, int dist_num)  {

	double failcomp =0.0;
	double suscomp =0.0;
	double discomp =0.0;
	double intcomp =0.0;

if(dist_num==1) {
	if(N[0]>0)  {
		for(int i=0; i<N[0]; i++)  {
			failcomp=failcomp+nf(i)*R::dweibull(fail(i),par(0),par(1),1);
		}
	}

	if(N[1]>0)  {
		for(int i=0; i<N[1]; i++)  {
			suscomp=suscomp+ns(i)*R::pweibull(susp(i),par(0),par(1),0,1);
		}
	}
	if(N[2]>0)  {
		for(int i=0; i<N[2]; i++)  {
			discomp=discomp+nd(i)*log(1-R::pweibull(disc(i),par(0),par(1),0,0));
		}
	}
	if(N[3]>0)  {
		for(int i=0; i<N[3]; i++)  {
			intcomp=intcomp+ni(i)*log(
			R::pweibull(left(i),par(0),par(1),0,0) -
			R::pweibull(right(i),par(0),par(1),0,0)
			);
		}
	}

}
else if(dist_num==2)  {
	if(N[0]>0)  {
		for(int i=0; i<N[0]; i++)  {
		failcomp=failcomp+nf(i)*R::dlnorm(fail(i),par(0),par(1),1);
		}
	}

	if(N[1]>0)  {
		for(int i=0; i<N[1]; i++)  {
			suscomp=suscomp+ns(i)*R::plnorm(susp(i),par(0),par(1),0,1);
		}
	}
	if(N[2]>0)  {
		for(int i=0; i<N[2]; i++)  {
			discomp=discomp+nd(i)*log(1-R::plnorm(disc(i),par(0),par(1),0,0));
		}
	}
	if(N[3]>0)  {
		for(int i=0; i<N[3]; i++)  {
			intcomp=intcomp+ni(i)*log(
			R::plnorm(left(i),par(0),par(1),0,0) -
			R::plnorm(right(i),par(0),par(1),0,0)
			);
		}
	}

}

return sign*(failcomp+suscomp+discomp+intcomp);
}

SEXP MLEmodel::MLEsimplex( SEXP arg1)  {	
	Rcpp::List L(arg1);
	int dist_num=Rcpp::as<int>(L["dist_num"]);
	arma::colvec vstart=Rcpp::as<arma::colvec>(L["vstart"]);
	double limit=Rcpp::as<double>(L["limit"]);
	int maxit=Rcpp::as<int>(L["maxit"]);
		
// this algorithm is optimized specificity for the two parameter case				
// variables to hold number of  parameters and number of vertices are vestigial				
	int npar=2, k= npar+1;			
 // coefficients for reflection, expansion, and contraction				
	double ALPHA=1.0, BETA=0.5, GAMMA=2.0;			
 // set the sign for minimization of negative LogLikelihood				
	int sign= -1;			
				
 // construct the initial simplex				
	arma::mat v(2,3);			
	for(int i=0;i<k;i++)  {			
	v.col(i)=vstart;			
	}			
	v(0,1)=v(0,1)*1.2;			
	v(1,2)=v(1,2)*1.2;			
				
	arma::colvec funval(3);			
	for(int i=0;i<k;i++)  {			
	funval(i)=LogLike(v.col(i), sign, dist_num );			
	}			
				
				
				
 // assign vertex order variables				
	arma::uvec ndx=sort_index(funval);			
	int vs=(int) ndx(0);			
	int vh=(int) ndx(1);			
	int vg=(int) ndx(2);			
 // generate the initial error measure				
	double P2avg=arma::as_scalar(mean(v.row(1)));			
	double error=arma::as_scalar(sum(pow((v.row(1)-P2avg),2.0)/npar));			
 // initialize the output dataframe for construction as a matrix				
	arma::rowvec DFrow(4);			
	DFrow(0)=v(0,vs);			
	DFrow(1)=v(1,vs);			
	DFrow(2)=funval(vs);			
	DFrow(3)=error;			
	arma::mat DF=DFrow;			
				
 // initialization of variables used in the loop				
	arma::colvec vm, vr, ve, vc;			
	double fr, fe, fc;			
	int loopcount=1;
	int warn=0;
		
 // This is the main loop for the minimization				
	while(error>limit)  {			
 // calculate the centroid				
	vm=(v.col(vs)+v.col(vh))/2.0;			
 // reflect vg to new vertex vr				
	vr=(vm+ALPHA*(vm-v.col(vg)));			
	fr=LogLike(vr, sign, dist_num);			
 // depending on success, save reflected vertex in place of vg				
	if (fr < funval(vh) && fr >= funval(vs)) {			
	v.col(vg)=vr;			
	funval(vg)=fr;			
	}else{			
		if(funval(vs)<fr)  {		
				
 // test for an outside contraction				
		if (fr < funval(vg) && fr >= funval(vh))   {		
			vc=vm+BETA*(vr-vm);	
			}else{	
 // this is an inside contraction				
				vc=vm-BETA*(vr-vm);
			}	
			fc=LogLike(vc, sign, dist_num);	
 // upon acceptance replace vg with contracton				
			if (fc < funval(vg)) {	
				v.col(vg) = vc;
				funval(vg)=fc;
			}else{	
 // upon rare-to-never case shrink the simplex				
				v.col(vh)=v.col(vs)+(v.col(vh)-v.col(vs))/2.0;
				v.col(vg)=v.col(vs)+(v.col(vg)-v.col(vs))/2.0;
 // This case results in two function calculations and simply replacing vh and vg points				
				funval(vh)=LogLike(v.col(vh), sign, dist_num);
				funval(vg)=LogLike(v.col(vg), sign, dist_num);
			}	
		}else{		
// now we make an expansion				
			ve=vm+GAMMA*(vr-vm);	
			fe=LogLike(ve, sign, dist_num);	
 // store the better of reflection or expansion in place of vg				
			if (fe < fr) {	
				v.col(vg) =ve;
				funval(vg)=fe;
			}else{	
				v.col(vg) = vr;
				funval(vg)=fr;
			}	
		}		
	}			
 //re-assign vertex order variables 				
	ndx=sort_index(funval);			
	vs=(int) ndx(0);			
	vh=(int) ndx(1);			
	vg=(int) ndx(2);			
				
	P2avg=arma::as_scalar(mean(v.row(1)));			
	error=arma::as_scalar(sum(pow((v.row(1)-P2avg),2.0)/npar));			
	DFrow(0)=v(0,vs);			
	DFrow(1)=v(1,vs);			
	DFrow(2)=funval(vs);			
	DFrow(3)=error;			
	DF=join_cols(DF,DFrow);

	loopcount=loopcount+1;	
	if(loopcount>maxit)  {	
		warn=1;
		break;
	}	
	
				
// then close main iteration loop				
	}			
				
				
	Rcpp::NumericVector outvec(4);			
	if(dist_num==1) {			
	outvec[0]=v(1,vs);			
	outvec[1]=v(0,vs);			
	}else{			
	outvec[0]=v(0,vs);			
	outvec[1]=v(1,vs);			
	}
// note: multiplication by sign here assures positive log-likelihood is delivered	
	outvec[2]=sign*funval(vs);			
	outvec[3]=warn;
			
	return List::create(outvec, wrap(DF));
		
	}	

	// Exported Functions

	SEXP MLEloglike(SEXP arg1, SEXP arg4, SEXP arg5)  {
		MLEmodel mymodel(arg1);
		arma::colvec par=Rcpp::as<arma::colvec>(arg4);
		int dist_num=Rcpp::as<int>(arg5);
		int sign=1;
		return wrap(mymodel.LogLike(par, sign, dist_num));
	}

	SEXP MLEsimplex(SEXP arg1, SEXP arg2)  {
		MLEmodel mymodel(arg1);
		return mymodel.MLEsimplex(arg2);

	}



