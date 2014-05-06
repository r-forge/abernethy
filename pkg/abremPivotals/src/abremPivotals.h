#ifndef _abremPivotals_H
#define _abremPivotals_H

#include <RcppArmadillo.h>


//RcppExport SEXP pivotalMCw2p(SEXP arg1, SEXP arg2, SEXP arg3, SEXP arg4, SEXP arg5, SEXP arg6);
//RcppExport SEXP pivotalMCln2p(SEXP arg1, SEXP arg2, SEXP arg3, SEXP arg4, SEXP arg5, SEXP arg6);

RcppExport SEXP MRRw2pXonY (SEXP arg1, SEXP arg2);
RcppExport SEXP MRRw2pYonX (SEXP arg1, SEXP arg2);
RcppExport SEXP MRRln2pXonY (SEXP arg1, SEXP arg2);
RcppExport SEXP MRRln2pYonX (SEXP arg1, SEXP arg2);
RcppExport SEXP MRRg2pXonY (SEXP arg1, SEXP arg2);
RcppExport SEXP MRRg2pYonX (SEXP arg1, SEXP arg2);

//RcppExport SEXP MRRw3pXonY (SEXP arg1, SEXP arg2, SEXP arg3);


#endif
