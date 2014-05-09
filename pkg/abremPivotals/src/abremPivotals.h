#ifndef _abremPivotals_H
#define _abremPivotals_H

#include <RcppArmadillo.h>


//RcppExport SEXP pivotalMCw2p(SEXP arg1, SEXP arg2, SEXP arg3, SEXP arg4, SEXP arg5, SEXP arg6);
//RcppExport SEXP pivotalMCln2p(SEXP arg1, SEXP arg2, SEXP arg3, SEXP arg4, SEXP arg5, SEXP arg6);

RcppExport SEXP LSLRw2pXonY (SEXP arg1, SEXP arg2);
RcppExport SEXP LSLRw2pYonX (SEXP arg1, SEXP arg2);
RcppExport SEXP LSLRln2pXonY (SEXP arg1, SEXP arg2);
RcppExport SEXP LSLRln2pYonX (SEXP arg1, SEXP arg2);
RcppExport SEXP LSLRg2pXonY (SEXP arg1, SEXP arg2);
RcppExport SEXP LSLRg2pYonX (SEXP arg1, SEXP arg2);

RcppExport SEXP LSLR(SEXP arg1, SEXP arg2, SEXP arg3, SEXP arg4);


#endif
