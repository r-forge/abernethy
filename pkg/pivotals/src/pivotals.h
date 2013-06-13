#ifndef _pivotals_H
#define _pivotals_H

#include <RcppArmadillo.h>


RcppExport SEXP pivotalMCw2p(SEXP arg1, SEXP arg2, SEXP arg3, SEXP arg4, SEXP arg5, SEXP arg6);
RcppExport SEXP pivotalMCln2p(SEXP arg1, SEXP arg2, SEXP arg3, SEXP arg4, SEXP arg5, SEXP arg6);
//RcppExport SEXP wbtk_pivotalMCw2(SEXP arg1, SEXP arg2, SEXP arg3, SEXP arg4, SEXP arg5, SEXP arg6);
RcppExport SEXP medianRank (SEXP arg1);
RcppExport SEXP medianRank1 (SEXP arg1);
RcppExport SEXP MRRw2pXonY (SEXP arg1, SEXP arg2);
RcppExport SEXP MRRln2pXonY (SEXP arg1, SEXP arg2);
RcppExport SEXP MRRln2pYonX (SEXP arg1, SEXP arg2, SEXP arg3);


#endif
