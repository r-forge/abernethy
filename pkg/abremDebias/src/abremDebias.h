#ifndef _abremDebias_H
#define _abremDebias_H

#ifdef __cplusplus
 
#include <RcppArmadillo.h>

RcppExport SEXP MLEloglike(SEXP arg1, SEXP arg3, SEXP arg4, SEXP arg5, SEXP arg6);
RcppExport SEXP MLEsimplex(SEXP arg1, SEXP arg2, SEXP arg3, SEXP arg4, SEXP arg5);
// exported for testing purposes only. Never called from R.
RcppExport SEXP MLEdMaxLLdx(SEXP arg1, SEXP arg2, SEXP arg3, SEXP arg4);
//******************//

#endif
#endif