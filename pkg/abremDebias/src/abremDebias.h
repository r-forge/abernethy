#ifndef _abremDebias_H
#define _abremDebias_H

#ifdef __cplusplus
 
#include <RcppArmadillo.h>

RcppExport SEXP MLEloglike(SEXP arg1, SEXP arg4, SEXP arg5);
RcppExport SEXP MLEsimplex(SEXP arg1, SEXP arg2);

#endif
#endif