\name{print.abrem}
\alias{print.abrem}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
    \code{Abrem} Object Printing
}
\description{
    This function prints the \code{abrem} object while omitting the printing of the
    long \code{$options} named list (for clarity).
}
\usage{\method{print}{abrem}(x, \dots)}
\arguments{
    \item{x}{
        Object of class \code{"abrem"}.
    }
    \item{\dots}{
        Further arguments passed to or from other methods.
    }
}

\author{Jurgen Symynck \email{jurgen.symynck@kahosl.be}}

\examples{
data(p2.2,package="abrem")
da <- abrem.fit(Abrem(p2.2))
print(da)
}
