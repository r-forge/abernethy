\name{getPPP}
\alias{getPPP}

\title{ determination of probability plotting postions for linear regression with many optional methods}

\description{
Determination of plotting postions to be used for linear regression of life data analysis is a highly debated topic.  This function implements a wide assortment of options for the Abernethy Reliability Methods project.
}

\usage{
getPPP(x, s=NULL, ppos="Benard", aranks="Johnson", ties=NULL) 
}

\arguments{
  \item{x}{A vector of failure data.}
  \item{s}{An optional vector of suspension data.}
  \item{ppos}{A string defining a plotting position method.  Implemented options include "Benard" (as default), "beta" for the incomplete beta function, "mean" also known as Herd-Johnson, "Hazen" or modified Kaplan-Meier, "Kaplan-Meier" with modification for final complete failure, and "Blom.}
  \item{aranks}{A string defining the method for establishing adjusted ranks when suspension data (right censored) are present. Implemented options include "Johnson" (as default) and "KMestimator"}
  \item{ties}{A string defining a method of eliminating ties, or duplicate time valued data, from plotting. Implemented options include "highest" (used as 'Inspection Option #1' by Abernethy), "mean", "lowest", and "sequential".}
}


\value{
Returns a dataframe with the failure data (as potentially reduced by ties argument), the probability plotting positions, and adjusted ranks.
}

\references{
  Dr. Robert B. Abernethy, (2008) "The New Weibull Handbook, Fifth Edition"
  Leonard C. Johnson (1964) "The Statistical Treatment of Fatique Experiments"  
}

\examples{
median_ranks<-getPPP(c(1,0,1,1,0,1,0,0,1,1))[,2]
}

\keyword{ median ranks }