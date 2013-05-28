# R package 'abrem'
# Abernethy Reliability Methods
# Implementations of lifetime data analysis methods described in
# 'The New Weibull Handbook, Fifth edition' by Dr. Robert B. Abernethy.
# May 2013, Jurgen Symynck
# Copyright 2013, Jurgen Symynck
#
# For the latest version of this file, check the Subversion repository at
# http://r-forge.r-project.org/projects/abernethy/
#
# Disclaimer:
#    The author is not affiliated with Dr. Abernethy or Wes Fulton - CEO of
#    Fulton Findings(TM) and author of the software package SuperSMITH
#-------------------------------------------------------------------------------
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#    For more info on this software and its predecesser, the "weibulltoolkit",
#    consult following documents:
#
#    - "Weibull analysis using R, in a nutshell",
#      (Jurgen Symynck, Filip De Bal, 2010)
#    - "Monte Carlo pivotal confidence bounds for Weibull analysis
#      with implementations in R",
#      (Jurgen Symynck, Filip De Bal, 2011)
#
# +-----------------------------------+
# |  execute this program with R:     |
# |  http://www.r-project.org/        |
# +-----------------------------------+
#
options.abremplot <- function(...){
   # function to handle the many options of the weibull toolkits functions
   # the option list should only be manipulated by this function!
   single <- FALSE
   args <- list(...)
   if(!exists(as.character(substitute(options_abremplot)))){
      # if the globally accessible variable was not defined yet, then
      # create it here with default values OR reset to default values
      # message ("Resetting Weibulltoolkit options to default values...")
      options_abremplot <<- list(
         main="Weibull Plot\n",
         sub=NULL,
         xlim=NULL,
         ylim=c(0.01,0.99),
         xlab="Time To Failure",
         ylab="Unreliability [%]",
         log="x",
         coordinate.text.size=0.7,
         signif=4,
         pch=1,
         lwd=2,
         lwd.points=2,
         lty=1,
         col="black",
         col.grid="gray",
         is.plot.grid=TRUE,
         is.plot.fittedline=TRUE,
         is.plot.datapoints=TRUE,
         is.plot.datacoordinates=FALSE,
         is.plot.legend=TRUE,
#         legend.position="bottomright",
         legend.text.size=0.7,
         legend.title=NULL,
         is.legend.blives=TRUE,
         is.legend.gof=FALSE,
         is.plot.cb = TRUE,
         persistent=TRUE)
   }
   if(!length(args)){
       args <- options_abremplot
         # return the current option list
   }else{
      if(all(unlist(lapply(args, is.character)))){
         # if all items in the args are characters, then
         # treat them as the names of the options.
         args <- as.list(unlist(args))
      }
      if(length(args) == 1) {
         if (is.list(args[[1L]]) | is.null(args[[1L]])){
            args <- args[[1L]]
            # unlist the first (and only) argument to a string
         }else{if(is.null(names(args)))
            # if there is no name to args then
            # the arg itself is the name (?)
            single <- TRUE
         }
      }
   }
   value <- args
   if(options_abremplot$persistent){
       options_abremplot <<-modifyList(options_abremplot, value)
   }
   if(!is.null(args$persistent)){
        value <- args
        if(args$persistent){
           options_abremplot <<-modifyList(options_abremplot, value)
        }
   }
       # make the options stick between calls of options.abremplot()
   if(is.null(names(args)))
      value <- options_abremplot[match(args,names(options_abremplot))]
   if(single) value <- value[[1L]]
   value}
# note that options that are NULL are not shown in the printout -> needs to change
