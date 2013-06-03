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
options.abrem <- function(...){
    # function to handle the many options of the weibull toolkits functions
    # the option list should only be manipulated through this function!

    single <- FALSE
    args <- list(...)

    if(!exists(as.character(substitute(options_abrem))))
        # if the globally accessible variable was not defined yet, then
        # create it here with default values OR reset to default values
        # message ("Resetting Weibulltoolkit options to default values...")
        options_abrem <<- list(
            dist="weibull",
            method.fit=c("mrr","qbeta","xony"),
            conf.what="blives",
            conf.blives.sides="double",
            conf.n=25,
            method.conf.blives=c("mcpivotal","bbb"),
            S=1e4,
            pivotals=FALSE,
            cl=0.9,
            blives=c(0.1,0.05,0.01),
            verbosity=0)
    if (!length(args))
        args <- options_abrem
           # return the current option list
    else {
        if (all(unlist(lapply(args, is.character))))
            # if all items in the args are characters, then
            # treat them as the names of the options.
            args <- as.list(unlist(args))
        if (length(args) == 1) {
            if (is.list(args[[1L]]) | is.null(args[[1L]]))
               args <- args[[1L]]
               # unlist the first (and only) argument to a string
         else if(is.null(names(args)))
            # if there is no name to args then
            # the arg itself is the name (?)
            single <- TRUE
         }}
   options_abrem <<-
      modifyList(options_abrem, value <- args)
   if (is.null(names(args)))
      value <- options_abrem[match(args,names(options_abrem))]
   if (single) value <- value[[1L]]
   value}
# note that options that are NULL are not shown in the printout -> needs to change
