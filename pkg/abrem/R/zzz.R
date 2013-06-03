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
.onLoad <- function(libname, pkgname, ...){
#    packageStartupMessage("Creating CCC2wb2()...")
    utils::data(CCC2table,package=pkgname,lib.loc=libname)
    # TODO: change the above and CCC2table itself to use readRDS
    fun1 <- stats::approxfun(
        x=CCC2table[,1],
        y=CCC2table[,2],method="constant",f=1)
        # 'f=1'ensures that when interpolation is happening,
        # the returned value is the highest of the possible
        # two choices -> allows for the most conservative comparison
    fun2 <- stats::approxfun(
        x=CCC2table[,1],
        y=CCC2table[,3],method="constant",f=1)
    CCC2wb2 <<- function(fail)list(
        CCC2  =fun1(fail),
        signif=fun2(fail))
#    packageStartupMessage("... finished creating CCC2wb2().")
    }