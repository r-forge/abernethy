# R package 'abremout'
# output methods for the abrem object
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
mark <- function(x,y,lwd=2,lty=2,col="black",type="b",pch=4){
   # draw lines from a particular point of interest towards both axis
   pu <- par("usr")
      # read a vector of the form ‘c(x1, x2, y1, y2)’ giving the
      # extremes of the user coordinates of the plotting region.
   if(length(x) == length(y)){
      for(i in 1:length(x)){
         a <- c((10^pu[1])/10,rep(x[i],2))
         b <- c(rep(F0inv(y[i]),2),F0inv(F0(pu[3])/10))
         points(x=a,y=b,lwd=lwd,lty=lty,col=col,type=type,pch=pch)
            # plot a vertical and horizontal line from the edge of the
            # plotting region to a point of interest
         }}}