# R package 'abrem'
# Abernethy Reliability Methods
# Implementations of lifetime data analysis methods described in
# 'The New Weibull Handbook, Fifth edition' by Dr. Robert B. Abernethy.
# April 2014, Jurgen Symynck
# Copyright 2014, Jurgen Symynck
#
# For more info, visit http://www.openreliability.org/
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
# +-----------------------------------+
# |  execute this software with R:    |
# |  http://www.r-project.org/        |
# +-----------------------------------+

findMaxDataRange <- function(x,v,log=""){
    # +-------------------------------------------+
    # |  find absolute maximum and minimum range  |
    # |     over the (list of) abrem objects      |
    # +-------------------------------------------+
    # x is always a list of abrem object(s)
    findrange <- function(abrem){
        if(!is.null(abrem$data)){
            if(!is.null(abrem$data$time)){
                ret <- data.frame(xrange=range(abrem$data$time,na.rm=TRUE))
            }else{
                stop("$data contains no \"$time\" column -> ",
                    "cannot create plot canvas.")
            }
            if(!is.null(abrem$data$rank.median) || !is.null(abrem$data$rank.benard)){
                ret <- cbind(ret,yrange=range(
                    c(abrem$data$rank.median,abrem$data$rank.benard),na.rm=TRUE))
            }else{
                stop("$data contains no rank column -> ",
                    "cannot create plot canvas.")
            }
        }else{stop('Argument \"x\" contains no \"$data\" dataframe.')}
        ret
    }
#    if(identical(class(x),"abrem")){
#        if(v>= 2)message(match.call()[[1]],
#            ": Argument \"x\" is a single Abrem object...")
#        ret <- findrange(x)
#    }else{
    if(all(sapply(x,function(x)identical(class(x),"abrem")))){
        ret <- do.call("rbind",lapply(x,findrange))
    }else{
        stop("Argument \"x\" is no list of \"abrem\" objects.")
    }
    # TODO: the above still needed? because x is always list of abrems?
    if(tolower(log) %in% c("x","xy","yx")){
        # if log scale is to be used then omit zero and negative time values 
        # from the range dataset
        ret[ret$xrange <=0,1] <- NA
    }
    ret
}
