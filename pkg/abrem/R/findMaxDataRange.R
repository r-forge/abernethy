    # +-------------------------------------------+
    # |  find absolute maximum and minimum range  |
    # |     over the (list of) abrem objects      |
    # +-------------------------------------------+
findMaxDataRange <- function(x,v){
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
            if(!is.null(abrem$data$rank.median)){
                ret <- cbind(ret,yrange=range(abrem$data$rank.median,na.rm=TRUE))
                    # TODO: replace above testcode
            }else{
                stop("$data contains no rank column -> ",
                    "cannot create plot canvas.")
            }
        }else{stop("Argument \"x\" contains no \"$data\" dataframe.")}
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
    # TODO: the above still needed? because x is always lost of abrems?
    ret
}
