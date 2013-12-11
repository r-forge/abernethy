contourRange <- function(MLEXContour){
    ra <- do.call("rbind",MLEXContour)
    data.frame(range(ra[,1]),range(ra[,2]))
}

findContourRanges <- function(x,v){
    # +---------------------------------------------------+
    # |  find absolute maximum and minimum contour range  |
    # |     over the (list of) abrem objects              |
    # +---------------------------------------------------+
    # x is always a list of abrem object(s)

    findrange1 <- function(abrem){
        if(!is.null(abrem$fit)){
            findrange2 <- function(fit){
                if(!is.null(fit$conf$blives)){
                    findrange3 <- function(blicon){
                        if(!is.null(blicon$MLEXContour)){
                            # a contour is available
                            #if(deb)mtrace(contourRange)
                            contourRange(blicon$MLEXContour)
                        }
                    }
                    #if(deb)mtrace(findrange3)
                    do.call("rbind",lapply(fit$conf$blives,findrange3))
                        # combine the ranges from all MLEXContours
                        # found in the list of blicons
                }
            }
            #if(deb)mtrace(findrange2)
            do.call("rbind",lapply(abrem$fit,findrange2))
                # combine the ranges from all MLEXContours
                # found in the list of fits
        }
    }
    #if(deb)mtrace(findrange1)
    do.call("rbind",lapply(x,findrange1))
}
