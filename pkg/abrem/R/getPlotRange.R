getPlotRangeX <- function(log){
    if(log %in% c("x","xy","yx")) 10^par("usr")[1:2]
    else par("usr")[1:2]
}

getPlotRangeY <- function(log){
    if(log %in% c("y","xy","yx")) 10^par("usr")[3:4]
    else par("usr")[3:4]
#    if(log %in% c("y","xy","yx"))) 10^par("usr")[1:2]
#    else par("usr")[1:2]
}
