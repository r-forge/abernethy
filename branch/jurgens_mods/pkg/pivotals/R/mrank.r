mrank<-function(x)  {
## WARNING
## this wrapper function is currently provided with no error checking
outvec<-.Call("medianRank",x, PACKAGE= "pivotals")
outvec
}