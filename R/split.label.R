split.label <-
function(dfrw, x.left, x.right, seg){
    
    bor.list<-list()
    rw.list<-list()
    yr.list<-list()
    
    for (i in 1:seg) {
        range. <- which(dfrw[, 1] >= x.left[i] & 
                            dfrw[, 1] <= x.right[i])
        dfbor. <- dfrw[, 1][range.]
        dfrw. <- dfrw[, 2][range.]
        dfyr. <- dfrw[, 3][range.]
        bor.list <- c(bor.list, list(dfbor.))
        rw.list <- c(rw.list, list(dfrw.))
        yr.list <- c(yr.list, list(dfyr.))
    }
    split.list <- list(bor.list = bor.list, 
                       rw.list = rw.list, yr.list = yr.list)
    return(split.list)
}
