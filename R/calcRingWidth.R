calcRingWidth <-
function(ring.data, seriesID){
    rd.attributes <- attributes(ring.data)
    
    x.dpi <- rd.attributes$x.dpi
    dp <- x.dpi/25.4
    incline <- rd.attributes$incline
    sample.yr <- rd.attributes$sample.yr
    path.dis <- rd.attributes$path.dis
    bor.u <- rd.attributes$bor.u
    bor.l <- rd.attributes$bor.l
    bor. <- rd.attributes$bor.

    
    if (!incline) {
        bx <- sort(bor.)
        lenbx <- length(bx)
        if (lenbx <= 1) 
            stop('A minimum of two ring borders on each path ',
                 'was required to generate a ring-width series')
        diff.col.num <- diff(bx)
        rw <- round(diff.col.num/dp, 2)
        years <- c((sample.yr-1):(sample.yr - lenbx + 1))
        df.rw <- data.frame(rw)
    }
    
    if (incline) {
        bx.up <- bor.u
        lenup <- length(bx.up)
        if (lenup >= 1) {
            if (lenup == 1) 
                stop('A minimum of two ring borders on each path ',
                     'was required to generate a ring-width series')
            diff.col.num.up <- diff(bx.up)
        }
        bx.lower <- bor.l
        lenlo <- length(bx.lower)
        if (lenlo >= 1) {
            if (lenlo == 1) 
                stop('A minimum of two ring borders on each path ',
                     'was required to generate a ring-width series')
            diff.col.num.lower <- diff(bx.lower)
        }
        
        if (lenlo != lenup) 
            stop("If incline = TRUE, the upper and lower paths ", 
                 "should have the same number of ring borders")
        years <- c((sample.yr-1):(sample.yr - lenup + 1))
        mean.bor <- (diff.col.num.lower + diff.col.num.up)/2
        x.cor <- abs(bx.lower - bx.up)
        x.cor <- x.cor[-length(x.cor)]
        correct.rw <- mean.bor * cos(atan(x.cor/(dp * path.dis)))
        correct.rw <- round(correct.rw/dp, 2)
        df.rw <- data.frame(correct.rw)

    }
    rownames(df.rw) <- years
    colnames(df.rw) <- seriesID
    return(df.rw)
}
