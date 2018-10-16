is.early2late <-
function(canny.seg, seg.) {
    canny.seg. <- canny.seg[, , 1, 1] 
    
    bor.canny <- which(canny.seg. == TRUE, arr.ind = TRUE)
    first. <- which(bor.canny[, 2] == 1)
    end. <- which(bor.canny[, 2] == ncol(seg.))
    if (length(c(first., end.)) != 0) 
        bor.canny <- bor.canny[-c(first., end.), ]
    bor.row <- bor.canny[, 1]
    bor.col <- bor.canny[, 2]
    bor.row. <- integer()
    bor.col. <- integer()
    for (i in 1:length(bor.row)) {
        if (seg.[bor.row[i], bor.col[i] + 1] - seg.[bor.row[i], bor.col[i] - 1] < 0) {
            bor.row. <- c(bor.row., bor.row[i])
            bor.col. <- c(bor.col., bor.col[i])
        }
    }
    early2late <- matrix(c(bor.row., bor.col.), ncol = 2)
    return(early2late)
}
