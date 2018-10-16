r.det <-
function(seg., py, px2){
    py.col <- nrow(seg.) - py
    avg.g <- seg.[py.col, ]
    
    diff.g <- c(0, diff(avg.g, lag = 1))
    border <- which(diff.g != 0)
    border. <- border + px2 - 1
    if (length(border.) == 0) 
        stop("Ring border was not detected")
    return(border.)
}
