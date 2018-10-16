watershed.im <-
function(water.seg, seg.) {
    
    normalize <- function(x) return((x - min(x))/(max(x) - min(x)))
    imgra <- imgradient(as.cimg(seg.), axes = "y", scheme = 2)
    watershed.seg <- watershed(as.cimg(water.seg), imgra, fill_lines = T)
    watershed.seg. <- normalize(watershed.seg[, , 1, 1])
    return(watershed.seg.)
}
