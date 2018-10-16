f.sort <-
function(border.,dp){
    
    len. <- length(border.)
    init.p <- border.[1]
    if (len. >= 2) {
        for (i in 2:len.) {
            if (border.[i] - border.[i - 1] >= (dp/10)) 
                init.p <- c(init.p, border.[i])
        }
    }
    return(init.p)
}
