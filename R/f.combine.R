f.combine <-
function(init.p,border.,dp){
    len. <- length(init.p)
    for (i in 1:len.) {
        p.range <- which(border. - init.p[i] <= (dp/10) & 
                             border. - init.p[i] >= 0)
        init.p[i] <- mean(border.[p.range])
    }
    return(init.p)
}
