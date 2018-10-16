create.label <-
function(bor., sample.yr, dp) {
    year. <- c(sample.yr:(sample.yr - length(bor.) + 1))
    sn. <- c(1:length(bor.))
    rw <- (diff(bor.))/dp
    year.rw <- year.[-1]
    dfrw <- data.frame(bor.[-1], rw, year.rw)
    label.list <- list(year = year., sn = sn., rw = rw,
                       year.rw = year.rw, dfrw = dfrw)
    return(label.list)
}
