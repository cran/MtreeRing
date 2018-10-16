f.border <-
function(seg.,py,px2,dp){
    border1 <- r.det(seg., py, px2)
    border2 <- f.sort(border1, dp)
    
    return(border2)
}
