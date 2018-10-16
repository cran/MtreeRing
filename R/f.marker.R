f.marker <-
function(border., x.left, x.right, ybottom, ytop, py, point.type, 
                     point.color, label.color, marker.cex, location)
{
    border. <- border.[-1]
    range. <- which(border. >= x.left & border. <= x.right)
    border.. <- border.[range.]
    y.loc <- ifelse(location == 'upper', 
                    ybottom + 1.25 * ytop, 
                    ybottom - 0.25 * ytop)
    segments(border.., rep(py, length(border..)), 
             border.., rep(y.loc, length(border..)), 
             col = label.color, lty = 2, lwd = 2, lend = 2)
}
