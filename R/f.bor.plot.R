f.bor.plot <-
function(border., x.left, x.right, py, line, sn., point.type, 
                       point.color, label.color, marker.cex)
{
    abline(h = py, lty = 2, col = label.color)
    range. <- which(border. >= x.left & border. <= x.right)
    border.. <- border.[range.]
    sn.. <- sn.[range.]
    if (length(border..) >= 1) {
        points(border.., rep(py, time = length(border..)), type = "p", 
               pch = point.type, col = point.color, cex = marker.cex)
        position <- ifelse(line == "u", 2, -1)
        text(border.., rep(py, time = length(border..)), sn.., 
             adj = c(1.25, position), col = label.color, cex = marker.cex)
    }
}
