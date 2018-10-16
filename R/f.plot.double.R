f.plot.double <-
function(segmented.label.list.u, bor.u,
                          segmented.label.list.l, bor.l,
                          x.left, x.right, seg.name, ring.data, i, x.left1, 
                          ybottom, ytop, py.upper, py.lower, sn.u, sn.l, 
                          point.type, point.color, label.color, marker.cex)
{
    bor.list.u <- segmented.label.list.u$bor.list[[i]]
    rw.list.u <- segmented.label.list.u$rw.list[[i]]
    yr.list.u <- segmented.label.list.u$yr.list[[i]]
    
    bor.list.l <- segmented.label.list.l$bor.list[[i]]
    rw.list.l <- segmented.label.list.l$rw.list[[i]]
    yr.list.l <- segmented.label.list.l$yr.list[[i]]
    
    layout(matrix(c(1, 2, 2, 3), 4, 1))
    par(mar = c(1.75, 5, 2, 0), mfg = c(1, 1))
    if (length(bor.list.u != 0)) {
        f.rw.plot(bor.list.u, rw.list.u, yr.list.u, 
                  x.left, x.right, seg.name, 'upper')
    } else {
        plot(0, 0, type = "p", pch = 4, axes = F, cex = 3, 
             ylab = "", col = "red")
        title(main = seg.name, cex.main = 1.5, line = -1)
        text(0, 0, labels = "Ring border was not detected along the path", 
             adj = c(0.5, -2), cex = 1.2)
    }
    par(mar = c(2, 5, 1.25, 0), mfg = c(3, 1))
    if (length(bor.list.l != 0)) {
        f.rw.plot(bor.list.l, rw.list.l, yr.list.l, 
                  x.left, x.right, seg.name, 'lower')
    } else {
        plot(0, 0, type = "p", pch = 4, axes = F, cex = 3, 
             ylab = "", col = "red")
        text(0, 0, labels = "Ring border was not detected along the path", 
             adj = c(0.5, -2), cex = 1.2)
    }
    par(mar = c(1.25, 5, 1.25, 0), mfg = c(2, 1))
    f.img.middle(ring.data, x.left, x.right, ybottom, ytop, x.left1, T)
    f.bor.plot(bor.u, x.left, x.right, py.upper, "u", sn.u, point.type, 
               point.color, label.color, marker.cex)
    f.bor.plot(bor.l, x.left, x.right, py.lower, "l", sn.l, point.type, 
               point.color, label.color, marker.cex)
    f.marker(bor.u, x.left, x.right, ybottom, ytop, py.upper, point.type, 
             point.color, label.color, marker.cex, 'upper')
    f.marker(bor.l, x.left, x.right, ybottom, ytop, py.lower, point.type, 
             point.color, label.color, marker.cex, 'lower')
}
