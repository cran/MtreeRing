f.plot.single <-
function(segmented.label.list, bor., x.left, x.right, seg.name, 
                          ring.data, i, x.left1, ybottom, ytop, py, sn., 
                          point.type, point.color, label.color, marker.cex)
{
    bor.list <- segmented.label.list$bor.list[[i]]
    rw.list <- segmented.label.list$rw.list[[i]]
    yr.list <- segmented.label.list$yr.list[[i]]
    
    layout(matrix(c(1, 2, 2), 3, 1))
    par(mar = c(1.25, 5, 2, 0))
    if (length(bor.list != 0)) {
        f.rw.plot(bor.list, rw.list, yr.list, 
                  x.left, x.right, seg.name, 'upper')
    } else {
        plot(0, 0, type = "p", pch = 4, axes = F, 
             cex = 3, ylab = "", col = "red")
        title(main = seg.name, cex.main = 1.5, line = -1)
        text(0, 0, labels = "Ring border was not detected along the path", 
             adj = c(0.5, -2), cex = 1.2)
    }
    par(mar = c(5, 5, 1.25, 0), mfg = c(2, 1))
    f.img.middle(ring.data, x.left, x.right, ybottom, ytop, x.left1, F)
    f.bor.plot(bor., x.left, x.right, py, "u", sn., point.type, 
               point.color, label.color, marker.cex)
    f.marker(bor., x.left, x.right, ybottom, ytop, py, point.type, 
             point.color, label.color, marker.cex, 'upper')
}
