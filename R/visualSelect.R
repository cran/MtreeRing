visualSelect <-
function(ring.data, del = NULL, del.u = NULL, del.l = NULL, add = FALSE)
{
    rd.attributes <- attributes(ring.data)
    
    x.dpi <- rd.attributes$x.dpi
    RGB <- rd.attributes$RGB
    grD.num <- rd.attributes$grD.num
    seg <- rd.attributes$seg
    incline <- rd.attributes$incline
    py <- rd.attributes$py
    px2 <- rd.attributes$px2
    py2 <- rd.attributes$py2
    px3 <- rd.attributes$px3
    py3 <- rd.attributes$py3
    sample.yr <- rd.attributes$sample.yr
    path.dis <- rd.attributes$path.dis
    border.type <- rd.attributes$border.type
    border.color <- rd.attributes$border.color
    label.color <- rd.attributes$label.color
    label.cex <- rd.attributes$label.cex
    x.left <- rd.attributes$x.left
    x.right <- rd.attributes$x.right
    seg.grD.num <- rd.attributes$seg.grD.num
    img.name <- rd.attributes$img.name
    py.upper <- rd.attributes$py.upper
    py.lower <- rd.attributes$py.lower
    bor.u <- rd.attributes$bor.u
    bor.l <- rd.attributes$bor.l
    sn.u <- rd.attributes$sn.u
    sn.l <- rd.attributes$sn.l
    year.u <- rd.attributes$year.u
    year.l <- rd.attributes$year.l
    bor. <- rd.attributes$bor.
    sn. <- rd.attributes$sn.
    year. <- rd.attributes$year.

    dp <- x.dpi/25.4
    
    
    grD.list <- as.numeric(dev.list())
    exist. <- is.element(seg.grD.num, grD.list)
    exist.seg <- seg.grD.num[exist.]
    
    del.cond <- c(is.null(del), is.null(del.u), is.null(del.l))
    
    if(!all(del.cond)) {
        
        
        if(incline){
            if(!all(is.element(del.l,sn.l)))
                stop('The ring number on the lower path you entered was not correct')
            if(!all(is.element(del.u,sn.u)))
                stop('The ring number on the upper path you entered was not correct')
            if(!is.null(del.l)) bor.l <- bor.l[-del.l]
            if(!is.null(del.u)) bor.u <- bor.u[-del.u]
        } else {
            if(!all(is.element(del,sn.)))
                stop('The ring number you entered was not correct')
            if(!is.null(del)) bor. <- bor.[-del]
        }
        for (i in exist.seg) dev.off(i)
        
        
        if(incline){
            
            label.list.l <- create.label(bor.l, sample.yr, dp)
            label.list.u <- create.label(bor.u, sample.yr, dp)
            
            dfrw.l <- label.list.l$dfrw
            dfrw.u <- label.list.u$dfrw
            segmented.label.list.l <- split.label(dfrw.l, x.left, x.right, seg)
            segmented.label.list.u <- split.label(dfrw.u, x.left, x.right, seg)
        } else {
            label.list. <- create.label(bor., sample.yr, dp)
            dfrw. <- label.list.$dfrw
            segmented.label.list. <- split.label(dfrw., x.left, x.right, seg)
        }
        
        seg.name <- paste(img.name, "Section", 1:seg)
        seg.grD.num <- vector(length = 0)
        ybottom <- py2
        ytop <- nrow(ring.data)   
        for(i in 1:seg) {
            dev.new()
            
            if (incline) {
                f.plot.double(segmented.label.list.u, bor.u, segmented.label.list.l, bor.l, 
                              x.left[i], x.right[i], seg.name[i], ring.data, i, x.left[1], 
                              ybottom, ytop, py.upper, py.lower, label.list.u$sn, 
                              label.list.l$sn, border.type, border.color, label.color, label.cex)
            } else {
                f.plot.single(segmented.label.list., bor., x.left[i], x.right[i], seg.name[i], 
                              ring.data, i, x.left[1], ybottom, ytop, py, 
                              label.list.$sn, border.type, border.color, label.color, label.cex)
            } 
            seg.grD.num[i] <- as.numeric(dev.cur())
            exist.seg[i] <- as.numeric(dev.cur())
        }
    }
    
    
    if(add) {
        if(all(!exist.))
            stop('All graphical windows have been closed. You can not mark new ring boundaries.')
        
        add. <- vector(length = 0)
        add.l <- vector(length = 0)
        add.u <- vector(length = 0)
        for(i in exist.seg){
            dev.set(i)
            bor.xy <- locator(type = "p",pch=16)
            bor.x <- bor.xy$x
            bor.y <- bor.xy$y
            if(!is.null(bor.x)){
                if(incline){
                    upper <- which((bor.y - py)>0)
                    lower <- which((bor.y - py)<0)
                    if(length(lower) != 0){
                        add.l. <- bor.x[lower]
                        add.l. <- sort(add.l.)
                        add.l <- c(add.l, add.l.)
                    }
                    if(length(upper) != 0){
                        add.u. <- bor.x[upper]
                        add.u. <- sort(add.u.)
                        add.u <- c(add.u, add.u.)
                    }
                } else {
                    add.. <- sort(bor.x)
                    add. <- c(add., add..)
                }
            }
            dev.off(i)
        } 
        
        if(incline) {
            if(length(add.l) != 0){
                bor.l <- c(bor.l, add.l)
                bor.l <- sort(bor.l)
            }
            if(length(add.u) != 0){
                bor.u <- c(bor.u, add.u)
                bor.u <- sort(bor.u)
            }
        } else {
            bor. <- c(bor., add.)
            bor. <- sort(bor.)
        }
        
        if(incline){
            
            label.list.l <- create.label(bor.l, sample.yr, dp)
            label.list.u <- create.label(bor.u, sample.yr, dp)
            
            dfrw.l <- label.list.l$dfrw
            dfrw.u <- label.list.u$dfrw
            segmented.label.list.l <- split.label(dfrw.l, x.left, x.right, seg)
            segmented.label.list.u <- split.label(dfrw.u, x.left, x.right, seg)
        } else {
            label.list. <- create.label(bor., sample.yr, dp)
            dfrw. <- label.list.$dfrw
            segmented.label.list. <- split.label(dfrw., x.left, x.right, seg)
        }
        
        seg.name <- paste(img.name, "Section", 1:seg)
        seg.grD.num <- vector(length = 0)
        ybottom <- py2
        ytop <- nrow(ring.data)   
        for(i in 1:seg){
            dev.new()
            
            if (incline) {
                f.plot.double(segmented.label.list.u, bor.u, segmented.label.list.l, bor.l, 
                              x.left[i], x.right[i], seg.name[i], ring.data, i, x.left[1], 
                              ybottom, ytop, py.upper, py.lower, label.list.u$sn, 
                              label.list.l$sn, border.type, border.color, label.color, label.cex)
            } else {
                f.plot.single(segmented.label.list., bor., x.left[i], x.right[i], seg.name[i], 
                              ring.data, i, x.left[1], ybottom, ytop, py, 
                              label.list.$sn, border.type, border.color, label.color, label.cex)
            } 
            seg.grD.num[i] <- as.numeric(dev.cur()) 
        }
    }
    
    
    if(all(del.cond) & !add) 
        stop('The user must perform one of the two operations (addition or deletion)')
    
    if(incline){
        attributes(ring.data) <- c(attributes(ring.data),list(bor.u = bor.u, bor.l = bor.l,
                                sn.u = label.list.u$sn, sn.l = label.list.l$sn, seg.grD.num = seg.grD.num,
                                year.u = label.list.u$year, year.l = label.list.l$year))
    } else {
        attributes(ring.data) <- c(attributes(ring.data),
                                 list(bor. = bor., sn. = label.list.$sn, 
                                      seg.grD.num = seg.grD.num,
                                      year. = label.list.$year))
    }
    return(ring.data)
}
