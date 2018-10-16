createServer <-
function(input, output, session) {
    
    
    f.morphological <- function(seg., struc.ele1, struc.ele2, x.dpi) {
        
        if (is.null(struc.ele1)) {
            stru.1 <- x.dpi/300
            struc.ele1 <- c(stru.1, stru.1) %>% round
        }
        
        if (is.null(struc.ele2)) {
            stru.2 <- x.dpi/80
            struc.ele2 <- c(stru.2, stru.2) %>% round
        }
        
        cim <- as.cimg(seg.)
        
        cim2 <- erode_rect(cim, sx = struc.ele1[1], sy = struc.ele1[2], sz = 1L)
        cim2 <- dilate_rect(cim2, sx = struc.ele1[1], sy = struc.ele1[2], sz = 1L)
        
        cim2 <- dilate_rect(cim2, sx = struc.ele1[1], sy = struc.ele1[2], sz = 1L)
        cim2 <- erode_rect(cim, sx = struc.ele1[1], sy = struc.ele1[2], sz = 1L)
        
        
        cim2 <- erode_rect(cim, sx = struc.ele2[1], sy = struc.ele2[2], sz = 1L)
        cim2 <- dilate_rect(cim2, sx = struc.ele2[1], sy = struc.ele2[2], sz = 1L)
        return(cim2)
    }
    
    
    hat <- function(seg.mor, x.dpi, watershed.threshold, watershed.adjust) {
        black.hat <- mclosing_square(seg.mor, size = round(x.dpi/10))
        black.hat <- black.hat - seg.mor
        black.hat <- threshold(black.hat, thr = watershed.threshold, 
                               approx = TRUE, adjust = watershed.adjust)
        
        black.hat <- 1 - black.hat
        black.hat.mat <- black.hat[, , 1, 1]
        return(black.hat.mat)
    }
    
    normalize <- function(x) {
        return((x - min(x))/(max(x) - min(x)))
    }
    
    
    correct.color <- function(water.c2) {
        
        color.adj <- function(i, water.c2, diff.m) {
            color.position <- which(water.c2 == i, arr.ind = T)
            row.range <- range(color.position[, 1])
            row.range <- row.range[1]:row.range[2]
            color.adjacent <- integer()
            for (j in row.range) {
                row.p <- which(color.position[, 1] == j)
                min.column <- color.position[row.p, 2] %>% min
                color.diff <- which(diff.m[, j] != 0)
                color.pre.p <- color.diff[which(color.diff == min.column) - 1] - 1
                color.pre <- water.c2[j, color.pre.p]
                color.adjacent <- c(color.adjacent, color.pre)
                
            }
            max(color.adjacent)
        }  
        
        
        
        water.c3 <- cbind(matrix(-1, nrow(water.c2), 1), 
                          matrix(0, nrow(water.c2), 1), 
                          water.c2)
        
        diff.m <- apply(water.c3, 1, function(x) c(0, diff(x)))
        
        
        color.max <- max(water.c2)
        
        df.color <- data.frame(color = c(1:color.max), 
                               adj = rep(NA, times = color.max))
        
        for (i in 1:color.max) {
            test.c <- color.adj(i, water.c3, diff.m)
            df.color[i, 2] <- test.c
        }
        for (i in -1:color.max) {
            adj.c <- which(df.color[, 2] == i) 
            if (length(adj.c) >= 2) {   
                max.c <- max(df.color[adj.c, 1])  
                covered.c <- sort(df.color[adj.c, 1])
                covered.c <- covered.c[-length(covered.c)]
                
                for (j in covered.c) {
                    cl = which(water.c3 == j, arr.ind = T)
                    water.c3[cl] = max.c
                    df.color[which(df.color == j, arr.ind = T)] = max.c
                }
            } 
        }
        return(water.c3[, -c(1, 2)])
    }
    
    
    water.im <- function(black.hat) {
        water.c <- connected(im(black.hat), background = 0, method = "C")
        
        water.c2 <- apply(water.c$v, 2, function(x){
            x[is.na(x)]<- 0
            return(x)
        })
        
        water.c2 <- correct.color(water.c2)
        return(water.c2)
    }
    
    watershed.im <- function(water.seg, seg.) {
        imgra <- imgradient(as.cimg(seg.), axes = "y", scheme = 2)
        watershed.seg <- watershed(as.cimg(water.seg), imgra, fill_lines = T)
        watershed.seg. <- normalize(watershed.seg[, , 1, 1])
        return(watershed.seg.)
    }
    
    
    is.early2late <- function(canny.seg, seg.) {
        canny.seg. <- canny.seg[, , 1, 1] 
        
        bor.canny <- which(canny.seg. == TRUE, arr.ind = TRUE)
        first. <- which(bor.canny[, 2] == 1)
        end. <- which(bor.canny[, 2] == ncol(seg.))
        if (length(c(first., end.)) != 0) 
            bor.canny <- bor.canny[-c(first., end.), ]
        bor.row <- bor.canny[, 1]
        bor.col <- bor.canny[, 2]
        bor.row. <- integer()
        bor.col. <- integer()
        for (i in 1:length(bor.row)) {
            if (seg.[bor.row[i], bor.col[i] + 1] - seg.[bor.row[i], bor.col[i] - 1] < 0) {
                bor.row. <- c(bor.row., bor.row[i])
                bor.col. <- c(bor.col., bor.col[i])
            }
        }
        early2late <- matrix(c(bor.row., bor.col.), ncol = 2)
        return(early2late)
    }
    
    
    r.det <- function(seg., py, px2){
        py.col <- nrow(seg.) - py
        avg.g <- seg.[py.col, ]
        
        diff.g <- c(0, diff(avg.g, lag = 1))
        border <- which(diff.g != 0)
        border. <- border + px2 - 1
        if (length(border.) == 0) 
            return(NULL)
        return(border.)
    }
    
    f.sort<-function(border.,dp){
        
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
    
    f.combine<-function(init.p,border.,dp){
        len. <- length(init.p)
        for (i in 1:len.) {
            p.range <- which(border. - init.p[i] <= (dp/10) & 
                                 border. - init.p[i] >= 0)
            init.p[i] <- mean(border.[p.range])
        }
        return(init.p)
    }
    
    f.border<-function(seg.,py,px2,dp){
        border1 <- r.det(seg., py, px2)
        if(is.null(border1)) 
            return(NULL)
        border2 <- f.sort(border1, dp)
        
        return(border2)
    }
    
    
    
    plot.marker <- function(py, incline, dp, sample.yr, h.dis, l.w, 
                            bor.color, lab.color, pch, marker.cex, 
                            df.loc, plot.year, img.name)
        
    {
        title(main = img.name)
        
        if (!is.null(py)) {
            abline(h = py, lty = 2, lwd = l.w, col = lab.color)
            if (incline) {
                abline(h = py, lty = 1, lwd = l.w, col = lab.color)
                h.dis. <- round((h.dis/2) * dp)
                py.upper <- py + h.dis.
                abline(h = py.upper, lty = 2, lwd = l.w, col = lab.color)
                py.lower <- py - h.dis.
                abline(h = py.lower, lty = 2, lwd = l.w, col = lab.color)
            }
        } else {
            
            return()
        }
        
        if (nrow(df.loc)>=3){
            bx <- df.loc$x[-c(1,2)]
            where.bx <- df.loc$z[-c(1,2)]
            where.bx <- where.bx[order(bx)]
            bx <- sort(bx)
            
            
            
            if (incline) {
                
                up <- which(where.bx > 0)
                lenup <- length(up)
                if (lenup >= 1) {
                    by.up <- rep(py.upper, time = lenup)
                    
                    points(bx[up], by.up, col = bor.color, type = "p", 
                           pch = pch, cex = marker.cex*0.75)
                    
                    if (plot.year) {
                        year.u <- c(sample.yr:(sample.yr - lenup + 1))
                        text(bx[up], by.up, year.u, adj = c(1.5, 0.5), 
                             srt = 90, col = lab.color, cex = marker.cex)
                        no. <- 1:lenup
                        text(bx[up], by.up, no., adj = c(0.5, -1.25), 
                             col = lab.color, cex = marker.cex)
                    }
                }
                
                lower <- which(where.bx < 0)
                lenlo <- length(lower)
                if (lenlo >= 1) {
                    by.lower <- rep(py.lower, time = lenlo)
                    points(bx[lower], by.lower, col = bor.color, type = "p", 
                           pch = pch, cex = marker.cex*0.75)
                    if (plot.year) {
                        year.l <- c(sample.yr:(sample.yr - lenlo + 1))
                        text(bx[lower], by.lower, year.l, adj = c(1.5, 0.5), 
                             srt = 90, col = lab.color, cex = marker.cex)
                        no. <- 1:lenlo
                        text(bx[lower], by.lower, no., adj = c(0.5, -1.25), 
                             col = lab.color, cex = marker.cex)
                    }
                }
            } else { 
                if (length(bx) >= 1) {
                    lenbx <- length(bx)
                    by <- rep(py, time = lenbx)
                    points(bx, by, col = bor.color, type = "p", pch = pch, 
                           cex = marker.cex*0.75)
                    if (plot.year) {
                        year.u <- c(sample.yr:(sample.yr - length(by) + 1))
                        text(bx, by, year.u, adj = c(1.5, 0.5), 
                             srt = 90, col = lab.color, cex = marker.cex)
                        no. <- 1:lenbx
                        text(bx, by, no., adj = c(0.5, -1.25), 
                             col = lab.color, cex = marker.cex)
                    }
                }
            }
        }
        
    }
    
    
    f.rw <- function(outfile, sample.yr, incline, py, dpi, h.dis) {
        
        df.loc <- outfile
        
        bx <- df.loc$x[-c(1:2)]
        where.bx <- df.loc$z[-c(1:2)]
        where.bx <- where.bx[order(bx)]
        bx <- sort(bx)
        
        dp <- dpi/25.4
        
        
        
        if (!incline) {
            lenbx <- length(bx)
            diff.col.num <- c(NA, diff(bx))
            rw <- round(diff.col.num/dp, 2)
            years <- c(sample.yr:(sample.yr - lenbx + 1))
            df.rw <- data.frame(year = years, column.numbers = bx, 
                                ring.width = rw)
        }
        
        if (incline) {
            up <- which(where.bx > 0)
            lenup <- length(up)
            if (lenup >= 1) {
                
                bx.up <- bx[up]
                diff.col.num.up <- c(NA, diff(bx.up))
                rw.up <- round(diff.col.num.up/dp, 2)
            }
            lower <- which(where.bx < 0)
            lenlo <- length(lower)
            if (lenlo >= 1) {
                bx.lower <- bx[lower]
                diff.col.num.lower <- c(NA, diff(bx.lower))
                rw.lower <- round(diff.col.num.lower/dp, 2)
            }
            
            years <- c(sample.yr:(sample.yr - lenup + 1))
            mean.bor <- (diff.col.num.lower[-1] + diff.col.num.up[-1])/2
            x.cor <- abs(bx.lower - bx.up)
            x.cor <- x.cor[-length(x.cor)]
            correct.rw <- mean.bor * cos(atan(x.cor/(dp * h.dis)))
            correct.rw <- c(NA, correct.rw)
            correct.rw <- round(correct.rw/dp, 2)
            df.rw <- data.frame(year = years, upper.cn = bx.up, upper.rw = rw.up, 
                                lower.cn = bx.lower, lower.rw = rw.lower, ring.width = correct.rw)
        }
        return(df.rw)
    }
    
    
    
    
    
    
    automatic.det <- function(img, incline, method, h.dis, dpi, m.line, RGB, x1, x2, y1, y2, arghed,
                              watershed.threshold, watershed.adjust, struc.ele1, struc.ele2, 
                              defaultcanny, canny.t1, canny.t2, canny.adjust, canny.smoothing,
                              origin) 
    {   
        
        dp <- dpi/25.4
        py <- round(m.line)
        if (incline) {
            h.dis. <- round((h.dis/2) * dp)
            py.upper <- py + h.dis.
            py.lower <- py - h.dis.
        }
        
        
        
        dim.img <- image_info(img) %>% 
            '['(1,2:3) %>% 
            as.numeric
        dimcol <- dim.img[1]
        dimrow <- dim.img[2]
        
        
        if (x1 <= 0) x1 <- 1
        if (y1 <= 0) y1 <- 0
        if (x2 >= dimcol) x2 <- dimcol
        if (y2 >= dimrow) y2 <- dimrow - 1
        
        geo <- paste0(as.character(x2-x1+1), 'x', as.character(y2-y1+1),'+',
                      as.character(x1-1),'+',as.character(dimrow-y2-1))
        img.crop <- image_crop(img, geo)
        
        
        rd.martix <- img.crop[[1]]
        
        
        hex2dec <- function(rd.martix){
            apply(rd.martix, 1, as.numeric)
        }
        
        
        
        
        
        rd.channel <- dim(rd.martix)[1]
        if (rd.channel == 1) {
            rd.m.array <- hex2dec(rd.martix[1, , ])
        } else {
            
            rd.m.array <- array(0, dim = rev(dim(rd.martix)))
            for (i in 1:rd.channel) {
                rd.m.array[,,i] <- hex2dec(rd.martix[i, , ])
            }
        }
        
        rd.m.array <- rd.m.array/255
        
        
        if (rd.channel == 1) {
            seg. <- rd.m.array[,]
        } 
        if (rd.channel == 2) {
            seg. <- rd.m.array[, , 1]
        } 
        
        
        if (rd.channel >= 3) {
            seg. <- apply(rd.m.array[, , 1:3], 1, function(x) x %*% RGB) %>% t
        }
        
        
        if (method == 'watershed') {
            
            seg.mor <- f.morphological(seg., struc.ele1, struc.ele2, dpi)
            
            black.hat <- hat(seg.mor, dpi, watershed.threshold, watershed.adjust)
            
            water.seg <- water.im(black.hat)
            
            water.seg. <- watershed.im(water.seg, seg.mor)
            seg. <- water.seg.
            
        }  
        
        
        if (method == 'canny') {
            seg.mor <- f.morphological(seg., struc.ele1, struc.ele2, dpi)
            if(defaultcanny){
                canny.seg <- cannyEdges(as.cimg(seg.mor), alpha = canny.adjust, 
                                        sigma = canny.smoothing)
            }
            if(!defaultcanny){
                canny.seg <- cannyEdges(as.cimg(seg.mor), t1=canny.t1, t2=canny.t2,
                                        alpha = canny.adjust, sigma = canny.smoothing)
            }
            
            early2late <- is.early2late(canny.seg, seg.)
            
            canny.mat <- matrix(0.2, ncol = ncol(seg.), nrow = nrow(seg.))
            canny.mat[early2late] <- 0.8
            seg. <- canny.mat
        } 
        
        
        if (method == 'lineardetect') {
            
            attributes(seg.)['image'] <- 'img'
            smoothed <- graySmoothed(seg., ppi = dpi, rgb = RGB)
            borders <- linearDetect(smoothed, origin = origin)
            borders <- borders + x1 - 1
            
            py.ld <- round((y1 + y2)/2)
            df. <- data.frame(x = borders, 
                              y = rep(py.ld, time = length(borders)), 
                              z = rep(0, time = length(borders)))
            df.loc <- rbind(arghed, df.)
            return(df.loc)
        }
        
        
        if (incline) {
            bor.u <- f.border(seg., py.upper - y1, 1, dp) + x1 - 2
            bor.l <- f.border(seg., py.lower - y1, 1, dp) + x1 - 2
            df.u <- data.frame(x = bor.u, 
                               y = rep(py.upper, time = length(bor.u)), 
                               z = rep(1, time = length(bor.u)))
            df.l <- data.frame(x = bor.l, 
                               y = rep(py.lower, time = length(bor.l)), 
                               z = rep(-1, time = length(bor.l)))
            df.loc <- rbind(arghed, df.u, df.l)
        } else {
            bor. <- f.border(seg., py - y1, 1, dp) + x1 - 2 
            df. <- data.frame(x = bor., 
                              y = rep(py, time = length(bor.)), 
                              z = rep(0, time = length(bor.)))
            df.loc <- rbind(arghed, df.)
        }
        return(df.loc)
    } 
    
    
    readImg <- function(img, img.name, open.magick = TRUE) {
        
        
        img.size <- file.size(img)/1024^2
        options(warn = -1)
        
        if(img.size <=10 | !open.magick){
            
            if (grepl("\\.tif", img)) {
                tree.data <- readTIFF(img, native = FALSE, info = TRUE)
            }
            if (grepl("\\.png", img)) {
                tree.data <- readPNG(img, native = FALSE, info = TRUE)
            }
            if (grepl("\\.jpg", img) | grepl("\\.jpeg", img)) {
                tree.data <- readJPEG(img, native = FALSE)
            }
            if (grepl("\\.bmp", img)) {
                tree.data <- read.bmp(img)
                tree.data <- tree.data/255
            }
            
            td.dim <- dim(tree.data)
            
            
            if(!is.matrix(tree.data)){
                if(any(td.dim[3]== c(2,4))){
                    tree.data <- tree.data[, , -td.dim[3]]
                    
                }
            }
            
            if(is.matrix(tree.data)){
                
                tdata <- as.raster(tree.data) %>%
                    image_read %>%
                    image_convert(colorspace = 'gray')
                
            } else {
                
                tdata <- image_read(tree.data)
                
            }
            
            rm(tree.data)
            g.tdata <- gc()
            
        } else {
            
            tdata <- image_read(img)
            
        }
        options(warn = 0)
        
        dim.tdata <- image_info(tdata) %>% 
            '['(1,2:3) %>% 
            as.numeric
        dimcol <- dim.tdata[1]
        dimrow <- dim.tdata[2]
        attributes(tdata) <- c(attributes(tdata), 
                               list(img.name = img.name, dimt = dim.tdata))
        return(tdata)
    }
    
    
    
    imgInput <- function(tdata, tdata.copy, plot1_rangesx, plot1_rangesy) {
        
        dimt <- attributes(tdata)$dimt
        img.name <- attributes(tdata)$img.name
        dimcol <- dimt[1]
        dimrow <- dimt[2]
        
        
        
        xleft <- 0
        ybottom <- 0
        xright <- dimcol
        ytop <- dimrow
        par(mar = c(2.5, 2, 2, 0))
        plot(x = c(xleft, xright), y = c(ybottom, ytop), 
             xlim = c(xleft, xright), ylim = c(ybottom, ytop), 
             main = img.name, xlab = "", ylab = "", 
             type = "n", axes = F, cex.main = 1.2)
        axis(1, col = "grey", cex.axis = 1)
        axis(2, col = "grey", cex.axis = 1)
        
        rasterImage(as.raster(tdata.copy), xleft, ybottom, 
                    xright, ytop, interpolate = FALSE)
        
        
        if(!is.null(plot1_rangesx)) {
            xmin <- plot1_rangesx[1]
            xmax <- plot1_rangesx[2]
            ymin <- plot1_rangesy[1]
            ymax <- plot1_rangesy[2]
            lines(c(xmin,xmin), c(ymin,ymax), lty = 2, lwd = 1.5)
            lines(c(xmin,xmax), c(ymax,ymax), lty = 2, lwd = 1.5)
            lines(c(xmax,xmax), c(ymin,ymax), lty = 2, lwd = 1.5)
            lines(c(xmin,xmax), c(ymin,ymin), lty = 2, lwd = 1.5)
        }
    }
    
    
    imgInput_crop <- function(tdata) {
        
        img.name <- attributes(tdata)$img.name
        dim.tdata <- dim(tdata)
        
        dimcol <- dim.tdata[2]
        dimrow <- dim.tdata[1]
        
        
        xleft <- 0
        ybottom <- 0
        xright <- dimcol
        ytop <- dimrow
        par(mar = c(2.5, 2, 2, 0))
        plot(x = c(xleft, xright), y = c(ybottom, ytop), 
             xlim = c(xleft, xright), ylim = c(ybottom, ytop), 
             main = img.name, xlab = "", ylab = "", 
             type = "n", axes = F, cex.main = 1.2)
        axis(1, col = "grey", cex.axis = 1)
        axis(2, col = "grey", cex.axis = 1)
        
        rasterImage(tdata, xleft, ybottom, 
                    xright, ytop, interpolate = FALSE)
        return(tdata)
        
    }
    
    
    
    
    rotateImg <- function(tdata, degree){
        tdata <- image_rotate(tdata, degree)
        
        dim.tdata <- image_info(tdata) %>% '['(1,2:3) %>% as.numeric
        
        
        attributes(tdata) <- c(attributes(tdata), list( dimt = dim.tdata))
        return(tdata)
    }
    
    options(shiny.maxRequestSize = 150*(1024^2))
    
    
    
    img.file <- reactiveValues(data = NULL)
    
    img.file.crop <- reactiveValues(data = NULL)
    
    img.file.copy <- reactiveValues(data = NULL)
    
    
    
    
    observeEvent(input$inmethod, {
        img.file$data <- NULL
        img.file.copy$data <- NULL
        img.file.crop$data <- NULL
        img.file.zoom$data <- NULL
        
        
        df.loc$data <- NULL
        df.loc$ID <- NULL
        plot1_ranges$x <- NULL
        plot1_ranges$y <- NULL
        plot2_ranges$x <- NULL
        plot2_ranges$y <- NULL
        rw.dataframe$data <- NULL
        
        updatePrettyRadioButtons(
            session = session, inputId = "cropcondition",
            choiceNames = 'Current State: Uncropped',
            choiceValues = list('a'),
            prettyOptions = list(shape = "round", status = "danger",
                                 fill = TRUE, inline = TRUE)
        )
        updateActionButton(session, "buttoncrop",
                           label = "Crop")
        updatePrettyRadioButtons(
            session = session, inputId = "rotatede",
            label = "Clockwise Rotation",
            choices = c("0 degrees" = "rotate0",
                        "90 degrees" = "rotate90",
                        "180 degrees" = "rotate180",
                        "270 degrees" = "rotate270"),
            prettyOptions = list(shape = "round", status = "success",
                                 fill = TRUE, inline = TRUE)
        )
        
    })
    
    observeEvent(input$buttonrotate, {
        
        
        if (!input$inmethod) {
            imgf <- input$select.file
            img <- as.character(imgf["datapath"])
        }
        if (input$inmethod) {
            img <- input$enter.path
        }
        
        img.check1 <- ifelse(length(img)>=1, T, F)
        img.check2 <- FALSE
        if(img.check1){
            img.check2 <- ifelse(nchar(img)>1, T, F)
        }
        if(any(!img.check1, !img.check2, is.null(img.file$data))){
            
            err.text <- paste('The preview image have not been generated')
            sendSweetAlert(
                session = session, title = "Error", text = err.text, type = "error"
            )
            return()
        }
        
        degree <- input$rotatede %>%
            substring(7) %>%
            as.numeric
        img.file$data <- rotateImg(img.file$data, degree)
        img.file.crop$data <- img.file$data
        
        img.file.copy$data <- rotateImg(img.file.copy$data, degree)
        
        img.file.zoom$data <- NULL
        
        
        
        
        
        attributes(img.file.copy$data)[["dimt"]] <- attributes(img.file$data)[["dimt"]]
        
        
        plot1_ranges$x <- NULL
        plot1_ranges$y <- NULL
        plot2_ranges$x <- NULL
        plot2_ranges$y <- NULL
        df.loc$data <- NULL
        df.loc$ID <- NULL
        rw.dataframe$data <- NULL
        updateTextInput(session, "m.line", value = '',
                        label = 'Y-coordinate of the path')
        updatePrettyRadioButtons(
            session = session, inputId = "cropcondition",
            choiceNames = 'Current State: Uncropped',
            choiceValues = list('a'),
            prettyOptions = list(shape = "round", status = "danger",
                                 fill = TRUE, inline = TRUE)
        )
        updateActionButton(session, "buttoncrop",
                           label = "Crop")
        
    })
    
    
    observeEvent(input$magick.switch, {
        if(input$magick.switch){
            updatePrettySwitch(session, inputId = 'magick.switch', 
                               label = 'Magick ON', value = TRUE)
        } else {
            updatePrettySwitch(session, inputId = 'magick.switch', 
                               label = 'Magick OFF', value = FALSE)
        }
        
    })
    
    
    
    
    observeEvent(input$buttoninputimage, {
        
        
        magick.switch <- input$magick.switch
        
        if (!input$inmethod) {
            imgf <- input$select.file
            
            
            if(is.null(imgf)){
                
                err.text <- paste('The image file has not been uploaded')
                sendSweetAlert(
                    session = session, title = "Error", text = err.text, type = "error"
                )
                return()
            }
            
            img <- as.character(imgf["datapath"])
            img.name <- as.character(imgf["name"])
        }
        if (input$inmethod) {
            img <- input$enter.path
            
            
            if(img == ''){
                
                err.text <- paste('The file path has not been entered')
                sendSweetAlert(
                    session = session, title = "Error", text = err.text, type = "error"
                )
                return()
            }
            img.name <- basename(img)
        }
        
        
        updatePrettyRadioButtons(
            session = session, inputId = "rotatede",
            label = "Clockwise Rotation",
            choices = c("0 degrees" = "rotate0",
                        "90 degrees" = "rotate90",
                        "180 degrees" = "rotate180",
                        "270 degrees" = "rotate270"),
            prettyOptions = list(shape = "round", status = "success",
                                 fill = TRUE, inline = TRUE)
        )
        
        
        plot1_ranges$x <- NULL
        plot1_ranges$y <- NULL
        plot2_ranges$x <- NULL
        plot2_ranges$y <- NULL
        df.loc$data <- NULL
        df.loc$ID <- NULL
        rw.dataframe$data <- NULL
        
        
        updateTextInput(session, "m.line", value = '',
                        label = 'Y-coordinate of the path')
        
        updateTextInput(session, "tuid", value = '',
                        label = 'Series ID')
        
        
        
        updatePrettyRadioButtons(
            session = session, inputId = "cropcondition",
            choiceNames = 'Current State: Uncropped',
            choiceValues = list('a'),
            prettyOptions = list(shape = "round", status = "danger",
                                 fill = TRUE, inline = TRUE)
        )
        updateActionButton(session, "buttoncrop", label = "Crop")
        
        
        
        
        img.file$data <- readImg(img, img.name, magick.switch)
        
        img.file.crop$data <- img.file$data
        img.file.zoom$data <- NULL
        
        
        
        dim.tdata <- attributes(img.file$data)[["dimt"]]
        dimcol <- dim.tdata[1]
        dimrow <- dim.tdata[2]
        
        
        
        if ((dimcol*dimrow) >= 1.2e+07){
            resize.ratio <- 0.25
            resize.str <- paste0(round(dimcol*resize.ratio), 'x', round(dimrow*resize.ratio))
            
            img.file.copy$data <- image_resize(img.file$data, resize.str)
        } else {
            img.file.copy$data <- img.file$data
        }
    })
    
    
    
    plot1_ranges <- reactiveValues(x = NULL, y = NULL)
    
    
    observeEvent(input$buttoncrop, {
        
        
        if(is.null(img.file$data)){
            
            err.text <- paste('The preview image have not been generated')
            sendSweetAlert(
                session = session, title = "Error", text = err.text, type = "error"
            )
            return()
        }
        
        plot1_brush <- input$plot1_brush
        
        
        plot1_ranges$x <- NULL
        plot1_ranges$y <- NULL
        plot2_ranges$x <- NULL
        plot2_ranges$y <- NULL
        df.loc$data <- NULL
        df.loc$ID <- NULL
        rw.dataframe$data <- NULL
        img.file.zoom$data <- NULL
        
        updateTextInput(session, "m.line", value = '',
                        label = 'Y-coordinate of the path')
        
        updateTextInput(session, "tuid", value = '',
                        label = 'Series ID')
        
        if (!is.null(plot1_brush)) {
            plot1_ranges$x <- c(round(plot1_brush$xmin), round(plot1_brush$xmax))
            plot1_ranges$y <- c(round(plot1_brush$ymin), round(plot1_brush$ymax))
            
            dimcol <- attributes(img.file$data)[["dimt"]][1]
            dimrow <- attributes(img.file$data)[["dimt"]][2]
            if (plot1_ranges$x[1] <= 0) plot1_ranges$x[1] <- 0
            if (plot1_ranges$y[1] <= 0) plot1_ranges$y[1] <- 0
            if (plot1_ranges$x[2] >= dimcol) plot1_ranges$x[2] <- dimcol
            if (plot1_ranges$y[2] >= dimrow) plot1_ranges$y[2] <- dimrow
            
            xmin <- plot1_ranges$x[1]
            ymin <- plot1_ranges$y[1]
            xmax <- plot1_ranges$x[2]
            ymax <- plot1_ranges$y[2]
            
            geo <- paste0(as.character(xmax-xmin), 'x', as.character(ymax-ymin),'+',
                          as.character(xmin), '+',as.character(dimrow-ymax))
            img.file.crop$data <- image_crop(img.file$data, geo)
            
            updateActionButton(session, "buttoncrop",
                               label = "Cancel")
            updatePrettyRadioButtons(
                session = session, inputId = "cropcondition",
                choiceNames = 'Current State: Cropped',
                choiceValues = list('a'),
                prettyOptions = list(shape = "round", status = "success",
                                     fill = TRUE, inline = TRUE)
            )
        } else {
            
            img.file.crop$data <- img.file$data
            
            updateActionButton(session, "buttoncrop",
                               label = "Crop")
            updatePrettyRadioButtons(
                session = session, inputId = "cropcondition",
                choiceNames = 'Current State: Uncropped',
                choiceValues = list('a'),
                prettyOptions = list(shape = "round", status = "danger",
                                     fill = TRUE, inline = TRUE)
            )
            
        }
        
        
        attributes(img.file$data) <- c(attributes(img.file$data), 
                                       list(plot1_rangesx = plot1_ranges$x,
                                            plot1_rangesy = plot1_ranges$y)
        )
        
    })
    
    
    output$pre.img <- renderPlot({
        
        if (is.null(img.file$data)) return()
        imgInput(img.file$data, img.file.copy$data, 
                 plot1_ranges$x, plot1_ranges$y)
    })
    
    plot2_ranges <- reactiveValues(x = NULL, y = NULL)
    
    
    img.file.zoom <- reactiveValues(data = NULL)
    
    
    zoom.add <- reactiveValues(data = NULL)
    
    
    
    
    df.loc <- reactiveValues(data = NULL, ID = NULL)
    
    
    observeEvent(input$buttoncreatpath, {
        
        
        
        
        if(is.null(img.file.crop$data)){
            err.text <- 'Path creation fail because a tree-ring image has not been plotted'
            sendSweetAlert(
                session = session, title = "Error", text = err.text, type = "error"
            )
            return()
        }
        
        
        
        
        py <- as.numeric(input$m.line)
        
        if(is.na(py)){
            err.text <- 'Path creation fail because the y-coordinate of the path has not been entered'
            sendSweetAlert(
                session = session, title = "Error", text = err.text, type = "error"
            )
            return()
        }
        
        dpi <- as.numeric(input$dpi)
        
        if(is.na(py)){
            err.text <- paste('Path creation fail because the DPI of the image has not been entered')
            sendSweetAlert(
                session = session, title = "Error", text = err.text, type = "error"
            )
            return()
        }
        
        seriesID <- input$tuid
        
        if(seriesID == ''){
            err.text <- paste('Path creation fail because the series ID has not been entered')
            sendSweetAlert(
                session = session, title = "Error", text = err.text, type = "error"
            )
            return()
        }
        
        incline <- input$incline
        
        
        if(incline){
            h.dis <- as.numeric(input$h.dis)
            incline.cond <- 1
        } else {
            
            h.dis <- 0
            incline.cond <- 0
        }
        
        dim.tdata <- image_info(img.file.crop$data) %>% 
            '['(1,2:3) %>% 
            as.numeric
        dimrow <- dim.tdata[2]
        
        
        if(py >= dimrow){
            err.text <- paste('The Y-coordinate of the path is out of range.',
                              'Please type an appropriate Y-coordinate value')
            sendSweetAlert(
                session = session, title = "Error", text = err.text, type = "error"
            )
            return()
            
        }
        
        
        f.df.loc <- c(dpi, incline.cond, 0, py, h.dis, 0) %>%
            matrix(byrow = T, nrow = 2) %>%
            as.data.frame(stringsAsFactors = F)
        colnames(f.df.loc) <- c('x','y','z')
        df.loc$data <- f.df.loc
        df.loc$ID <- seriesID
        
    })
    
    observeEvent(input$buttoncreatpath2, {
        
        
        
        if(is.null(img.file.crop$data)){
            err.text <- 'Path creation fail because a tree-ring image has not been plotted'
            sendSweetAlert(
                session = session, title = "Error", text = err.text, type = "error"
            )
            return()
        }
        
        
        
        
        py <- as.numeric(input$m.line)
        
        if(is.na(py)){
            err.text <- 'Path creation fail because the y-coordinate of the path has not been entered'
            sendSweetAlert(
                session = session, title = "Error", text = err.text, type = "error"
            )
            return()
        }
        
        dpi <- as.numeric(input$dpi)
        
        if(is.na(py)){
            err.text <- 'Path creation fail because the DPI of the image has not been entered'
            sendSweetAlert(
                session = session, title = "Error", text = err.text, type = "error"
            )
            return()
        }
        
        seriesID <- input$tuid
        
        if(seriesID == ''){
            err.text <- 'Path creation fail because the series ID has not been entered'
            sendSweetAlert(
                session = session, title = "Error", text = err.text, type = "error"
            )
            return()
        }
        
        incline <- input$incline
        
        
        if(incline){
            h.dis <- as.numeric(input$h.dis)
            incline.cond <- 1
        } else {
            
            h.dis <- 0
            incline.cond <- 0
        }
        
        dim.tdata <- image_info(img.file.crop$data) %>% 
            '['(1,2:3) %>% 
            as.numeric
        dimrow <- dim.tdata[2]
        
        
        if(py >= dimrow){
            err.text <- paste('The Y-coordinate of the path is out of range.',
                              'Please type an appropriate Y-coordinate value')
            sendSweetAlert(
                session = session, title = "Error", text = err.text, type = "error"
            )
            return()
            
        }
        
        
        f.df.loc <- c(dpi, incline.cond, 0, py, h.dis, 0) %>%
            matrix(byrow = T, nrow = 2) %>%
            as.data.frame(stringsAsFactors = F)
        colnames(f.df.loc) <- c('x','y','z')
        df.loc$data <- f.df.loc
        df.loc$ID <- seriesID
    })
    
    
    
    observeEvent(input$buttonsubimg, {
        plot2_brush <- input$plot2_brush
        
        if(is.null(img.file.crop$data)){
            err.text <- 'Path creation fail because a tree-ring image has not been plotted'
            sendSweetAlert(
                session = session, title = "Error", text = err.text, type = "error"
            )
            return()
        }
        if (!is.null(plot2_brush$xmin)) {
            
            plot2_ranges$x <- c(round(plot2_brush$xmin), round(plot2_brush$xmax))
            plot2_ranges$y <- c(round(plot2_brush$ymin), round(plot2_brush$ymax))
            
            
            dim.tdata <- image_info(img.file.crop$data) %>% 
                '['(1,2:3) %>% 
                as.numeric
            dimcol <- dim.tdata[1]
            dimrow <- dim.tdata[2]
            if (plot2_ranges$x[1] <= 0) plot2_ranges$x[1] <- 0
            if (plot2_ranges$y[1] <= 0) plot2_ranges$y[1] <- 0
            if (plot2_ranges$x[2] >= dimcol) plot2_ranges$x[2] <- dimcol
            if (plot2_ranges$y[2] >= dimrow) plot2_ranges$y[2] <- dimrow
            
            
            xmin <- plot2_ranges$x[1]
            ymin <- plot2_ranges$y[1]
            xmax <- plot2_ranges$x[2]
            ymax <- plot2_ranges$y[2]
            
            geo <- paste0(as.character(xmax-xmin), 'x', as.character(ymax-ymin),'+',
                          as.character(xmin),'+',as.character(dimrow-ymax))
            img.file.zoom$data <- image_crop(img.file.crop$data, geo)
        } else {
            plot2_ranges$x <- NULL
            plot2_ranges$y <- NULL
            
            img.file.zoom$data <- NULL
            err.text <- 'You have not selected an area by brushing'
            sendSweetAlert(
                session = session, title = "Error", text = err.text, type = "error"
            )
            return()
        }
        
    })
    
    
    
    observeEvent(input$plot2_dblclick, {
        plot2_brush <- input$plot2_brush
        
        
        if(is.null(img.file.crop$data)){
            err.text <- 'Path creation fail because a tree-ring image has not been plotted'
            sendSweetAlert(
                session = session, title = "Error", text = err.text, type = "error"
            )
            return()
        }
        
        if (!is.null(plot2_brush$xmin)) {
            
            plot2_ranges$x <- c(round(plot2_brush$xmin), round(plot2_brush$xmax))
            plot2_ranges$y <- c(round(plot2_brush$ymin), round(plot2_brush$ymax))
            
            
            dim.tdata <- image_info(img.file.crop$data) %>% 
                '['(1,2:3) %>% 
                as.numeric
            dimcol <- dim.tdata[1]
            dimrow <- dim.tdata[2]
            if (plot2_ranges$x[1] <= 0) plot2_ranges$x[1] <- 0
            if (plot2_ranges$y[1] <= 0) plot2_ranges$y[1] <- 0
            if (plot2_ranges$x[2] >= dimcol) plot2_ranges$x[2] <- dimcol
            if (plot2_ranges$y[2] >= dimrow) plot2_ranges$y[2] <- dimrow
            
            
            xmin <- plot2_ranges$x[1]
            ymin <- plot2_ranges$y[1]
            xmax <- plot2_ranges$x[2]
            ymax <- plot2_ranges$y[2]
            
            geo <- paste0(as.character(xmax-xmin), 'x', as.character(ymax-ymin),'+',
                          as.character(xmin),'+',as.character(dimrow-ymax))
            img.file.zoom$data <- image_crop(img.file.crop$data, geo)
        } else {
            plot2_ranges$x <- NULL
            plot2_ranges$y <- NULL
            
            img.file.zoom$data <- NULL
            err.text <- 'The tree-ring image was not brushed'
            sendSweetAlert(
                session = session, title = "Error", text = err.text, type = "error"
            )
            return()
        }
        
    })
    
    
    
    observeEvent(input$zoom_dblclick, {
        
        if(is.null(df.loc$data)){
            err.text <- 'A path has not been created'
            sendSweetAlert(
                session = session, title = "Error", text = err.text, type = "error"
            )
            return()
        } 
        
        
        if(is.null(img.file.zoom$data)){
            err.text <- 'An enlarged image has not been created'
            sendSweetAlert(
                session = session, title = "Error", text = err.text, type = "error"
            )
            return()
        } 
        
        
        
        f.df.loc <- df.loc$data
        plot.arg <- f.df.loc[1:2,]
        
        
        incline <- ifelse(plot.arg[1,2]==0, FALSE, TRUE)
        
        py <- plot.arg[2,1] - plot2_ranges$y[1]
        
        bor <- input$zoom_dblclick
        
        bor.x <- bor$x + plot2_ranges$x[1]
        bor.y <- bor$y
        
        
        if (!incline) 
            f.df.loc <- rbind(f.df.loc, list(bor.x, bor.y, 0))
        
        if (incline) {
            if (bor.y == py){
                err.text <- paste('The piexl you clicked should be precisely',
                                  'located at the upper path or the lower path')
                sendSweetAlert(
                    session = session, title = "Error", text = err.text, type = "error"
                )
                return()
            }
            if (bor.y > py) 
                f.df.loc <- rbind(f.df.loc, list(bor.x, bor.y, 1))
            if (bor.y < py) 
                f.df.loc <- rbind(f.df.loc, list(bor.x, bor.y, -1))
        }
        
        
        df.loc$data <- f.df.loc
        
    })
    
    
    
    plot3_ranges <- reactiveValues(data = NULL)
    
    observeEvent(input$buttonzoomdel, {
        
        
        if(is.null(input$zoom_brush$xmin)){
            err.text <- 'You have not selected an area on the enlarged image'
            sendSweetAlert(
                session = session, title = "Error", text = err.text, type = "error"
            )
            return()
        } 
        
        
        if(is.null(df.loc$data)){
            err.text <- 'A path has not been created'
            sendSweetAlert(
                session = session, title = "Error", text = err.text, type = "error"
            )
            return()
        } 
        
        
        if(nrow(df.loc$data) < 3){
            
            
            remove.text <- paste('Ring border was NOT found along the path. Please confirm',
                                 'that boundaries have already been added to the image')
            sendSweetAlert(
                session = session, title = "Error", text = remove.text, type = "error"
            )
            return()
        } 
        
        xmin <- input$zoom_brush$xmin
        xmax <- input$zoom_brush$xmax
        ymin <- input$zoom_brush$ymin
        ymax <- input$zoom_brush$ymax
        
        
        plot.arg <- df.loc$data[1:2,]
        
        
        dpi <- plot.arg[1,1]
        dp <- dpi/25.4
        incline <- ifelse(plot.arg[1,2]==0, FALSE, TRUE)
        py <- plot.arg[2,1]
        h.dis <- plot.arg[2,2]
        
        x.ranges <- df.loc$data$x[-c(1:2)] - plot2_ranges$x[1]
        
        delete.bor <- which(x.ranges >= xmin & x.ranges <= xmax)
        
        
        if(length(delete.bor) == 0){
            
            err.text <- 'Ring border was NOT found in the rectangular area you selected'
            sendSweetAlert(
                session = session, title = "Error", text = err.text, type = "error"
            )
            return()
        }
        
        
        if(incline){
            h.dis. <- round((h.dis/2) * dp)
            py.upper <- py + h.dis.
            py.lower <- py - h.dis.
            
            which.line <- df.loc$data$z[-c(1:2)][delete.bor]
            y.value <- ifelse(which.line > 0, 
                              py.upper - plot2_ranges$y[1],
                              py.lower - plot2_ranges$y[1])
            is.contain <- c(ymin <= y.value) & c(ymax >= y.value)
            
            
            if(any(is.contain)){
                delete.bor <- delete.bor[is.contain]
                delete.bor <- delete.bor+2
                df.loc$data <- df.loc$data[-delete.bor,]
            } else {
                
                err.text <- 'Ring border was NOT found in the rectangular area you selected'
                sendSweetAlert(
                    session = session, title = "Error", text = err.text, type = "error"
                )
            }
            
        } else {
            
            y.value <- py - plot2_ranges$y[1]
            is.contain <- c(ymin <= y.value) & c(ymax >= y.value)
            
            
            if(any(is.contain)){
                delete.bor <- delete.bor[is.contain]
                delete.bor <- delete.bor+2
                df.loc$data <- df.loc$data[-delete.bor,]
            } else {
                
                err.text <- 'Ring border was NOT found in the rectangular area you selected'
                sendSweetAlert(
                    session = session, title = "Error", text = err.text, type = "error"
                )
            }
            
        }
    })
    
    
    
    
    
    img.file.crop.copy <- reactiveValues(data = NULL)
    
    observe({
        if(!is.null(img.file.crop$data)){
            img.file.crop.copy$data <- as.raster(img.file.crop$data)
            
        }
    })
    
    
    img.file.zoom.copy <- reactiveValues(data = NULL)
    
    observeEvent(input$plot2_dblclick,{
        if(!is.null(img.file.zoom$data)){
            img.file.zoom.copy$data <- as.raster(img.file.zoom$data)
            
        }
    })
    
    observeEvent(input$buttonsubimg,{
        if(!is.null(img.file.zoom$data)){
            img.file.zoom.copy$data <- as.raster(img.file.zoom$data)
            
        }
    })
    
    
    
    output$pre.img2 <- renderPlot({
        
        if (is.null(img.file$data)) return()
        
        
        imgInput_crop(img.file.crop.copy$data)
        
        
        
        sample.yr <- as.numeric(input$sample.yr)
        
        if(is.na(sample.yr)) return()
        
        pch <- as.numeric(input$pch)
        bor.color <- input$border.color
        lab.color <- input$label.color
        l.w <- as.numeric(input$linelwd)
        marker.cex <- as.numeric(input$marker.cex)*0.7
        
        
        if(is.null(df.loc$data)) return()
        
        if(is.null(df.loc$ID)) return()
        f.df.loc <- df.loc$data
        
        
        plot.arg <- f.df.loc[1:2,]
        
        
        dpi <- plot.arg[1,1]
        dp <- dpi/25.4
        incline <- ifelse(plot.arg[1,2]==0, FALSE, TRUE)
        py <- plot.arg[2,1]
        h.dis <- plot.arg[2,2]
        img.name <- paste('Series ID:', df.loc$ID)
        
        
        plot.marker(py, incline, dp, sample.yr, h.dis, l.w, bor.color, 
                    lab.color, pch, marker.cex, f.df.loc, T, img.name)
        
        
    })
    
    
    output$zoom.img <- renderPlot({
        
        
        if (is.null(plot2_ranges$x)) return()
        
        if (is.null(img.file.zoom$data)) return()
        if (is.null(img.file.zoom.copy$data)) return()
        
        imgInput_crop(img.file.zoom.copy$data)
        
        
        
        sample.yr <- as.numeric(input$sample.yr)
        if(is.na(sample.yr)) return()
        
        pch <- as.numeric(input$pch)
        bor.color <- input$border.color
        lab.color <- input$label.color
        l.w <- as.numeric(input$linelwd)
        marker.cex <- as.numeric(input$marker.cex)
        
        
        if(is.null(df.loc$data)) return()
        
        if(is.null(df.loc$ID)) return()
        f.df.loc <- df.loc$data
        
        
        
        f.df.loc$x[-c(1,2)] <- f.df.loc$x[-c(1,2)] - plot2_ranges$x[1]
        plot.arg <- f.df.loc[1:2,]
        
        
        dpi <- plot.arg[1,1]
        dp <- dpi/25.4
        incline <- ifelse(plot.arg[1,2]==0, FALSE, TRUE)
        
        py <- plot.arg[2,1] - plot2_ranges$y[1]
        h.dis <- plot.arg[2,2]
        img.name <- paste('Series ID:', df.loc$ID)
        
        
        
        plot.marker(py, incline, dp, sample.yr, h.dis, l.w, bor.color, 
                    lab.color, pch, marker.cex, f.df.loc, T, img.name)
        
    })
    
    
    autoresult <- reactiveValues(data = NULL, text = NULL)
    
    
    
    
    observeEvent(input$button_run_auto, {
        
        
        if(is.null(input$plot2_brush)){
            brush.text <- paste('Please create a rectangular area on the image', 
                                'before running the automatic measurement')
            sendSweetAlert(
                session = session, title = "Error", text = brush.text, type = "error"
            )
            return()
        }
        
        
        
        if(is.null(df.loc$data)){
            f.df.loc.text <- 'A path has not been created'
            sendSweetAlert(
                session = session, title = "Error", text = f.df.loc.text, type = "error"
            )
            return()
        }
        f.df.loc <- df.loc$data
        
        
        
        
        
        brush <- input$plot2_brush
        x1 <- brush$xmin
        x2 <- brush$xmax
        y1 <- brush$ymin
        y2 <- brush$ymax
        
        isrgb <- input$isrgb
        if (isrgb) 
            RGB = c(0.299, 0.587, 0.114)
        if (!isrgb) {
            RGB <- input$customRGB
            RGB <- strsplit(RGB, ",")
            RGB <- RGB[[1]] %>% as.numeric
        }
        
        plot.arg <- f.df.loc[1:2,]
        dpi <- plot.arg[1,1]
        dp <- dpi/25.4
        incline <- ifelse(plot.arg[1,2]==0, FALSE, TRUE)
        m.line <- plot.arg[2,1]
        h.dis <- plot.arg[2,2]
        
        m.line1 <- m.line
        m.line2 <- m.line
        if (incline) {
            h.dis. <- round((h.dis/2) * dp)
            m.line2 <- m.line + h.dis.
            m.line1 <- m.line - h.dis.
        }
        
        
        
        img <- img.file.crop$data
        
        
        
        method <- input$method
        
        
        linear.waring <- FALSE
        
        
        if(m.line1 > y1 & m.line2 < y2){
            
            
            
            
            
            defaultse <- input$defaultse
            
            if (defaultse) {
                struc.ele1 <- NULL
                struc.ele2 <- NULL
            } else {
                struc.ele1 <- input$struc.ele1
                struc.ele1 <- strsplit(struc.ele1, ',')[[1]] %>% as.numeric
                
                if(length(struc.ele1) >= 3){
                    
                    err.text <- paste('The rectangular structuring element allows no more than two',
                                      'non-negative integers. If entering two integers, the first',
                                      'integer is the width of the structuring element and the second',
                                      'is height. Use a comma to separate them. For example: 15,10')
                    sendSweetAlert(
                        session = session, title = "Error", text = err.text, type = "error"
                    )
                    return()
                }
                
                if(length(struc.ele1) == 0){
                    
                    err.text <- 'The size of the first structuring element has not been entered'
                    sendSweetAlert(
                        session = session, title = "Error", text = err.text, type = "error"
                    )
                    return()
                }
                
                if(as.numeric(struc.ele1) %>% is.na %>% any) {
                    
                    err.text <- 'The structuring element should be non-negative integers'
                    sendSweetAlert(
                        session = session, title = "Error", text = err.text, type = "error"
                    )
                    return()
                }
                if(length(struc.ele1) == 1)
                    struc.ele1 <- as.numeric(c(struc.ele1, struc.ele1))
                if(length(struc.ele1) == 2)
                    struc.ele1 <- as.numeric(c(struc.ele1[1], struc.ele1[2]))
                
                
                struc.ele2 <- input$struc.ele2
                struc.ele2 <- strsplit(struc.ele2, ',')[[1]] %>% as.numeric
                
                if(length(struc.ele2) >= 3){
                    
                    err.text <- paste('The rectangular structuring element allows no more than two',
                                      'non-negative integers. If entering two integers, the first',
                                      'integer is the width of the structuring element and the second',
                                      'is height. Use a comma to separate them. For example: 15,10')
                    sendSweetAlert(
                        session = session, title = "Error", text = err.text, type = "error"
                    )
                    return()
                }
                
                if(length(struc.ele2) == 0){
                    
                    err.text <- 'The size of the second structuring element has not been entered'
                    sendSweetAlert(
                        session = session, title = "Error", text = err.text, type = "error"
                    )
                    return()
                }
                
                if(as.numeric(struc.ele2) %>% is.na %>% any) {
                    
                    err.text <- 'The structuring element should be non-negative integers'
                    sendSweetAlert(
                        session = session, title = "Error", text = err.text, type = "error"
                    )
                    return()
                }
                if(length(struc.ele2) == 1)
                    struc.ele2 <- as.numeric(c(struc.ele2, struc.ele2))
                if(length(struc.ele2) == 2)
                    struc.ele2 <- as.numeric(c(struc.ele2[1], struc.ele2[2]))
            }
            
            if (method == 'watershed') {
                
                
                if(input$watershed.threshold == 'custom.waterthr'){
                    watershed.threshold <- input$watershed.threshold2
                } else {
                    watershed.threshold <- input$watershed.threshold
                }
                
                watershed.adjust <- input$watershed.adjust
                
                df.loc$data <- automatic.det(img, incline, method, h.dis, dpi, m.line, 
                                             RGB, x1, x2, y1, y2, plot.arg,watershed.threshold,
                                             watershed.adjust, struc.ele1, struc.ele2)
            }
            
            if (method == "canny") {
                
                defaultcanny <- input$defaultcanny
                canny.t1 <- as.numeric(input$canny.t1)
                canny.t2 <- as.numeric(input$canny.t2)
                canny.adjust <- input$canny.adjust
                canny.smoothing <- input$canny.smoothing
                df.loc$data <- automatic.det(img, incline, method, h.dis, dpi, m.line, RGB, 
                                             x1, x2, y1, y2, plot.arg, watershed.threshold, 
                                             watershed.adjust, struc.ele1, struc.ele2, defaultcanny,
                                             canny.t1, canny.t2, canny.adjust, canny.smoothing)
            }   
            
            if (method == "lineardetect") {
                origin <- as.numeric(input$origin)
                py.ld <- round((y1 + y2)/2)
                
                updateTextInput(session, "m.line", value = as.character(round(py.ld)))
                f.df.loc <- automatic.det(img, incline, method, h.dis, dpi, m.line, 
                                          RGB, x1, x2, y1, y2, plot.arg, origin = origin)
                
                
                if(incline){
                    
                    updateCheckboxInput(session, 'incline', 'Inclined tree rings', F)
                    f.df.loc[1,2] <- FALSE
                    linear.waring <- TRUE
                }
                f.df.loc[2,1] <- py.ld 
                df.loc$data <- f.df.loc
                
            }
            number.border <- nrow(df.loc$data) - 2
            if(number.border == 0){
                result.text <- 'Ring border was not detected'
                sendSweetAlert(
                    session = session, title = "Error", text = result.text, type = "error"
                )
            } else {
                result.text <- paste(number.border, 'boreders were detected')
                sendSweetAlert(
                    session = session, title = "Finished", text = result.text, type = "success"
                )
            }
        } else {
            
            result.text <- paste('The brushed area does not contain the',
                                 'path. Please re-brush on the image')
            sendSweetAlert(
                session = session, title = "Error", text = result.text, type = "error"
            )
        }
        
        
        if(linear.waring){
            result.text <- paste('Linear detection does not support \'incline = TRUE\'.',
                                 'This option has been automatically corrected in order',
                                 'to continue executing the automatic detection')
            sendSweetAlert(
                session = session, title = "TIPS", text = result.text, type = "warning"
            )
        }
        
    })
    
    
    
    observeEvent(input$buttonrcm, {
        plot1_ranges$x <- NULL
        plot1_ranges$y <- NULL
        plot2_ranges$x <- NULL
        plot2_ranges$y <- NULL
        df.loc$data <- NULL
        df.loc$ID <- NULL
        
        
        updateTextInput(session, "m.line", value = '',
                        label = 'Y-coordinate of the path')
        remove.text <- paste('The current path and borders have been',
                             'removed. The user should re-create a path')
        sendSweetAlert(
            session = session, title = "Success", text = remove.text, type = "success"
        )
    })
    observeEvent(input$buttonrcm2, {
        plot1_ranges$x <- NULL
        plot1_ranges$y <- NULL
        plot2_ranges$x <- NULL
        plot2_ranges$y <- NULL
        df.loc$data <- NULL
        df.loc$ID <- NULL
        
        
        updateTextInput(session, "m.line", value = '',
                        label = 'Y-coordinate of the path')
        remove.text <- paste('The current path and borders have been',
                             'removed. The user should re-create a path')
        sendSweetAlert(
            session = session, title = "Remove", text = remove.text, type = "success"
        )
    })
    
    
    
    
    observeEvent(input$button_del, {
        
        
        if(is.null(df.loc$data)){
            
            remove.text <- paste('A path has not been created. You can',
                                 'not execute the delete operation')
            sendSweetAlert(
                session = session, title = "Error", text = remove.text, type = "error"
            )
            return()
        }
        
        f.df.loc <- df.loc$data
        
        plot.arg <- f.df.loc[1:2,]
        dpi <- plot.arg[1,1]
        dp <- dpi/25.4
        incline <- ifelse(plot.arg[1,2]==0, FALSE, TRUE)
        m.line <- plot.arg[2,1]
        h.dis <- plot.arg[2,2]
        
        
        if (nrow(f.df.loc) < 3){
            
            
            remove.text <- paste('No border was found along the path. Please confirm',
                                 'that borders have already been added to the image')
            sendSweetAlert(
                session = session, title = "Error", text = remove.text, type = "error"
            )
            return()
        }
        
        
        bx <- f.df.loc$x[-c(1,2)]
        where.bx <- f.df.loc$z[-c(1,2)]
        where.bx <- where.bx[order(bx)]
        bx <- sort(bx)
        
        
        
        
        if (incline) {
            if (input$del.u == '' & input$del.l == '') {
                
                remove.text <- paste('Please enter at least one border number')
                sendSweetAlert(
                    session = session, title = "Error", text = remove.text, type = "error"
                )
                return()
            }
            del.u <- input$del.u
            del.l <- input$del.l
            if(del.u != '')
                del.u <- as.numeric(strsplit(del.u, ",")[[1]])
            if(del.l != '')
                del.l <- as.numeric(strsplit(del.l, ",")[[1]])
            ndfu <- length(del.u)
            ndfl <- length(del.l)
            ndf <- length(del.u) + length(del.l)
            
            
            
            up <- which(where.bx > 0)
            lenup <- length(up)
            bx.u <- bx[up]
            
            if (lenup >= 1 & input$del.u != '') {
                
                if(max(del.u) <= length(bx.u)){
                    bx.u <- bx.u[-del.u]
                } else {
                    
                    remove.text <- 'The border number you entered did not exist'
                    sendSweetAlert(
                        session = session, title = "Error", text = remove.text, type = "error"
                    )
                    return()
                }
            }
            
            lower <- which(where.bx < 0)
            lenlo <- length(lower)
            bx.l <- bx[lower]
            if (lenlo >= 1 & input$del.l != '') {
                
                if(max(del.l) <= length(bx.l)){
                    bx.l <- bx.l[-del.l]
                } else {
                    
                    remove.text <- 'The border number you entered did not exist'
                    sendSweetAlert(
                        session = session, title = "Error", text = remove.text, type = "error"
                    )
                    return()
                }
            }
            
            
            df.u <- data.frame(x = bx.u, 
                               y = rep(m.line, time = length(bx.u)), 
                               z = rep(1, time = length(bx.u))) 
            df.l <- data.frame(x = bx.l, 
                               y = rep(m.line, time = length(bx.l)), 
                               z = rep(-1, time = length(bx.l)))
            
            
            df.loc$data <- rbind(plot.arg, df.u, df.l)
            
            updateTextInput(session, "del.u",
                            label = 'Border number in the upper portion',
                            value = '')
            updateTextInput(session, "del.l",
                            label = 'Border number in the lower portion',
                            value = '')
            
            
        } else { 
            
            if (input$del == '') {
                
                remove.text <- paste('Please enter at least one border number')
                sendSweetAlert(
                    session = session, title = "Error", text = remove.text, type = "error"
                )
                return()
            }
            
            del <- input$del
            del <- as.numeric(strsplit(del, ",")[[1]])
            
            
            if(max(del) <= length(bx)){
                bx <- bx[-del]
                df. <- data.frame(x = bx, 
                                  y = rep(m.line, time = length(bx)), 
                                  z = rep(0, time = length(bx)))
                
                
                df.loc$data <- rbind(plot.arg, df.)
                
                updateTextInput(session, "del",
                                label = 'Border number',
                                value = '')
                
            } else {
                
                remove.text <- 'The border number you entered did not exist'
                sendSweetAlert(
                    session = session, title = "Error", text = remove.text, type = "error"
                )
                return()
            }
        }
    }) 
    
    
    rw.dataframe <- reactiveValues(data = NULL)
    
    observeEvent(input$button_results, {
        
        if(is.null(df.loc$data)){
            error.text <- paste('No border was found along the path. Please confirm',
                                'that borders have already been added to the image')
            sendSweetAlert(
                session = session, title = "Error", text = error.text, type = "error"
            )
            return()
            
        } 
        
        
        if(nrow(df.loc$data) <= 3){
            
            error.text <- paste('A minimum of two ring borders on each path ',
                                'was required to generate a ring-width series')
            sendSweetAlert(
                session = session, title = "Error", text = error.text, type = "error"
            )
            return()
        } 
        
        sample.yr <- as.numeric(input$sample.yr)
        
        if(is.na(sample.yr)){
            error.text <- paste('Please verify the argument \'Sampling year\' ')
            sendSweetAlert(
                session = session, title = "Error", text = error.text, type = "error"
            )
            return()
        }
        
        
        plot.arg <- df.loc$data[1:2,]
        dpi <- plot.arg[1,1]
        dp <- dpi/25.4
        incline <- ifelse(plot.arg[1,2]==0, FALSE, TRUE)
        py <- plot.arg[2,1]
        h.dis <- plot.arg[2,2]
        
        
        if(incline){
            
            incline.cond <- df.loc$data$z[-c(1:2)] %>% table %>% as.numeric
            
            
            if(all(incline.cond >= 2) & incline.cond[1] == incline.cond[2]) {
                
                rw.dataframe$data <- f.rw(df.loc$data, sample.yr, incline, py, dpi, h.dis)
            } else {
                if(any(incline.cond < 2)){
                    
                    error.text <- paste('A minimum of two ring borders on each path ',
                                        'was required to generate a ring-width series')
                    sendSweetAlert(
                        session = session, title = "Error", text = error.text, type = "error"
                    )
                } else {
                    
                    error.text <-  paste("If incline = TRUE, the upper and lower paths", 
                                         "should have the same number of ring borders")
                    sendSweetAlert(
                        session = session, title = "Error", text = error.text, type = "error"
                    )
                }
            }
            
        } else {
            
            
            rw.dataframe$data <- f.rw(df.loc$data, sample.yr, incline, py, dpi, h.dis)
        }
        
    })
    
    
    
    output$results <- renderTable({
        
        if (is.null(rw.dataframe$data)) {
            return()
        } else {
            
            return(rw.dataframe$data)
        }
    })   
    
    
    observeEvent(input$button_hide, {
        if(is.null(rw.dataframe$data)){
            warn.text <- paste('You can not implement this operation because',
                               'the data frame to be deleted does not exist')
            sendSweetAlert(
                session = session, title = "Error", text = warn.text, type = "error"
            )
        } else {
            rw.dataframe$data <- NULL
            
        }
    })
    
    
    output$RingWidth.csv <- downloadHandler(
        filename =  function() {
            if(is.null(img.file$data)){
                img.name. <- 'Download Unavailable'
                return(paste0(img.name.,'.csv'))
            } else {
                img.name. <- input$tuid
            }
            if(input$csv.name !='')
                img.name. <- input$csv.name
            
            return(paste0(img.name.,'.csv'))
        },
        content = function(filename) {
            
            if(is.null(df.loc$data)){
                error.text <- 'Please verify that all necessary steps have been completed'
                sendSweetAlert(
                    session = session, title = "Error", text = error.text, type = "error"
                )
                
                return()
            } 
            
            
            sample.yr <- as.numeric(input$sample.yr)
            
            if(is.na(sample.yr)){
                error.text <- paste('Please check the argument \'Sampling year\'')
                sendSweetAlert(
                    session = session, title = "Error", text = error.text, type = "error"
                )
                return()
            }
            plot.arg <- df.loc$data[1:2,]
            dpi <- plot.arg[1,1]
            dp <- dpi/25.4
            incline <- ifelse(plot.arg[1,2]==0, FALSE, TRUE)
            py <- plot.arg[2,1]
            h.dis <- plot.arg[2,2]
            
            
            if(nrow(df.loc$data) <= 3){
                
                error.text <- paste('A minimum of two ring borders on each path ',
                                    'was required to generate a ring-width series')
                sendSweetAlert(
                    session = session, title = "Error", text = error.text, type = "error"
                )
                return()
            } 
            
            
            if(incline){
                
                incline.cond <- df.loc$data$z[-c(1:2)] %>%
                    table %>%
                    as.numeric
                
                
                if(all(incline.cond >= 2) & incline.cond[1] == incline.cond[2]) {
                    
                    df.rw <- f.rw(df.loc$data, sample.yr, incline, py, dpi, h.dis)
                    write.csv(df.rw, filename)
                    
                } else {
                    if(any(incline.cond < 2)){
                        
                        error.text <- paste('A minimum of two ring borders on each path ',
                                            'was required to generate a ring-width series')
                        sendSweetAlert(
                            session = session, title = "Error", text = error.text, type = "error"
                        )
                        return()
                    } else {
                        
                        error.text <- paste("If incline = TRUE, the upper and lower paths", 
                                            "should have the same number of ring borders")
                        sendSweetAlert(
                            session = session, title = "Error", text = error.text, type = "error"
                        )
                        return()
                    }
                }
                
            } else {
                
                
                df.rw <- f.rw(df.loc$data, sample.yr, incline, py, dpi, h.dis)
                write.csv(df.rw, filename)
            }
            
        },
        contentType = 'csv'
    )
    
    
    observeEvent(input$reset.hdr,{
        updateTextInput(session, "tuhdr1",label = 'Site ID',value = '')
        updateTextInput(session, "tuhdr2",label = 'Site Name',value = '')
        updateTextInput(session, "tuhdr3",label = 'Species Code',value = '')
        updateTextInput(session, "tuhdr4",label = 'State or Country',value = '')
        updateTextInput(session, "tuhdr5",label = 'Species',value = '')
        updateTextInput(session, "tuhdr6",label = 'Elevation',value = '')
        updateTextInput(session, "tuhdr7",label = 'Latitude',value = '')
        updateTextInput(session, "tuhdr8",label = 'Longitude',value = '')
        updateTextInput(session, "tuhdr9",label = 'First Year',value = '')
        updateTextInput(session, "tuhdr10",label = 'Last Year',value = '')
        updateTextInput(session, "tuhdr11",label = 'Lead Investigator',value = '')
        updateTextInput(session, "tuhdr12",label = 'Completion Date',value = '')
    })
    
    output$RingWidth.rwl <- downloadHandler(
        filename =  function() {
            if(is.null(img.file$data)){
                img.name. <- 'Download Unavailable'
                return(paste0(img.name.,'.rwl'))
            } else {
                img.name. <- input$tuid
            }
            
            if(input$rwl.name !='')
                img.name. <- input$rwl.name
            return(paste0(img.name.,'.rwl'))
            
        }, 
        content = function(filename) {
            seriesID <- df.loc$ID
            miss.id1 <- seriesID == ''
            if(miss.id1){
                error.text <- paste('The series ID has not been entered. The series',
                                    'ID is used as the column name of the data frame')
                sendSweetAlert(
                    session = session, title = "Error", text = error.text, type = "error"
                )
                return()
            }
            
            
            if(is.null(df.loc$data)){
                error.text <- 'Please verify that all necessary steps have been completed'
                sendSweetAlert(
                    session = session, title = "Error", text = error.text, type = "error"
                )
                
                return()
            } 
            
            
            sample.yr <- as.numeric(input$sample.yr)
            
            
            if(is.na(sample.yr)){
                error.text <- paste('Please check the argument \'Sampling year\'')
                sendSweetAlert(
                    session = session, title = "Error", text = error.text, type = "error"
                )
                return()
            }
            
            if(nrow(df.loc$data) <= 3){
                
                error.text <- paste('A minimum of two ring borders on each path ',
                                    'was required to generate a ring-width series')
                sendSweetAlert(
                    session = session, title = "Error", text = error.text, type = "error"
                )
            } 
            
            
            plot.arg <- df.loc$data[1:2,]
            dpi <- plot.arg[1,1]
            dp <- dpi/25.4
            incline <- ifelse(plot.arg[1,2]==0, FALSE, TRUE)
            py <- plot.arg[2,1]
            h.dis <- plot.arg[2,2]
            
            
            df.rw <- NULL
            
            
            if(incline){
                
                incline.cond <- df.loc$data$z[-c(1:2)] %>% table %>% as.numeric
                
                
                if(all(incline.cond >= 2) & incline.cond[1] == incline.cond[2]) {
                    
                    df.rw <- f.rw(df.loc$data, sample.yr, incline, py, dpi, h.dis)
                } else {
                    if(any(incline.cond < 2)){
                        
                        error.text <- paste('A minimum of two ring borders on each path ',
                                            'was required to generate a ring-width series')
                        sendSweetAlert(
                            session = session, title = "Error", text = error.text, type = "error"
                        )
                        return()
                    } else {
                        
                        error.text <- paste("If incline = TRUE, the upper and lower paths", 
                                            "should have the same number of ring borders")
                        sendSweetAlert(
                            session = session, title = "Error", text = error.text, type = "error"
                        )
                        return()
                    }
                }
                
            } else {
                
                
                df.rw <- f.rw(df.loc$data, sample.yr, incline, py, dpi, h.dis)
            }
            
            
            
            df.rwl <- data.frame(df.rw$ring.width, row.names = df.rw$year)
            
            
            tuid <- seriesID
            tuprec <- as.numeric(input$tuprec)
            tuheader <- input$tuheader
            tuhdr1 <- input$tuhdr1
            tuhdr2<- input$tuhdr2
            tuhdr3 <- input$tuhdr3
            tuhdr4 <- input$tuhdr4
            tuhdr5 <- input$tuhdr5
            tuhdr6 <- input$tuhdr6
            tuhdr7 <- input$tuhdr7
            tuhdr8 <- input$tuhdr8
            tuhdr9 <- input$tuhdr9
            tuhdr10 <- input$tuhdr10
            tuhdr11 <- input$tuhdr11
            tuhdr12 <- input$tuhdr12
            
            
            colnames(df.rwl) <- tuid
            
            
            hdr.list<- NULL
            
            
            if(tuheader){
                hdr <- c(tuhdr1, tuhdr2, tuhdr3, tuhdr4, tuhdr5, tuhdr6, 
                         tuhdr7, tuhdr8, tuhdr9, tuhdr10, tuhdr11, tuhdr12)
                hdr.name <- c('site.id','site.name', 'spp.code', 'state.country', 
                              'spp','elev', 'lat', 'long', 'first.yr', 'last.yr',
                              'lead.invs', 'comp.date')
                which.not.empty <- hdr != ''
                if (any(which.not.empty)){
                    hdr.list <- lapply(hdr,function(x) x)
                    names(hdr.list)<-hdr.name
                }
            }
            
            write.rwl(rwl.df = df.rwl, fname = filename,
                      format = "tucson", header = hdr.list,
                      append = FALSE, prec = tuprec)
            
        }, contentType = "rwl")
}
