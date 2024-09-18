## function to scale group coordinates to discount
## excess variance
scaleGroup <-
function(xy, varX, varY) {
    UseMethod("scaleGroup")
}

scaleGroup.data.frame <-
function(xy, varX, varY) {
    xy <- getXYmat(xy, xyTopLeft=FALSE)

    scaleGroup(xy, varX=varX, varY=varY)
    # NextMethod("scaleGroup")
}

scaleGroup.default <-
function(xy, varX, varY) {
    xy <- as.matrix(xy)
    if(!is.numeric(xy)) { stop("xy must be numeric") }
    if(ncol(xy) != 2L)  { stop("xy must have two columns") }
    if(nrow(xy) <  2L)  { stop("xy must have at least two rows") }

    ## determine x, y columns
    idxX <- which(colnames(xy) %in% c("x", "point.x"))
    idxY <- which(colnames(xy) %in% c("y", "point.y"))
    
    if(length(idxX) < 1L) {
        warning("No x/point.x column found. Assume column 1.")
        idxX <- 1
    } else if(length(idxX) > 1L) {
        warning("Multiple x/point.x columns found. Assume 1st one.")
        idxX <- idxX[1]
    }

    if(length(idxY) < 1L) {
        warning("No y/point.y column found. Assume column 2.")
        idxY <- 2
    } else if(length(idxY) > 1L) {
        warning("Multiple y/point.y columns found. Assume 1st one.")
        idxY <- idxY[1]
    }
    
    ## extract horizontal and vertical variance, group center
    varXY <- diag(cov(xy))
    ctr   <- colMeans(xy)
    
    ## center group before rescaling
    xyCtr <- scale(xy, center=ctr, scale=FALSE)
    xyScl <- xyCtr
    
    if(!missing(varX)) {
        ## rescale x coords
        if(is.logical(varX)) {
            if(varX) {
                ## rescale x to match variance of y
                sclFac <- sqrt(varXY[idxY] / varXY[idxX])
                xyScl[ , idxX] <- sclFac*xyCtr[ , idxX]
            }
        } else {
            ## remove variance component from x
            varX <- varX[1]
            stopifnot(is.numeric(varX),
                      varX > 0,
                      varX < varXY[idxX])
            
            sclFac <- sqrt((varXY[idxX]-varX) / varXY[idxX])
            xyScl[ , idxX] <- sclFac*xyCtr[ , idxX]
        }
    }
    
    if(!missing(varY)) {
        if(is.logical(varY)) {
            if(varY) {
                ## rescale y to match variance of x
                sclFac <- sqrt(varXY[idxX] / varXY[idxY])
                xyScl[ , idxY] <- sclFac*xyCtr[ , idxY]
            }
        } else {
            ## remove variance component from y
            varY <- varY[1]
            stopifnot(is.numeric(varY),
                      varY > 0,
                      varY < varXY[idxY])
            
            sclFac <- sqrt((varXY[idxY]-varY) / varXY[idxY])
            xyScl[ , idxY] <- sclFac*xyCtr[ , idxY]
        }
    }
    
    ## move group back into original centroid
    sweep(xyScl, MARGIN=2, STATS=ctr, FUN="+")
}
