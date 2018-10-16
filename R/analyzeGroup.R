analyzeGroup <-
function(DF, xyTopLeft=TRUE, center=FALSE,
         dstTarget, conversion, bandW=0.5,
         CEPtype="CorrNormal", bootCI="none") {
    if(!is.data.frame(DF)) { stop("DF must be a data frame") }

    ## convert DF names to lower case
    DF <- setNames(DF, tolower(names(DF)))

    #####-----------------------------------------------------------------------
    ## make sure DF has the required variable names
    varNames <- names(DF)                # what variables are present
    needsXY1 <- c("point.x", "point.y")  # coordinates must have this name
    needsXY2 <- c("x", "y")              # or this
    wantsAIM <- c("aim.x", "aim.y")      # useful
    hasXY1   <- needsXY1 %in% varNames
    hasXY2   <- needsXY2 %in% varNames
    hasAIM   <- wantsAIM %in% varNames   # useful ones we have

    if(!xor(all(hasXY1), all(hasXY2))) {
        stop("Coordinates must be named either x, y or point.x, point.y")
    }

    if(!all(hasAIM)) {
        warning(c("The data frame is missing variable(s)\n",
            paste(wantsAIM[!hasAIM], collapse=" "), "\n",
            "Point of Aim is assumed to be in (0,0)"))
        DF$aim.x <- 0
        DF$aim.y <- 0
    }

    ## distance to target from override or from data
    if(missing(dstTarget)) {
        dstTarget <- if(hasName(DF, "distance")) {
            DF[["distance"]]
        } else {
            NA_real_
        }
    }

    ## determine conversion factor from data if override is not given
    if(missing(conversion)) {
        conversion <- determineConversion(DF)
    }

    #####-----------------------------------------------------------------------
    ## prepare data: get (x,y)-coords relative to point of aim as matrix
    xy <- getXYmat(DF, xyTopLeft=xyTopLeft, center=center)

    #####-----------------------------------------------------------------------
    ## assess shape, location and spread
    shape    <- groupShape(xy, plots=TRUE, bandW=bandW, outlier="mcd",
                           dstTarget=dstTarget, conversion=conversion)
    location <- groupLocation(xy, plots=FALSE, bootCI=bootCI, level=0.95,
                              dstTarget=dstTarget, conversion=conversion)
    spread   <- groupSpread(xy, plots=TRUE, CEPlevel=0.5, CIlevel=0.95,
                            CEPtype=CEPtype, bootCI=bootCI,
                            dstTarget=dstTarget, conversion=conversion)

    #####-----------------------------------------------------------------------
    ## return all the collected numerical results and tests
    return(c(shape, location, spread))
}
