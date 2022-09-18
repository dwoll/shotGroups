getConfEll <-
function(xy, level=0.5, dstTarget, conversion,
         center=FALSE, doRob=TRUE) {
    UseMethod("getConfEll")
}

getConfEll.data.frame <-
function(xy, level=0.5, dstTarget, conversion,
         center=FALSE, doRob=TRUE) {
    ## distance to target from override or from data
    if(missing(dstTarget)) {
        dstTarget <- if(hasName(xy, "distance")) {
            xy[["distance"]]
        } else {
            NA_real_
        }
    }
    
    ## determine conversion factor from data if override is not given
    if(missing(conversion)) {
        conversion <- determineConversion(xy)
    }

    xy     <- getXYmat(xy, xyTopLeft=FALSE, center=center, relPOA=FALSE)
    center <- FALSE                   # centering was done in getXYmat()

    getConfEll(xy, level=level, dstTarget=dstTarget, conversion=conversion,
               center=center, doRob=doRob)
    # NextMethod("getConfEll")
}

getConfEll.default <-
function(xy, level=0.5, dstTarget, conversion,
         center=FALSE, doRob=TRUE) {
    if(!is.matrix(xy))     { stop("xy must be a matrix") }
    if(!is.numeric(xy))    { stop("xy must be numeric") }
    if(nrow(xy) < 2L)      { stop("we need >= 2 points for confidence ellipse") }
    if(!is.numeric(level)) { stop("level must be numeric") }
    if(level <= 0)         { stop("level must be > 0") }
    if(center) {
        warning("Centering only works for data frames, ignored here")
    }
    
    ## check if CI level is given in percent
    if(level >= 1) {
        while(level >= 1) { level <- level / 100 }
        warning(c("level must be in (0,1) and was set to ", level))
    }

    dstTarget <- if(missing(dstTarget)    ||
                    all(is.na(dstTarget)) ||
                    (length(unique(dstTarget)) > 1L)) {
        NA_real_
    } else {
        mean(dstTarget)
    }
    
    conversion <- if(missing(conversion)    ||
                     all(is.na(conversion)) ||
                     (length(unique(conversion)) > 1L)) {
        NA_character_
    } else {
        unique(conversion)
    }
    
    ## group center and covariance matrix
    ctr    <- colMeans(xy)                # group center
    covXY  <- cov(xy)                     # covariance matrix (x,y)-coords
    Seig   <- eigen(covXY)
    ellRad <- sqrt(Seig$values)           # radii error ellipse
    trXY   <- sum(diag(covXY))            # trace of covariance matrix
    detXY  <- det(covXY)                  # determinant

    e    <- Seig$vectors[ , 1]
    eUp  <- e * sign(e[2])                # rotate upwards 180 deg if necessary
    lens <- sqrt(Seig$values)
    deg  <- atan2(eUp[2], eUp[1])*180 / pi  # angle in degrees
    
    ## magnification factor to turn error ellipse into confidence ellipse
    Npts <- nrow(xy)                      # number of observations
    dfn  <- ncol(xy)                      # numerator df
    dfd  <- Npts-1                        # denominator df
    mag  <- sqrt(dfn*qf(level, dfn, dfd)) # magnification factor = t-value
    
    ## radii confidence ellipse
    size <- makeMOA(mag*ellRad, dst=dstTarget, conversion=conversion)
    colnames(size) <- if(dfn == 2L) {
        c("semi-major", "semi-minor")
    } else {
        paste("semi-axis", seq_len(dfn), sep="-")
    }

    ## error ellipse characteristics -> radii = sqrt of eigenvalues
    ## aspect ratio of ellipse = sqrt of kappa condition index
    aspRat <- sqrt(kappa(covXY, exact=TRUE))
    flat   <- 1 - (1/aspRat)             # flattening
    shape  <- c(angle=deg, aspectRatio=aspRat, flattening=flat, trace=trXY, det=detXY)

    ## can we do robust estimation?
    haveRobustbase <- requireNamespace("robustbase", quietly=TRUE)
    haveRob <- if(haveRobustbase && (Npts >= 4L)) {
        TRUE
    } else {
        if(doRob && (Npts < 4L)) {
            warning("We need >= 4 points for robust estimations")
        }
        
        if(doRob && !haveRobustbase) {
            warning("Please install package 'robustbase' for robust estimations")
        }
        
        FALSE
    }
    
    if(doRob && haveRob) {               # same for robust estimation
        rob       <- robustbase::covMcd(xy)
        ctrRob    <- rob$center
        covXYrob  <- rob$cov
        Srobeig   <- eigen(covXYrob)
        ellRadRob <- sqrt(Srobeig$values)          # radii error ellipse
        trXYrob   <- sum(diag(covXYrob))           # trace of covariance matrix
        detXYrob  <- det(covXYrob)                 # determinant
        ## radii robust confidence ellipse
        sizeRob <- makeMOA(mag*ellRadRob, dst=dstTarget, conversion=conversion)
        colnames(sizeRob) <- colnames(size)

        e    <- Srobeig$vectors[ , 1]
        eUp  <- e * sign(e[2])                  # rotate upwards 180 deg if necessary
        lens <- sqrt(Srobeig$values)
        deg  <- atan2(eUp[2], eUp[1])*180 / pi  # angle in degrees
        
        aspRatRob <- sqrt(kappa(covXYrob, exact=TRUE))   # aspect ratio
        flatRob   <- 1 - (1/aspRatRob)   # flattening
        shapeRob  <- c(angle=deg, aspectRatio=aspRatRob, flattening=flatRob,
                       trace=trXYrob, det=detXYrob)
    } else {
        ## set robust estimates to NULL if not available
        sizeRob  <- NULL
        shapeRob <- NULL
        ctrRob   <- NULL
        covXYrob <- NULL
    }                                    # if(!(doRob && haveRob))

    return(list(ctr=ctr, ctrRob=ctrRob, cov=covXY, covRob=covXYrob,
                size=size, sizeRob=sizeRob,
                shape=shape, shapeRob=shapeRob, magFac=mag))
}
