getKuchnost <-
function(xy, dstTarget, conversion, center=FALSE, doRob=FALSE,
         strict=FALSE) {
    UseMethod("getKuchnost")
}

getKuchnost.data.frame <-
function(xy, dstTarget, conversion, center=FALSE, doRob=FALSE,
         strict=FALSE) {
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

    getKuchnost(xy, dstTarget=dstTarget, conversion=conversion,
                center=center, doRob=doRob, strict=strict)
}

getKuchnost.default <-
function(xy, dstTarget, conversion, center=FALSE, doRob=FALSE,
         strict=FALSE) {
    xy <- as.matrix(xy)
    if(!is.numeric(xy))        { stop("xy must be numeric") }
    if(center) {
        warning("Centering only works for data frames, ignored here")
    }

    if(strict) {
        stopifnot(nrow(xy) == 4L)
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

    ## check if we can do robust estimation if so required
    haveRob <- if(nrow(xy) < 4L) {
        if(doRob) { warning("We need >= 4 points for robust estimations") }
        FALSE
    } else {
        TRUE
    }                                    # if(nrow(xy) < 4L)

    #####-----------------------------------------------------------------------
    ## some basic calculations used later
    ## calculate group center and distance to center
    ## for all shots, and leave-one-out
    idx <- seq_len(nrow(xy))
    getCtr <- function(z, rob=FALSE) {
        if(rob) {
            robustbase::covMcd(z, cor=FALSE)$center # robust estimate: group center
        } else {
            colMeans(z)
        }
    }
    
    getCtr_loo <- function(i, rob=FALSE) {
        xy_i    <- xy[i, ]
        xy_loo  <- xy[setdiff(idx, i), ]
        ctr_loo <- getCtr(xy_loo, rob=rob)
        dst_loo <- sqrt(rowSums(scale(xy_loo, center=TRUE, scale=FALSE)^2))
        dst_i   <- sqrt(sum((xy_i - ctr_loo)^2))
        list(xy_i=xy[i, ], dst_i=dst_i, ctr_loo=ctr_loo, dst_loo=dst_loo)
    }

    if(doRob && haveRob) {                # center
        ctr     <- getCtr(xy, rob=TRUE)
        ctr_loo <- lapply(idx, getCtr_loo, rob=TRUE)
    } else {
        ctr     <- getCtr(xy, rob=FALSE)
        ctr_loo <- lapply(idx, getCtr_loo, rob=FALSE)
    }

    #####-----------------------------------------------------------------------
    ## Kuchnost estimate
    ## identify outliers = a shot 2.5 times or more distant from mean point
    ## of impact of the other three shots than any of these three shots
    isOutlier <- function(z) {
        z$dst_i >= 2.5 * max(z$dst_loo)
    }

    ## remove outliers
    outlier <- vapply(ctr_loo, isOutlier, logical(1))
    xy_sub  <- xy[!outlier, ]
    ctr_sub <- colMeans(xy_sub)
    
    ## using mean point of impact as the center, calculate minimum radius
    ## of circle that encloses all shots
    dst_all <- sqrt(rowSums(scale(xy_sub, center=ctr_sub, scale=FALSE)^2))

    return(list(Kuchnost=max(dst_all),
                outlier=which(outlier),
                ctr=ctr_sub))
}
