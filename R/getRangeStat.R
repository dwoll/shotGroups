getRangeStat <-
function(xy, dstTarget, conversion) {
    UseMethod("getRangeStat")
}

getRangeStat.data.frame <-
function(xy, dstTarget, conversion) {
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

    xy <- getXYmat(xy, xyTopLeft=FALSE, center=FALSE, relPOA=FALSE)

    getRangeStat(xy, dstTarget=dstTarget, conversion=conversion)
    # NextMethod("getRangeStat")
}

getRangeStat.default <-
function(xy, dstTarget, conversion) {
    xy <- as.matrix(xy)
    if(!is.numeric(xy))        { stop("xy must be numeric") }

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
    
    bb  <- getBoundingBox(xy)
    return(c(ES=getMaxPairDist(xy)$d,
             FoM=bb$FoM,
             D=bb$diag))
}
