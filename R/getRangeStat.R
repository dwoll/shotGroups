getRangeStat <-
function(xy, dstTarget, conversion, CIlevel=0.95) {
    UseMethod("getRangeStat")
}

getRangeStat.data.frame <-
function(xy, dstTarget, conversion, CIlevel=0.95) {
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

    getRangeStat(xy, dstTarget=dstTarget, conversion=conversion,
                 CIlevel=CIlevel)
    # NextMethod("getRangeStat")
}

getRangeStat.default <-
function(xy, dstTarget, conversion, CIlevel=0.95) {
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
    
    ## check if CIlevel is given in percent
    if(CIlevel >= 1) {
        while(CIlevel >= 1) { CIlevel <- CIlevel / 100 }
        warning(c("CIlevel must be in (0,1) and was set to ", CIlevel))
    }
    
    bb     <- getBoundingBox(xy)
    ES     <- getMaxPairDist(xy)[["d"]]
    FoM    <- bb$FoM
    D      <- bb$diag
    getRangeStatCI(c(ES, FoM, D),
                   stat=c("ES", "FoM", "D"),
                   n=NA_integer_,       # TODO
                   nGroups=NA_integer_, # TODO
                   dstTarget=dstTarget,
                   conversion=conversion,
                   CIlevel=CIlevel)
}
