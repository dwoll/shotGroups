simRingCount <-
function(xy, center=FALSE, target, caliber, unit="cm", score) {
    UseMethod("simRingCount")
}

simRingCount.data.frame <-
function(xy, center=FALSE, target, caliber, unit="cm", score) {
    ## if caliber unspecified but data has caliber, use that
    ## need not be unique
    if(missing(caliber) && hasName(xy, "caliber")) {
        caliber <- xy[["caliber"]]
    }

    ## if target unspecified but data has unique target, use that
    if(missing(target)       &&
       hasName(xy, "target") &&
       length(unique(xy[["target"]])) == 1L) {
        target <- unique(xy[["target"]])
    }

    if(missing(score) && hasName(xy, "score")) {
        
    }

    xy     <- getXYmat(xy, center=center)
    center <- FALSE                   # centering was done in getXYmat()

    simRingCount(xy=xy, center=center, target=target, caliber=caliber,
                 unit=unit, score=score)
}

simRingCount.default <-
function(xy, center=FALSE, target, caliber, unit="cm", score) {
    if(!is.matrix(xy))       { stop("xy must be a matrix") }
    if(!is.numeric(xy))      { stop("xy must be numeric") }
    if(ncol(xy) != 2L)       { stop("xy must have two columns") }
    if(!is.numeric(caliber)) { stop("caliber must be numeric") }
    if(any(caliber <= 0))    { stop("caliber must be > 0") }
    if(center) {
        warning("Centering only works for data frames, ignored here")
    }
    
    unit <- match.arg(unit, choices=c("cm", "mm", "m", "km", "in", "ft", "yd",
                                      NA_character_))

    ## prepare data
    ## get target definition in requested unit
    ## dstTarget and conversion don't matter here
    trgt <- getTarget(target, unit=unit, dstTarget=1, conversion="mm2mm")

    ## convert caliber to required unit
    convFac <- getConvFac(paste0("mm2", unit))
    calSize <- convFac * caliber

    ## do we need a special simScore function?
    fun <- if(hasName(trgt, "simScore_fun")) {
        eval(trgt[["simScore_fun"]])
        
    } else {
        simScore_default
    }
    
    fun(xy, target=trgt, calSize=calSize)
}

simScore_default <-
function(xy, target, calSize) {
    ## get distance of inner edge of bullet hole to point of aim (0,0)
    ## negative difference -> distance from other side
    dstPOA <- abs(sqrt(rowSums(xy^2)) - calSize/2)

    ## cut with breaks = ring radii
    rings <- if(hasName(target, "countMouche") && target$countMouche) {
        ## with 1st ring (mouche, ring 10 inner sub-division)
        maxAll <- with(target, maxVal+1)
        if(!all(is.na(unlist(target$inUnit)))) {
            with(target$inUnit, cut(dstPOA, breaks=c(0, ringR, Inf),
                                    labels=c((target$maxVal+1):(target$maxVal-target$nRings+1), 0)),
                 include.lowest=TRUE)
        } else {
             rep(NA_real_, nrow(xy))
         }
    } else {
        ## except 1st ring (mouche, ring 10 inner sub-division)
        maxAll <- with(target, maxVal)
        if(!all(is.na(unlist(target$inUnit)))) {
            with(target$inUnit, cut(dstPOA, breaks=c(0, ringR[-1], Inf),
                                    labels=c(target$maxVal:(target$maxVal-target$nRings+1), 0)),
                 include.lowest=TRUE)
        } else {
            rep(NA_real_, nrow(xy))
        }
    }

    ## convert factor labels to numeric, then sum
    ringCount <- sum(as.numeric(as.character(rings)))  # observed ring count
    ringMax   <- maxAll * nrow(xy)                     # maximum possible

    return(list(count=ringCount, max=ringMax, rings=rings))
}

## TODO
simScore_DSUb <-
function(xy, target, calSize) {
#     ctrHi <- with(target$inUnit, cbind(0,   ringRV-ringR))
#     ctrLo <- with(target$inUnit, cbind(0, -(ringRV-ringR)))
#     ## get convex polygons for all rings
#     getRingPoly <- function(x, ang, i) {
#         pts <- with(x$inUnit, drawDSUOval(c(0, 0),
#             h=ringRV[i]-ringR[i],
#             radius=ringR[i],
#             angle=ang,
#             fg=colsTxt[i],
#             bg=cols[i],
#             plot=FALSE))
#
#         pts[chull(pts), ]
#     }
#
#     ang <- (180/pi)*with(target$inUnit, atan((ringRV-ringR) / ringR))
#     ringPoly <- Map(getRingPoly, list(target), ang, rev(seq_along(target$inUnit$ringR)))
# 
#     ## get distance of inner edge of bullet hole to point of aim (0,0)
#     ## negative difference -> distance from other side
#     dstPOA <- abs(sqrt(rowSums(xy^2)) - calSize/2)
# 
#     ## cut with breaks = ring radii
#     rings <- if(hasName(target, "countMouche") && target$countMouche) {
#         ## with 1st ring (mouche, ring 10 inner sub-division)
#         maxAll <- with(target, maxVal+1)
#         with(target$inUnit, cut(dstPOA, breaks=c(0, ringR, Inf),
#                                 labels=c((target$maxVal+1):(target$maxVal-target$nRings+1), 0)),
#                                 include.lowest=TRUE)
#     } else {
#         ## except 1st ring (mouche, ring 10 inner sub-division)
#         maxAll <- with(target, maxVal)
#         with(target$inUnit, cut(dstPOA, breaks=c(0, ringR[-1], Inf),
#                                 labels=c(target$maxVal:(target$maxVal-target$nRings+1), 0)),
#                                 include.lowest=TRUE)
#     }
# 
#     ## convert factor labels to numeric, then sum
#     ringCount <- sum(as.numeric(as.character(rings)))  # observed ring count
#     ringMax   <- maxAll * nrow(xy)                     # maximum possible

#    return(list(count=ringCount, max=ringMax, rings=rings))
    warning("Simulated ring count not yet available for this target")
    return(list(count=NA_integer_, max=NA_integer_, rings=NA_integer_))
}

## TODO
simScore_DSUa <- simScore_DSUb
simScore_x    <- simScore_DSUb