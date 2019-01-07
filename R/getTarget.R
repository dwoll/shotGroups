## get target definition in desired unit
getTarget <-
function(x, unit, dstTarget, conversion) {
    f    <- system.file("targets_add.rda", package="shotGroups")
    trgt <- readRDS(file=f)
    
    x <- if(is.character(x)) {
        if(!hasName(shotGroups::targets, x) && !hasName(trgt, x)) {
            stop("Target unknown, see help(targets) for a list")
        }
        
        if(startsWith(x, "x_")) {
            trgt[[x]]
        } else {
            shotGroups::targets[[x]]
        }
    } else if(is.numeric(x)) {
        x <- as.integer(x)
        if((x < 1L) || (x > (length(shotGroups::targets) + length(trgt)))) {
            stop("Target unknown, see help(targets) for a list")
        }
        
        if(x > length(shotGroups::targets)) {
            trgt[[x]]
        } else {
            shotGroups::targets[[x]]
        }
    } else {
        x
    }
    
    if(missing(unit) || is.na(unit)) {
        unit <- x[["unitTarget"]]
    }
    
    unit <- match.arg(tolower(unit),
                      choices=c("cm", "mm", "m", "km", "in", "ft", "yd",
                                "deg", "moa", "smoa", "rad", "mrad", "mil"))
    
    if(missing(dstTarget)) {
        dstTarget <- NA_real_
    }
    
    if(missing(conversion)) {
        conversion <- NA_character_
    }
    
    fun <- if(hasName(x, "convert_fun")) {
        eval(x[["convert_fun"]])
    } else {
        convertTarget_default
    }
    
    fun(x, unit, dstTarget, conversion)
}

convertTarget_default <- function(x, unit, dstTarget, conversion) {
    ## add ring radii
    if(!is.null(x$convert$ringW)) {
        x$convert$ringR <- with(x$convert,
                                c(ringD10i/2, ringD10/2,  # ring 10 with inner sub-division
                                  (ringD10/2) + seq(ringW, ringW*(x$nRings-1), by=ringW)))
    }
    
    ## add ring radii if second radius is used
    if(!is.null(x$convert$ringWL)) {
        x$convert$ringR <- with(x$convert,
                                c(ringD10i/2, ringD10/2,  # ring 10 with inner sub-division
                                  (ringD10/2) +                        seq(ringW,  ringW *(x$nSmall-1), by=ringW),
                                  (ringD10/2) + (ringW*(x$nSmall-1)) + seq(ringWL, ringWL*(x$nLarge),   by=ringWL)))
    }
    
    ## add ring radii if third radius is used
    if(!is.null(x$convert$ringWLL)) {
        x$convert$ringR <- with(x$convert,
                                c(ringD10i/2, ringD10/2,  # ring 10 with inner sub-division
                                  (ringD10/2) +                        seq(ringW,   ringW *(x$nSmall-1), by=ringW),
                                  (ringD10/2) + (ringW*(x$nSmall-1)) + seq(ringWL,  ringWL*(x$nLarge),   by=ringWL),
                                  (ringD10/2) + (ringW*(x$nSmall-1)) + ringWL*(x$nLarge) +
                                      seq(ringWLL, ringWLL*(x$nLargeL), by=ringWLL)))
    }
    
    ## add ring center vertical offset if present
    if(!is.null(x$convert$ringWV)) {
        x$convert$ringRV <- with(x$convert,
                                 c(ringD10Vi/2, ringD10V/2,  # ring 10 with inner sub-division
                                   (ringD10V/2) + seq(ringWV, ringWV*(x$nRings-1), by=ringWV)))
    }
    
    ## convert all available measurements (ring width and radii, etc.)
    x$unitConv <- unit                           # add unit converted to
    x$inUnit <- if(tolower(unit) %in% c("deg", "moa", "smoa", "rad", "mrad", "mil"))  {  # angular size
        ## infer distance unit from conversion
        dst      <- unique(dstTarget)
        unitDst  <- unique(getUnits(conversion, first=TRUE))
        ringConv <- paste0(unitDst, "2", x[["unitTarget"]])
        Map(getMOA, x[["convert"]], conversion=ringConv, dst=dst, type=unit)
    } else {                                     # absolute size
        ringConv <- getConvFac(paste0(x[["unitTarget"]], "2", unit))
        Map(function(y, rc) { rc*y }, y=x[["convert"]], rc=ringConv)
    }

    xLims   <- c(-1, 1)
    yLims   <- c(-1, 1)
    setLims <- FALSE
    if(hasName(x[["inUnit"]], "ringR")) {
        xLims   <- range(c(xLims, -x$inUnit$ringR, x$inUnit$ringR))
        yLims   <- xLims
        setLims <- TRUE
    }

    if(hasName(x[["inUnit"]], "ringRV")) {
        yLims <- range(c(yLims, -x$inUnit$ringRV, x$inUnit$ringRV))
    }
    
    if(hasName(x[["inUnit"]], "ringD8")) {
        xLims <- range(c(xLims, -x$inUnit$ringD8/2,  x$inUnit$ringD8/2))
        yLims <- range(c(yLims, -x$inUnit$ringD8/2,  x$inUnit$ringD8/2))
        setLims <- TRUE
    }

    if(hasName(x[["inUnit"]], "ringD8V")) {
        yLims <- range(c(yLims, -x$inUnit$ringD8V/2, x$inUnit$ringD8V/2))
    }
    
    x[["xLims"]] <- xLims
    x[["yLims"]] <- yLims
    
    if(!setLims) { warning("Could not determine axis limits") }
    if(all(is.na(x[["xLims"]]))) {
        warning("Conversion failed")
        x[["failed"]] <- TRUE
    }
    x                                    # return selected target
}

convertTarget_x <- function(x, unit, dstTarget, conversion) {
    ## convert all available measurements
    x$unitConv <- unit                         # add unit converted to
    if(tolower(unit) %in% c("deg", "moa", "smoa", "rad", "mrad", "mil"))  {  # angular size
        ## infer distance unit from conversion
        unitDst  <- getUnits(conversion, first=TRUE)
        ringConv <- paste0(unitDst, "2", x[["unitTarget"]])

        ## coords may be negative
        if(hasName(x, "rings")) {
            x[["rings"]][["x"]]    <- getMOA(abs(x[["rings"]][["x"]]),
                                             conversion=ringConv, dst=dstTarget, type=unit) *
                                            sign(x[["rings"]][["x"]])
            x[["rings"]][["y"]]    <- getMOA(abs(x[["rings"]][["y"]]),
                                             conversion=ringConv, dst=dstTarget, type=unit) *
                                            sign(x[["rings"]][["y"]])
            x[["rings"]][["diam"]] <- getMOA(    x[["rings"]][["diam"]],
                                             conversion=ringConv, dst=dstTarget, type=unit)
        }
        
        if(hasName(x, "rings_top")) {
            x[["rings_top"]][["x"]]    <- getMOA(abs(x[["rings_top"]][["x"]]),
                                                 conversion=ringConv, dst=dstTarget, type=unit) *
                                                sign(x[["rings_top"]][["x"]])
            x[["rings_top"]][["y"]]    <- getMOA(abs(x[["rings_top"]][["y"]]),
                                                 conversion=ringConv, dst=dstTarget, type=unit) *
                                                sign(x[["rings_top"]][["y"]])
            x[["rings_top"]][["diam"]] <- getMOA(    x[["rings_top"]][["diam"]],
                                                 conversion=ringConv, dst=dstTarget, type=unit)
        }
        
        if(hasName(x, "polyL")) {
            for(i in seq_along(x[["polyL"]])) {
                x[["polyL"]][[i]][["points"]][["x"]] <- getMOA(abs(x[["polyL"]][[i]][["points"]][["x"]]),
                                                               conversion=ringConv, dst=dstTarget, type=unit) *
                                                              sign(x[["polyL"]][[i]][["points"]][["x"]])
                x[["polyL"]][[i]][["points"]][["y"]] <- getMOA(abs(x[["polyL"]][[i]][["points"]][["y"]]),
                                                               conversion=ringConv, dst=dstTarget, type=unit) *
                                                              sign(x[["polyL"]][[i]][["points"]][["y"]])
            }
        }
        
        if(hasName(x, "poly_topL")) {
            for(i in seq_along(x[["poly_topL"]])) {
                x[["poly_topL"]][[i]][["points"]][["x"]] <- getMOA(abs(x[["poly_topL"]][[i]][["points"]][["x"]]),
                                                                   conversion=ringConv, dst=dstTarget, type=unit) *
                                                                  sign(x[["poly_topL"]][[i]][["points"]][["x"]])
                x[["poly_topL"]][[i]][["points"]][["y"]] <- getMOA(abs(x[["poly_topL"]][[i]][["points"]][["y"]]),
                                                                   conversion=ringConv, dst=dstTarget, type=unit) *
                                                                  sign(x[["poly_topL"]][[i]][["points"]][["y"]])

            }
        }
        
        if(hasName(x, "txt")) {
            x[["txt"]][["x"]] <- getMOA(abs(x[["txt"]][["x"]]),
                                        conversion=ringConv, dst=dstTarget, type=unit) *
                                       sign(x[["txt"]][["x"]])
            x[["txt"]][["y"]] <- getMOA(abs(x[["txt"]][["y"]]),
                                        conversion=ringConv, dst=dstTarget, type=unit) *
                                       sign(x[["txt"]][["y"]])
        }
        
        x[["xLims"]] <- getMOA(abs(x[["xLims"]]),
                               conversion=ringConv, dst=dstTarget, type=unit) *
                              sign(x[["xLims"]])
        x[["yLims"]] <- getMOA(abs(x[["yLims"]]),
                               conversion=ringConv, dst=dstTarget, type=unit) *
                              sign(x[["yLims"]])
    } else {                                     # absolute size
        ringConv <- getConvFac(paste0(x[["unitTarget"]], "2", unit))
        if(hasName(x, "rings")) {
            x[["rings"]][["x"]]    <- ringConv*x[["rings"]][["x"]]
            x[["rings"]][["y"]]    <- ringConv*x[["rings"]][["y"]]
            x[["rings"]][["diam"]] <- ringConv*x[["rings"]][["diam"]]
        }
        
        if(hasName(x, "rings_top")) {
            x[["rings_top"]][["x"]]    <- ringConv*x[["rings_top"]][["x"]]
            x[["rings_top"]][["y"]]    <- ringConv*x[["rings_top"]][["y"]]
            x[["rings_top"]][["diam"]] <- ringConv*x[["rings_top"]][["diam"]]
        }
        
        if(hasName(x, "polyL")) {
            for(i in seq_along(x[["polyL"]])) {
                x[["polyL"]][[i]][["points"]][["x"]] <- ringConv*x[["polyL"]][[i]][["points"]][["x"]]
                x[["polyL"]][[i]][["points"]][["y"]] <- ringConv*x[["polyL"]][[i]][["points"]][["y"]]
            }
        }
        
        if(hasName(x, "poly_topL")) {
            for(i in seq_along(x[["poly_topL"]])) {
                x[["poly_topL"]][[i]][["points"]][["x"]] <- ringConv*x[["poly_topL"]][[i]][["points"]][["x"]]
                x[["poly_topL"]][[i]][["points"]][["y"]] <- ringConv*x[["poly_topL"]][[i]][["points"]][["y"]]
            }
        }
        
        if(hasName(x, "txt")) {
            x[["txt"]][["x"]] <- ringConv*x[["txt"]][["x"]]
            x[["txt"]][["y"]] <- ringConv*x[["txt"]][["y"]]
        }

        x[["xLims"]] <- ringConv*x[["xLims"]]
        x[["yLims"]] <- ringConv*x[["yLims"]]
    }
    
    if(all(is.na(x[["xLims"]]))) {
        warning("Conversion failed")
        x[["failed"]] <- TRUE
    }

    x                                  # return selected target
}
