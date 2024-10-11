compareGroups <-
function(DF,
         plots=TRUE,
         xyTopLeft=TRUE,
         center=FALSE,
         ABalt=c("two.sided", "less", "greater"),
         Walt=c("two.sided", "less", "greater"),
         CEPtype="CorrNormal",
         CEPlevel=0.5,
         CIlevel=0.95,
         dstTarget, conversion) {
    if(!is.data.frame(DF))    { stop("DF must be a data frame") }
    if(!is.numeric(CEPlevel)) { stop("CEPlevel must be numeric") }
    if(CEPlevel <= 0)         { stop("CEPlevel must be > 0") }
    if(!is.numeric(CIlevel))  { stop("CIlevel must be numeric") }
    if(CIlevel <= 0)          { stop("CIlevel must be > 0") }

    CEPtype <- match.arg(CEPtype,
                         choices=c("CorrNormal", "GrubbsPearson", "GrubbsLiu",
                                   "GrubbsPatnaik", "Rayleigh", "Krempasky",
                                   "Ignani", "RMSE", "Ethridge", "RAND", "Valstar"),
                         several.ok=FALSE)

    ## check if CEP / CI level is given in percent
    if(CEPlevel >= 1) {
        while(CEPlevel >= 1) { CEPlevel <- CEPlevel / 100 }
        warning(c("CEPlevel must be in (0,1) and was set to ", CEPlevel))
    }

    if(CIlevel >= 1) {
        while(CIlevel >= 1) { CIlevel <- CIlevel / 100 }
        warning(c("CIlevel must be in (0,1) and was set to ", CIlevel))
    }

    ## convert DF names to lower case
    DF <- setNames(DF, tolower(names(DF)))

    #####-----------------------------------------------------------------------
    ## make sure DF has the required variable names and at least two groups
    varNames <- names(DF)                # what variables are present
    needsSer <- "series"                 # required
    needsXY1 <- c("point.x", "point.y")  # coordinates must have this name
    needsXY2 <- c("x", "y")              # or this
    # wantsDst <- "distance"               # useful
    wantsAIM <- c("aim.x", "aim.y")      # useful
    hasSer   <- needsSer %in% varNames   # required ones we have
    hasXY1   <- needsXY1 %in% varNames   # coordinates we have
    hasXY2   <- needsXY2 %in% varNames
    # hasDst   <- wantsDst %in% varNames   # useful ones we have
    hasAIM   <- wantsAIM %in% varNames   # useful ones we have

    if(!all(hasSer)) {
        stop(c("The data frame is missing variable\n",
               paste(needsSer[!hasSer], collapse=" ")))
    }

    if(!xor(all(hasXY1), all(hasXY2))) {
        stop("Coordinates must be named either X, Y or Point.X, Point.Y")
    }

    # if(!all(hasDst)) {
    #     warning(c("The data frame is missing variable\n",
    #               paste(wantsDst[!hasDst], collapse=" "), "\n",
    #               "Distance is assumed to be 100"))
    #     DF$distance <- 100
    # }

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
    ## prepare data
    res <- vector("list", 0)               # empty list to later collect the results
    if(!is.factor(DF$series)) {            # make sure series is a factor
        DF$series <- as.factor(DF$series)
    } else {
        DF$series <- droplevels(DF$series) # remove all non-used factor levels
    }

    ## check if we have enough groups
    if(nlevels(DF$series) < 2L) {
        stop("We need >= 2 groups for a comparison")
    }

    ## check if we have enough points per group
    cTab <- xtabs(~ series, data=DF)
    if(any(cTab < 2L)) {
        ## remove series with too few points
        rem <- names(cTab[cTab < 2L])
        idx <- DF[["series"]] %in% rem
        
        ## drop series entries with too few points
        ## from dstTarget and data frame
        if(length(dstTarget) == nrow(DF)) {
            dstTarget <- dstTarget[!idx]
        }
        
        DF <- droplevels(DF[!idx, , drop=FALSE])
        warning(paste0("Removed series ",
                       toString(rem),
                       " with < 2 points per group"))
    }

    ## prepare data: get (x,y)-coords relative to point of aim as matrix
    ## for each group extract the (x,y)-coords as a matrix
    extractXY <- function(x) {
        getXYmat(x, xyTopLeft=xyTopLeft, center=center)
    }

    xyL  <- lapply(split(DF, DF$series, drop=TRUE), extractXY)
    nS   <- length(xyL)                   # total number of series
    nObs <- vapply(xyL, nrow, integer(1)) # number of obs per series
    names(xyL) <- levels(DF$series)

    ## append (x,y)-coords to data frame
    DF <- cbind(DF, do.call("rbind", xyL))

    ## distances to target per group
    dstTargetGrp <- if(length(dstTarget) == nrow(DF)) {
        ## check that distance is homogeneous per group
        groupVar <- tapply(dstTarget, DF[["series"]], var)
        if(sum(groupVar) < sqrt(.Machine$double.eps)) {
            tapply(dstTarget, DF[["series"]], mean)
        } else {
            setNames(rep(NA_real_, nlevels(DF[["series"]])), levels(DF[["series"]]))
        }
    } else {
        if(length(dstTarget) == 1L) {
            setNames(rep(dstTarget, nlevels(DF[["series"]])), levels(DF[["series"]]))
        } else {
            setNames(rep(NA_real_, nlevels(DF[["series"]])), levels(DF[["series"]]))
        }
    }
    
    conversionGrp <- if(length(conversion) == nrow(DF)) {
        groupConv <- tapply(conversion, DF[["series"]], function(conv) { length(unique(conv)) })
        if(all(groupConv) == 1L) {
            tapply(conversion, DF[["series"]], unique)
        } else {
            setNames(rep(NA_character_, nlevels(DF[["series"]])), levels(DF[["series"]]))
        }
    } else {
        if(length(conversion) == 1L) {
            setNames(rep(conversion, nlevels(DF[["series"]])), levels(DF[["series"]]))
        } else {
            setNames(rep(NA_character_, nlevels(DF[["series"]])), levels(DF[["series"]]))
        }
    }

    ## to determine axis limits later, collect all results in a vector
    axisLimsX <- numeric(0)
    axisLimsY <- numeric(0)

    #####-----------------------------------------------------------------------
    ## location measures
    res$ctr <- vapply(xyL, colMeans, numeric(2))     # group centers
    distPOA <- sqrt(colSums(res$ctr^2))  # distances to point of aim
    distPOAmoa  <- Map(makeMOA, distPOA, dst=dstTargetGrp, conversion=conversionGrp)
    res$distPOA <- do.call("cbind", distPOAmoa)

    ## multivariate location test for equal group centers (relative to POA)
    res$MANOVA <- anova(lm(cbind(x, y) ~ series, data=DF), test="Wilks")

    #####-----------------------------------------------------------------------
    ## shape measures
    ## correlation matrices for x- and y-coords
    res$corXY <- lapply(xyL, cor)

    #####-----------------------------------------------------------------------
    ## spread measures
    ## standard deviations for x- and y-coords with parametric CIs
    alpha <- 1-CIlevel
    getSDxyCI <- function(x) {
        N    <- nrow(x)
        sdXY <- sqrt(diag(cov(x)))       # standard deviations

        ## and their parametric confidence intervals
        sdXci  <- sqrt((N-1)*sdXY["x"]^2 / qchisq(c(1-(alpha/2), alpha/2), N-1))
        sdYci  <- sqrt((N-1)*sdXY["y"]^2 / qchisq(c(1-(alpha/2), alpha/2), N-1))
        sdXYci <- c(sdXci, sdYci)

        setNames(sdXYci, c("sdX (", "sdX )", "sdY (", "sdY )"))
    }

    ## sd and sd CIs as separate lists for unit of measurement MOA, SMOA, mrad
    sdXY       <- lapply(xyL, function(x) sqrt(diag(cov(x)))) # standard deviations
    sdXYci     <- lapply(xyL, getSDxyCI) # confidence intervals
    res$sdXY   <- Map(makeMOA, sdXY,   dst=dstTargetGrp, conversion=conversionGrp)
    res$sdXYci <- Map(makeMOA, sdXYci, dst=dstTargetGrp, conversion=conversionGrp)

    ## (mean) distances to group center
    dstCtrL    <- lapply(xyL, getDistToCtr) # distances to group center
    meanDstCtr <- lapply(dstCtrL, mean)
    meanDstCtrMOA <- Map(makeMOA, meanDstCtr, dst=dstTargetGrp, conversion=conversionGrp)
    res$meanDistToCtr <- do.call("cbind", meanDstCtrMOA)

    ## maximum pairwise distance = maximum group spread
    maxPD      <- lapply(xyL, getMaxPairDist)   # max pairwise distance
    maxSpread  <- lapply(maxPD, function(x) { x$d } )
    maxPDidx   <- vapply(maxPD, function(x) { x$idx }, numeric(2))
    maxSpreadL <- Map(makeMOA, maxSpread, dst=dstTargetGrp, conversion=conversionGrp)
    res$maxPairDist <- do.call("cbind", maxSpreadL)

    ## bounding box figure of merit and diagonal
    ## bbs     <- lapply(xyL, getBoundingBox)   # bounding boxes
    bbs     <- lapply(xyL, getMinBBox)
    bbFoM   <- lapply(bbs, function(x) { x$FoM } )
    bbDiag  <- lapply(bbs, function(x) { x$diag } )
    bbFoML  <- Map(makeMOA, bbFoM,  dst=dstTargetGrp, conversion=conversionGrp)
    bbDiagL <- Map(makeMOA, bbDiag, dst=dstTargetGrp, conversion=conversionGrp)
    res$bbFoM  <- do.call("cbind", bbFoML)
    res$bbDiag <- do.call("cbind", bbDiagL)

    ## for axis limits
    bbMinPts  <- do.call("rbind", lapply(bbs, function(x) x$pts))
    axisLimsX <- c(axisLimsX, bbMinPts[ , 1])
    axisLimsY <- c(axisLimsY, bbMinPts[ , 2])

    ## radius of minimum enclosing circle
    minCircs    <- lapply(xyL, getMinCircle)
    minCircRad  <- lapply(minCircs, function(x) { x$rad } )     # radius
    minCircRadL <- Map(makeMOA, minCircRad, dst=dstTargetGrp, conversion=conversionGrp)
    res$minCircleRad <- do.call("cbind", minCircRadL)

    ## for axis limits
    getMinCircLims <- function(x) {
        cbind(X=c(x$ctr[1] + x$rad, x$ctr[1] - x$rad),
              Y=c(x$ctr[2] + x$rad, x$ctr[2] - x$rad))
    }
    minCircLims <- do.call("rbind", lapply(minCircs, getMinCircLims))
    axisLimsX   <- c(axisLimsX, minCircLims[ , 1])
    axisLimsY   <- c(axisLimsY, minCircLims[ , 2])

    ## Rayleigh sigma and MR
    rayParL <- Map(getRayParam, xyL, level=CIlevel, doRob=FALSE)
    sigma   <- lapply(rayParL, function(x) { x$sigma["sigma"]   })
    MR      <- lapply(rayParL, function(x) { x$MR["MR"]   })
    sigMRci <- lapply(rayParL, function(x) {
        sigMRci <- c(x$sigma[c("sigCIlo", "sigCIup")],
                     x$MR[   c("MRciLo",  "MRciUp")])
        setNames(sigMRci, c("sigma (", "sigma )", "MR (", "MR )"))
    })

    sigmaL <- Map(makeMOA, sigma, dst=dstTargetGrp, conversion=conversionGrp)
    MRL    <- Map(makeMOA, MR,    dst=dstTargetGrp, conversion=conversionGrp)
    res$sigma <- do.call("cbind", sigmaL)
    res$MR    <- do.call("cbind", MRL)
    res$sigmaMRci <- Map(makeMOA, sigMRci, dst=dstTargetGrp, conversion=conversionGrp)

    ## 50% circular error probable
    CEPlist <- Map(getCEP, xyL, CEPlevel=CEPlevel,
                   dstTarget=dstTargetGrp, conversion=conversionGrp,
                   type=CEPtype, doRob=FALSE)
    CEPl    <- lapply(CEPlist, function(x) { x$CEP[[1]][ , CEPtype, drop=FALSE] })
    CEPmat  <- do.call("cbind", CEPl)    # as matrix
    colnames(CEPmat) <- names(xyL)
    res$CEP <- CEPmat

    #####-----------------------------------------------------------------------
    ## tests for equal spread
    ## 2 groups:   Ansari-Bradley for x- and y-coords
    ##             Kruskal-Wallis for distance to center
    ## > 2 groups: Fligner-Killeen for x- and y-coords
    ##             Wilcoxon Rank Sum (= Mann-Whitney U) for distance to center
    dstCtrGrp <- unlist(dstCtrL)           # distances to group center grouped by series
    MRGrp     <- do.call("rbind", MRL)[ , "unit"]
    MRCIGrp   <- do.call("rbind", sigMRci)
    MRDF      <- data.frame(MR=MRGrp,
                            MRciLo=MRCIGrp[ , "MR ("],
                            MRciUp=MRCIGrp[ , "MR )"],
                            series=factor(levels(DF$series)))
    names(dstCtrGrp) <- NULL

    ## create data frame with added series factor
    dstCtrDF <- data.frame(dstCtr=dstCtrGrp,
                           series=factor(rep(seq_along(levels(DF$series)), nObs),
                                         labels=levels(DF$series)))

    if(nS == 2L) {                       # compare two groups
        if(requireNamespace("coin", quietly=TRUE)) {
            res$AnsariX  <- coin::ansari_test(x ~ series, alternative=ABalt,
                                              data=DF, distribution="exact")
            res$AnsariY  <- coin::ansari_test(y ~ series, alternative=ABalt,
                                              data=DF, distribution="exact")
            res$Wilcoxon <- coin::wilcox_test(dstCtr ~ series, alternative=Walt,
                                              data=dstCtrDF, distribution="exact")
        } else {
            res$AnsariX  <- ansari.test(x ~ series,      alternative=ABalt, data=DF)
            res$AnsariY  <- ansari.test(y ~ series,      alternative=ABalt, data=DF)
            res$Wilcoxon <- wilcox.test(dstCtr ~ series, alternative=Walt,  data=dstCtrDF)
        }
    } else {                             # compare more than two groups
        if(requireNamespace("coin", quietly=TRUE)) {
            res$FlignerX <- coin::fligner_test(x ~ series, data=DF,
                                               distribution=coin::approximate(nresample=9999))  # x
            res$FlignerY <- coin::fligner_test(y ~ series, data=DF,
                                               distribution=coin::approximate(nresample=9999))  # y
            res$Kruskal  <- coin::kruskal_test(dstCtr ~ series,    # dist to center
                                               data=dstCtrDF,
                                               distribution=coin::approximate(nresample=9999))
        } else {
            res$FlignerX <- fligner.test(x ~ series,      data=DF)  # x
            res$FlignerY <- fligner.test(y ~ series,      data=DF)  # y
            res$Kruskal  <- kruskal.test(dstCtr ~ series, data=dstCtrDF)
        }
    }

    if(plots) {
        ## infer (x,y)-coord units from conversion
        unitXY  <- paste(unique(na.omit(getUnits(conversion, first=FALSE))), collapse=", ")
        unitDst <- paste(unique(na.omit(getUnits(conversion, first=TRUE))),  collapse=", ")
        devNew  <- getDevice()           # platform-dependent window open

        ## distance to target may be heterogeneous
        dstTargetPlot <- paste(unique(round(na.omit(dstTargetGrp))), collapse=", ")

        #####-------------------------------------------------------------------
        ## diagram: 2D-scatter plot for the (x,y)-distribution
        syms <- c(4, 16, 2, 1, 6, 8, 3, 5, 7, 9:13, 15, 17:25)  # data symbols
        cols <- getColors(nS)            # colors

        if(nS > length(syms)) {
            stop(paste("At most", length(syms), "series possible"))
        }

        ## confidence ellipse
        confElls <- Map(getConfEll, xyL, level=CEPlevel, dst=dstTargetGrp, conversion=conversionGrp)

        ## adjust axis limits
        getConfEllLims <- function(x) {
            cbind(X=c(x$ctr[1] + x$size["unit", "semi-major"],
                      x$ctr[1] - x$size["unit", "semi-major"]),
                  Y=c(x$ctr[2] + x$size["unit", "semi-major"],
                      x$ctr[2] - x$size["unit", "semi-major"]))
        }

        confEllLims <- do.call("rbind", lapply(confElls, getConfEllLims))
        axisLimsX   <- c(axisLimsX, confEllLims[ , 1])
        axisLimsY   <- c(axisLimsY, confEllLims[ , 2])

        ## determine axis limits
        xLims <- range(c(DF$x, axisLimsX))
        yLims <- range(c(DF$y, axisLimsY))

        devNew()                         # open new diagram
        plot(y ~ x, data=DF, xlim=xLims, ylim=yLims, asp=1, lwd=2,
             pch=syms[unclass(DF$series)], col=cols[unclass(DF$series)],
             main=paste0("Groups with ", 100*CEPlevel, "% confidence ellipse"),
             sub=paste("distance:", dstTargetPlot, unitDst),
             xlab=paste0("X [", unitXY, "]"), ylab=paste0("Y [", unitXY, "]"))
        abline(v=0, h=0, col="lightgray")  # add point of aim

        ## add confidence ellipses and group centers
        for(i in seq(along=xyL)) {
            drawEllipse(confElls[[i]], fg=cols[i],
                        lwd=2, pch=syms[i], cex=3)
            points(res$ctr[1, i], res$ctr[2, i], pch=syms[i], col=cols[i],
                   cex=3, lwd=2)
        }

        ## add legend
        legend(x="bottomleft", legend=names(xyL), lty=NA, pch=syms[seq_len(nS)],
               col=cols[seq_len(nS)], lwd=2, pt.cex=1.5, bg=rgb(1, 1, 1, 0.6))

        #####-------------------------------------------------------------------
        ## diagram: 2D-scatter plot for the (x,y)-distribution
        devNew()                         # open new diagram
        plot(y ~ x, data=DF, asp=1, xlim=xLims, ylim=yLims, lwd=2,
             pch=syms[unclass(DF$series)], col=cols[unclass(DF$series)],
             main="Groups w/ minimum bounding box & maximum spread",
             sub=paste("distance:", dstTargetPlot, unitDst),
             xlab=paste0("X [", unitXY, "]"), ylab=paste0("Y [", unitXY, "]"))
        abline(v=0, h=0, col="lightgray")  # add point of aim
        points(res$ctr[1, ], res$ctr[2, ], col=cols[seq_len(nS)],
               pch=syms[seq_len(nS)], lwd=2, cex=3)

        ## add bounding box and maximum group spread
        for(i in seq(along=xyL)) {
            bb <- bbs[[i]]
            ## drawBox(bb, fg=cols[i])
            drawBox2(bb, fg=cols[i])
            segments(x0=xyL[[i]][maxPDidx[1, i], 1], y0=xyL[[i]][maxPDidx[1, i], 2],
                     x1=xyL[[i]][maxPDidx[2, i], 1], y1=xyL[[i]][maxPDidx[2, i], 2],
                     col=cols[i])
        }

        ## add legend
        legend(x="bottomleft", legend=names(xyL), lty=NA, pch=syms[seq_len(nS)],
               col=cols[seq_len(nS)], lwd=2, pt.cex=1.5, bg=rgb(1, 1, 1, 0.6))

        #####-------------------------------------------------------------------
        ## diagram: 2D-scatter plot for the (x,y)-distribution
        devNew()                         # open new diagram
        plot(y ~ x, data=DF, asp=1, xlim=xLims, ylim=yLims, lwd=2,
             pch=syms[unclass(DF$series)], col=cols[unclass(DF$series)],
             main="Groups w/ minimum enclosing circle and mean dist to center",
             sub=paste("distance:", dstTargetPlot, unitDst),
             xlab=paste0("X [", unitXY, "]"), ylab=paste0("Y [", unitXY, "]"))
        abline(v=0, h=0, col="lightgray")  # add point of aim
        points(res$ctr[1, ], res$ctr[2, ], col=cols[seq_len(nS)], pch=syms[seq_len(nS)],
               lwd=2, cex=3)

        ## add circle with mean distance to center and minimum enclosing circle
        for(i in seq(along=xyL)) {
            drawCircle(res$ctr[ , i], radius=meanDstCtr[[i]], fg=cols[i])
            drawCircle(minCircs[[i]], fg=cols[i])
        }

        ## add legend
        legend(x="bottomleft", legend=names(xyL), lty=NA, pch=syms[seq_len(nS)],
               col=cols[seq_len(nS)], lwd=2, pt.cex=1.5, bg=rgb(1, 1, 1, 0.6))

        #####-------------------------------------------------------------------
        ## diagram: distances to center
        ## grouped boxplot + Rayleigh MR+CI
        devNew()

        op <- par(mfrow=c(1, 2))
        yLims <- c(0, max(dstCtrDF$dstCtr, MRDF$MRciUp))
        boxplot(dstCtr ~ series, data=dstCtrDF,
                main="Distance to center",
                sub=paste("distance:", dstTargetPlot, unitDst),
                xaxt="n", col=cols, ylim=yLims,
                xlab="group", ylab=paste0("distance to center [", unitXY, "]"))
        axis(side=1, at=seq_along(levels(dstCtrDF$series)),
             labels=substring(levels(dstCtrDF$series), 1, 7), las=2)

        ## Rayleigh MR+CI
        ## raw distances to center
        stripchart(dstCtr ~ series, data=dstCtrDF, pch=20, vert=TRUE,
                   method="jitter",
                   main=paste0("Dist2ctr w/ Rayleigh MR + ", 100*CIlevel, "% CI"),
                   sub=paste("distance:", dstTargetPlot, unitDst),
                   xaxt="n", col=adjustcolor(cols, alpha.f=0.5),
                   xlim=range(seq_along(levels(dstCtrDF$series))) + c(-0.5, 0.5),
                   ylim=yLims,
                   xlab="group", ylab=paste0("distance to center [", unitXY, "]"))
        axis(side=1, at=seq_along(levels(dstCtrDF$series)),
             labels=substring(levels(dstCtrDF$series), 1, 7), las=2)

        ## MR per group
        with(MRDF,
             points(seq_along(series), MR, pch=4, cex=2, col="black", lwd=2))
        ## MR CI per group
        with(MRDF,
             arrows(x0=seq_along(series), y0=MRciLo,
                    x1=seq_along(series), y1=MRciUp,
                    code=3, angle=90, length=0.1, col="black", lwd=2))

        par(op)
    }                                    # if(plots)

    #####-----------------------------------------------------------------------
    ## return all the collected numerical results and tests
    return(res)
}

compareGroupsPlot <-
function(DF, which=1L, xyTopLeft=TRUE, center=FALSE,
         CEPlevel=0.5, CIlevel=0.95, dstTarget, conversion) {
    if(!is.data.frame(DF)) { stop("DF must be a data frame") }

    which <- match.arg(as.character(which), choices=1:4)

    #####-----------------------------------------------------------------------
    ## make sure DF has the required variable names and at least two groups
    varNames <- tolower(names(DF))       # what variables are present
    needsSer <- "series"                 # required
    needsXY1 <- c("point.x", "point.y")  # coordinates must have this name
    needsXY2 <- c("x", "y")              # or this
    wantsAIM <- c("aim.x", "aim.y")      # useful
    hasSer   <- needsSer %in% varNames   # required ones we have
    hasXY1   <- needsXY1 %in% varNames   # coordinates we have
    hasXY2   <- needsXY2 %in% varNames
    hasAIM   <- wantsAIM %in% varNames   # useful ones we have

    if(!all(hasSer)) {
        stop(c("The data frame is missing variable\n",
               paste(needsSer[!hasSer], collapse=" ")))
    }

    if(!xor(all(hasXY1), all(hasXY2))) {
        stop("Coordinates must be named either X, Y or Point.X, Point.Y")
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
    ## prepare data
    res <- vector("list", 0)               # empty list to later collect the results
    if(!is.factor(DF$series)) {            # make sure series is a factor
        DF$series <- as.factor(DF$series)
    } else {
        DF$series <- droplevels(DF$series) # remove all non-used factor levels
    }

    ## check if we have enough groups
    if(nlevels(DF$series) < 2L) {
        stop("We need >= 2 groups for a comparison")
    }
    
    ## check if we have enough points per group
    cTab <- xtabs(~ series, data=DF)
    if(any(cTab < 2L)) {
        ## remove series with too few points
        rem <- names(cTab[cTab < 2L])
        idx <- DF[["series"]] %in% rem
        if(length(dstTarget) == nrow(DF)) {
            dstTarget <- dstTarget[!idx]
        }
        
        DF <- droplevels(DF[!idx, , drop=FALSE])
        warning(paste0("Removed series ",
                       toString(rem),
                       " with < 2 points per group"))
    }
    
    ## prepare data: get (x,y)-coords relative to point of aim as matrix
    ## for each group extract the (x,y)-coords as a matrix
    extractXY <- function(x) {
        getXYmat(x, xyTopLeft=xyTopLeft, center=center)
    }
    
    xyL  <- lapply(split(DF, DF$series, drop=TRUE), extractXY)
    nS   <- length(xyL)                   # total number of series
    nObs <- vapply(xyL, nrow, integer(1)) # number of obs per series
    names(xyL) <- levels(DF$series)
    
    ## append (x,y)-coords to data frame
    DF <- cbind(DF, do.call("rbind", xyL))

    ## distances to target per group
    dstTargetGrp <- if(length(dstTarget) == nrow(DF)) {
        ## check that distance is homogeneous per group
        groupVar <- tapply(dstTarget, DF[["series"]], var)
        if(sum(groupVar) < sqrt(.Machine$double.eps)) {
            tapply(dstTarget, DF[["series"]], mean)
        } else {
            setNames(rep(NA_real_, nlevels(DF[["series"]])), levels(DF[["series"]]))
        }
    } else {
        if(length(dstTarget) == 1L) {
            setNames(rep(dstTarget, nlevels(DF[["series"]])), levels(DF[["series"]]))
        } else {
            setNames(rep(NA_real_, nlevels(DF[["series"]])), levels(DF[["series"]]))
        }
    }
    
    conversionGrp <- if(length(conversion) == nrow(DF)) {
        groupConv <- tapply(conversion, DF[["series"]], function(conv) { length(unique(conv)) })
        if(all(groupConv) == 1L) {
            tapply(conversion, DF[["series"]], unique)
        } else {
            setNames(rep(NA_character_, nlevels(DF[["series"]])), levels(DF[["series"]]))
        }
    } else {
        if(length(conversion) == 1L) {
            setNames(rep(conversion, nlevels(DF[["series"]])), levels(DF[["series"]]))
        } else {
            setNames(rep(NA_character_, nlevels(DF[["series"]])), levels(DF[["series"]]))
        }
    }
    
    ## to determine axis limits later, collect all results in a vector
    axisLimsX <- numeric(0)
    axisLimsY <- numeric(0)

    #####-----------------------------------------------------------------------
    ## location measures
    res$ctr <- vapply(xyL, colMeans, numeric(2))     # group centers

    ## (mean) distances to group center
    dstCtrL    <- lapply(xyL, function(x) getDistToCtr(x))
    meanDstCtr <- lapply(dstCtrL, mean)

    ## maximum pairwise distance = maximum group spread
    maxPD     <- lapply(xyL, getMaxPairDist)   # max pairwise distance
    maxSpread <- lapply(maxPD, function(x) { x$d } )
    maxPDidx  <- vapply(maxPD, function(x) { x$idx }, numeric(2))

    ## bounding box figure of merit and diagonal
    bbs    <- lapply(xyL, getMinBBox)
    bbDiag <- lapply(bbs, function(x) { x$diag } )

    ## for axis limits
    bbMinPts  <- do.call("rbind", lapply(bbs, function(x) x$pts))
    axisLimsX <- c(axisLimsX, bbMinPts[ , 1])
    axisLimsY <- c(axisLimsY, bbMinPts[ , 2])

    ## radius of minimum enclosing circle
    minCircs   <- lapply(xyL, getMinCircle)
    minCircRad <- lapply(minCircs, function(x) { x$rad } )     # radius

    ## for axis limits
    getMinCircLims <- function(x) {
        cbind(X=c(x$ctr[1] + x$rad, x$ctr[1] - x$rad),
              Y=c(x$ctr[2] + x$rad, x$ctr[2] - x$rad))
    }

    minCircLims <- do.call("rbind", lapply(minCircs, getMinCircLims))
    axisLimsX   <- c(axisLimsX, minCircLims[ , 1])
    axisLimsY   <- c(axisLimsY, minCircLims[ , 2])

    ## Rayleigh sigma and MR
    rayParL <- Map(getRayParam, xyL, level=CIlevel, doRob=FALSE)
    sigma   <- lapply(rayParL, function(x) { x$sigma["sigma"]   })
    MR      <- lapply(rayParL, function(x) { x$MR["MR"]   })
    sigMRci <- lapply(rayParL, function(x) {
        sigMRci <- c(x$sigma[c("sigCIlo", "sigCIup")],
                     x$MR[   c("MRciLo",  "MRciUp")])
        setNames(sigMRci, c("sigma (", "sigma )", "MR (", "MR )"))
    })

    sigmaL <- Map(makeMOA, sigma, dst=dstTargetGrp, conversion=conversionGrp)
    MRL    <- Map(makeMOA, MR,    dst=dstTargetGrp, conversion=conversionGrp)
    res$sigma <- do.call("cbind", sigmaL)
    res$MR    <- do.call("cbind", MRL)
    res$sigmaMRci <- Map(makeMOA, sigMRci, dst=dstTargetGrp, conversion=conversionGrp)

    ## distance to center, Rayleigh sigma + MR
    dstCtrGrp <- unlist(dstCtrL)           # distances to group center grouped by series
    MRGrp     <- do.call("rbind", MRL)[ , "unit"]
    MRCIGrp   <- do.call("rbind", sigMRci)
    MRDF      <- data.frame(MR=MRGrp,
                            MRciLo=MRCIGrp[ , "MR ("],
                            MRciUp=MRCIGrp[ , "MR )"],
                            series=factor(levels(DF$series)))
    names(dstCtrGrp) <- NULL

    ## create data frame with added series factor
    dstCtrDF <- data.frame(dstCtr=dstCtrGrp,
                           series=factor(rep(seq_along(levels(DF$series)), nObs),
                                         labels=levels(DF$series)))

    ## confidence ellipse
    confElls <- Map(getConfEll, xyL, level=CEPlevel, dst=dstTargetGrp, conversion=conversionGrp)

    ## for axis limits
    getConfEllLims <- function(x) {
        cbind(X=c(x$ctr[1] + x$size["unit", "semi-major"],
                  x$ctr[1] - x$size["unit", "semi-major"]),
              Y=c(x$ctr[2] + x$size["unit", "semi-major"],
                  x$ctr[2] - x$size["unit", "semi-major"]))
    }

    confEllLims <- do.call("rbind", lapply(confElls, getConfEllLims))
    axisLimsX   <- c(axisLimsX, confEllLims[ , 1])
    axisLimsY   <- c(axisLimsY, confEllLims[ , 2])

    ## determine axis limits
    xLims <- range(c(DF$x, axisLimsX))
    yLims <- range(c(DF$y, axisLimsY))

    syms <- c(4, 16, 2, 1, 6, 8, 3, 5, 7, 9:13, 15, 17:25)  # data symbols
    cols <- getColors(nS)            # colors

    if(nS > length(syms)) {
        stop(paste("At most", length(syms), "series possible"))
    }

    ## infer (x,y)-coord units from conversion
    unitXY  <- paste(unique(na.omit(getUnits(conversion, first=FALSE))), collapse=", ")
    unitDst <- paste(unique(na.omit(getUnits(conversion, first=TRUE))),  collapse=", ")

    ## distance to target may be heterogeneous
    dstTargetPlot <- paste(unique(round(na.omit(dstTargetGrp))), collapse=", ")

    if(which == 1L) {
        #####-----------------------------------------------------------------------
        ## diagram: 2D-scatter plot for the (x,y)-distribution
        plot(y ~ x, data=DF, xlim=xLims, ylim=yLims, asp=1, lwd=2,
             pch=syms[unclass(DF$series)], col=cols[unclass(DF$series)],
             main=paste0("Groups with ", 100*CEPlevel, "% confidence ellipse"),
             sub=paste("distance:", dstTargetPlot, unitDst),
             xlab=paste0("X [", unitXY, "]"), ylab=paste0("Y [", unitXY, "]"))
        abline(v=0, h=0, col="lightgray")  # add point of aim

        ## add confidence ellipses and group centers
        for(i in seq(along=xyL)) {
            drawEllipse(confElls[[i]], fg=cols[i],
                        lwd=2, pch=syms[i], cex=3)
            points(res$ctr[1, i], res$ctr[2, i], pch=syms[i], col=cols[i],
                   cex=3, lwd=2)
        }

        ## add legend
        legend(x="bottomleft", legend=names(xyL), lty=NA, pch=syms[seq_len(nS)],
               col=cols[seq_len(nS)], lwd=2, pt.cex=1.5, bg=rgb(1, 1, 1, 0.6))
    }

    if(which == 2L) {
        #####-----------------------------------------------------------------------
        ## diagram: 2D-scatter plot for the (x,y)-distribution
        plot(y ~ x, data=DF, asp=1, xlim=xLims, ylim=yLims, lwd=2,
             pch=syms[unclass(DF$series)], col=cols[unclass(DF$series)],
             main="Groups w/ minimum bounding box & maximum spread",
             sub=paste("distance:", dstTargetPlot, unitDst),
             xlab=paste0("X [", unitXY, "]"), ylab=paste0("Y [", unitXY, "]"))
        abline(v=0, h=0, col="lightgray")  # add point of aim
        points(res$ctr[1, ], res$ctr[2, ], col=cols[seq_len(nS)],
               pch=syms[seq_len(nS)], lwd=2, cex=3)

        ## add bounding box and maximum group spread
        for(i in seq(along=xyL)) {
            bb <- bbs[[i]]
            ## drawBox(bb, fg=cols[i])
            drawBox2(bb, fg=cols[i])
            segments(x0=xyL[[i]][maxPDidx[1, i], 1], y0=xyL[[i]][maxPDidx[1, i], 2],
                     x1=xyL[[i]][maxPDidx[2, i], 1], y1=xyL[[i]][maxPDidx[2, i], 2],
                     col=cols[i])
        }

        ## add legend
        legend(x="bottomleft", legend=names(xyL), lty=NA, pch=syms[seq_len(nS)],
               col=cols[seq_len(nS)], lwd=2, pt.cex=1.5, bg=rgb(1, 1, 1, 0.6))
    }

    if(which == 3L) {
        #####-----------------------------------------------------------------------
        ## diagram: 2D-scatter plot for the (x,y)-distribution
        plot(y ~ x, data=DF, asp=1, xlim=xLims, ylim=yLims, lwd=2,
             pch=syms[unclass(DF$series)], col=cols[unclass(DF$series)],
             main="Groups w/ minimum enclosing circle and mean dist to center",
             sub=paste("distance:", dstTargetPlot, unitDst),
             xlab=paste0("X [", unitXY, "]"), ylab=paste0("Y [", unitXY, "]"))
        abline(v=0, h=0, col="lightgray")  # add point of aim
        points(res$ctr[1, ], res$ctr[2, ], col=cols[seq_len(nS)], pch=syms[seq_len(nS)],
               lwd=2, cex=3)

        ## add circle with mean distance to center and minimum enclosing circle
        for(i in seq(along=xyL)) {
            drawCircle(res$ctr[ , i], radius=meanDstCtr[[i]], fg=cols[i])
            drawCircle(minCircs[[i]], fg=cols[i])
        }

        ## add legend
        legend(x="bottomleft", legend=names(xyL), lty=NA, pch=syms[seq_len(nS)],
               col=cols[seq_len(nS)], lwd=2, pt.cex=1.5, bg=rgb(1, 1, 1, 0.6))
    }                                    # if(plots)

    if(which == 4L) {
        #####-------------------------------------------------------------------
        ## diagram: distances to center
        ## grouped boxplot + Rayleigh MR+CI
        op <- par(mfrow=c(1, 2))
        yLims <- c(0, max(dstCtrDF$dstCtr, MRDF$MRciUp))
        boxplot(dstCtr ~ series, data=dstCtrDF,
                main="Distance to center",
                sub=paste("distance:", dstTargetPlot, unitDst),
                xaxt="n", col=cols, ylim=yLims,
                xlab="group", ylab=paste0("distance to center [", unitXY, "]"))
        axis(side=1, at=seq_along(levels(dstCtrDF$series)),
             labels=substring(levels(dstCtrDF$series), 1, 7), las=2)

        ## Rayleigh MR+CI
        stripchart(dstCtr ~ series, data=dstCtrDF, pch=20, vert=TRUE,
                   method="jitter",
                   main=paste0("Dist2ctr w/ Rayleigh MR + ", 100*CIlevel, "% CI"),
                   sub=paste("distance:", dstTargetPlot, unitDst),
                   xaxt="n", col=adjustcolor(cols, alpha.f=0.5),
                   xlim=range(seq_along(levels(dstCtrDF$series))) + c(-0.5, 0.5),
                   ylim=yLims,
                   xlab="group", ylab=paste0("distance to center [", unitXY, "]"))
        axis(side=1, at=seq_along(levels(dstCtrDF$series)),
             labels=substring(levels(dstCtrDF$series), 1, 7), las=2)

        with(MRDF,
             points(seq_along(series), MR, pch=4, cex=2, col="black", lwd=2))
        with(MRDF,
             arrows(x0=seq_along(series), y0=MRciLo,
                    x1=seq_along(series), y1=MRciUp,
                    code=3, angle=90, length=0.1, col="black", lwd=2))
        par(op)
    }

    #####-----------------------------------------------------------------------
    ## return all the collected numerical results and tests
    return(res)
}
