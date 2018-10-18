## asumme Rayleigh case
## convert extreme spread / figure of merit / bounding box diagonal
## to Rayleigh sigma using lookup table from 1000000 runs for each
## combination of n*nGroups
## http://ballistipedia.com/index.php?title=Range_Statistics
range2sigma <-
function(x, stat="ES", n=5, nGroups=1, CIlevel=0.95, collapse=TRUE,
         dstTarget, conversion) {
    n       <- as.integer(n[1])
    nGroups <- as.integer(nGroups[1])
    stopifnot(all(x > 0),
              n       > 1L, n       <= max(shotGroups::DFdistr$n),
              nGroups > 0L, nGroups <= max(shotGroups::DFdistr$nGroups),
              CIlevel > 0)
    stat <- match.arg(toupper(stat), choices=c("ES", "FOM", "D"), several.ok=TRUE)
    argL <- recycle(x, stat)
    x    <- argL[[1]]
    stat <- c(ES="ES", FOM="FoM", D="D")[argL[[2]]]
    x    <- setNames(x, stat)

    ## check if CIlevel is given in percent
    if(CIlevel >= 1) {
        while(CIlevel >= 1) { CIlevel <- CIlevel / 100 }
        warning(c("CIlevel must be in (0,1) and was set to ", CIlevel))
    }

    dstTarget <- if(missing(dstTarget)    ||
                    all(is.na(dstTarget)) ||
                    (length(unique(dstTarget)) > 1L)) {
        NA_real_
    } else {
        unique(dstTarget)
    }
    
    conversion <- if(missing(conversion)    ||
                     all(is.na(conversion)) ||
                     (length(unique(conversion)) > 1L)) {
        NA_character_
    } else {
        unique(conversion)
    }
    
    alpha    <- 1 - CIlevel
    idxGroup <- which(shotGroups::DFdistr$nGroups == nGroups)
    haveN    <- unique(shotGroups::DFdistr$n[idxGroup])
    haveCI   <- c(0.5, 0.8, 0.9, 0.95, 0.99)

    ## Rayleigh sigma: use lookup table or monotone spline interpolation
    ## CI: use lookup table or Grubbs-Patnaik chi^2 approximation
    if(n %in% haveN) {
        ## can use lookup table for sigma estimate
        idx <- which((shotGroups::DFdistr$n       == n) &
                     (shotGroups::DFdistr$nGroups == nGroups))
        M <- data.matrix(DFdistr[idx, paste0(stat, "_M"), drop=FALSE])
        M <- setNames(c(M), stat)

        ## for Grubbs-Patnaik ES-CI approximation
        if(!(CIlevel %in% haveCI)) {
            ES_V    <- shotGroups::DFdistr$ES_V[idx]
            ESSQ_M  <- shotGroups::DFdistr$ESSQ_M[idx]
            ESSQ_V  <- shotGroups::DFdistr$ESSQ_V[idx]
            ES_SKEW <- shotGroups::DFdistr$ES_SKEW[idx]
            ES_KURT <- shotGroups::DFdistr$ES_KURT[idx]
        }
    } else {
        ## spline interpolation for sigma estimate - M is monotonically increasing in n
        warning("Rayleigh sigma estimate based on monotone spline interpolation for n")

        ## only interpolate for requested number of groups
        M <- setNames(numeric(length(x)), stat)
        M[names(M) == "ES"]  <- splinefun(haveN, shotGroups::DFdistr$ES_M[idxGroup],  method="monoH.FC")(n)
        M[names(M) == "FOM"] <- splinefun(haveN, shotGroups::DFdistr$FoM_M[idxGroup], method="monoH.FC")(n)
        M[names(M) == "D"]   <- splinefun(haveN, shotGroups::DFdistr$D_M[idxGroup],   method="monoH.FC")(n)

        ## spline interpolation for Grubbs-Patnaik ES-CI approximation
        ## M is monotonically increasing, but V, SKEW, KURT are not
        if(!(CIlevel %in% haveCI)) {
            ES_V    <- splinefun(haveN, shotGroups::DFdistr$ES_V[idxGroup],    method="fmm")(n)
            ESSQ_M  <- splinefun(haveN, shotGroups::DFdistr$ESSQ_M[idxGroup],  method="monoH.FC")(n)
            ESSQ_V  <- splinefun(haveN, shotGroups::DFdistr$ESSQ_V[idxGroup],  method="fmm")(n)
            #ES_SKEW <- splinefun(haveN, shotGroups::DFdistr$ES_SKEW[idxGroup], method="fmm")(n)
            #ES_KURT <- splinefun(haveN, shotGroups::DFdistr$ES_KURT[idxGroup], method="fmm")(n)
        }
    }

    ## Rayleigh sigma estimate
    sigma <- x/M

    ## need extreme spread for sigma CI
    xES  <- setNames(x[names(x) == "ES"],  NULL)
    xFoM <- setNames(x[names(x) == "FOM"], NULL)
    xD   <- setNames(x[names(x) == "D"],   NULL)
    if(CIlevel %in% haveCI) {
        CIlo <- sprintf("%04.1f", round((1-(alpha/2))*100, digits=1))
        CIup <- sprintf("%04.1f", round(   (alpha/2) *100, digits=1))
        if(n %in% haveN) {
            ES_CIlo  <-  xES/shotGroups::DFdistr[[sub("\\.", "", paste0("ES_Q",  CIlo))]][idx]
            ES_CIup  <-  xES/shotGroups::DFdistr[[sub("\\.", "", paste0("ES_Q",  CIup))]][idx]
            FoM_CIlo <- xFoM/shotGroups::DFdistr[[sub("\\.", "", paste0("FoM_Q", CIlo))]][idx]
            FoM_CIup <- xFoM/shotGroups::DFdistr[[sub("\\.", "", paste0("FoM_Q", CIup))]][idx]
            D_CIlo   <-   xD/shotGroups::DFdistr[[sub("\\.", "", paste0("D_Q",   CIlo))]][idx]
            D_CIup   <-   xD/shotGroups::DFdistr[[sub("\\.", "", paste0("D_Q",   CIup))]][idx]
        } else {
            ES_CIlo  <-  xES/splinefun(haveN, shotGroups::DFdistr[[sub("\\.", "", paste0("ES_Q",  CIlo))]][idxGroup],
                                       method="fmm")(n)
            ES_CIup  <-  xES/splinefun(haveN, shotGroups::DFdistr[[sub("\\.", "", paste0("ES_Q",  CIup))]][idxGroup],
                                       method="fmm")(n)
            FoM_CIlo <- xFoM/splinefun(haveN, shotGroups::DFdistr[[sub("\\.", "", paste0("FoM_Q", CIlo))]][idxGroup],
                                       method="fmm")(n)
            FoM_CIup <- xFoM/splinefun(haveN, shotGroups::DFdistr[[sub("\\.", "", paste0("FoM_Q", CIup))]][idxGroup],
                                       method="fmm")(n)
            D_CIlo   <-   xD/splinefun(haveN, shotGroups::DFdistr[[sub("\\.", "", paste0("D_Q",   CIlo))]][idxGroup],
                                       method="fmm")(n)
            D_CIup   <-   xD/splinefun(haveN, shotGroups::DFdistr[[sub("\\.", "", paste0("D_Q",   CIup))]][idxGroup],
                                       method="fmm")(n)
        }
    } else {
        if("ES" %in% stat) {
            warning("CI estimate based on Grubbs-Patnaik chi^2 approximation")
            ES_M <- setNames(M[names(M) %in% "ES"], NULL)
            ## Patnaik chi-approximation
            m <- ESSQ_M
            v <- ESSQ_V
            # m <- ES_M^2 + ES_V
            # v <- ES_KURT*ES_V^2 + 4*ES_SKEW*sqrt(ES_V)^3*ES_M + 4*ES_V*ES_M^2 - ES_V^2
            ES_CIlo  <- xES/qChisqGrubbs(1-(alpha/2), m=m, v=v, n=2*m^2/v)
            ES_CIup  <- xES/qChisqGrubbs(   alpha/2,  m=m, v=v, n=2*m^2/v)
            FoM_CIlo <- NULL
            FoM_CIup <- NULL
            D_CIlo   <- NULL
            D_CIup   <- NULL
        } else {
            warning("CI from Grubbs-Patnaik approximation requires extreme spread")
            ES_CIlo  <- NULL
            ES_CIup  <- NULL
            FoM_CIlo <- NULL
            FoM_CIup <- NULL
            D_CIlo   <- NULL
            D_CIup   <- NULL
        }
    }

    ## convert CIs to MOA
    sigma <- makeMOA(sigma, dst=dstTarget, conversion=conversion)
    if(is.matrix(sigma)) {
        colnames(sigma) <- paste0(colnames(sigma), "_",  round(xES, digits=2))
    }
    
    sigmaESCI  <- lapply(seq_along(ES_CIlo),  function(i) {
        makeMOA(c("sigma ("=ES_CIlo[i], "sigma )" =ES_CIup[i]),
                dst=dstTarget, conversion=conversion) })
    sigmaESCI  <- if(length(sigmaESCI) > 0L) {
        setNames(sigmaESCI,  paste0("ES_",  round(xES, digits=2)))
    } else { NULL }

    sigmaFoMCI <- lapply(seq_along(FoM_CIlo), function(i) {
        makeMOA(c("sigma ("=FoM_CIlo[i], "sigma )"=FoM_CIup[i]),
                dst=dstTarget, conversion=conversion) })
    sigmaFoMCI <- if(length(sigmaFoMCI) > 0L) {
        setNames(sigmaFoMCI, paste0("FoM_", round(xFoM, digits=2)))
    } else { NULL }

    sigmaDCI   <- lapply(seq_along(D_CIlo),   function(i) {
        makeMOA(c("sigma ("=D_CIlo[i], "sigma )"  =D_CIup[i]),
                dst=dstTarget, conversion=conversion) })
    sigmaDCI   <- if(length(sigmaDCI) > 0L) {
        setNames(sigmaDCI,   paste0("D_",   round(xD,   digits=2)))
    } else { NULL }

    ## weed out non existing CIs
    sigmaCI <- Filter(Negate(is.null), list(ES=sigmaESCI, FoM=sigmaFoMCI, D=sigmaDCI))

    ## collapse sigmaCI list if required and possible
    if(collapse) {
        for(i in seq_along(sigmaCI)) {
            if(length(sigmaCI[[i]]) == 1L) { sigmaCI[[i]] <- sigmaCI[[c(i, 1)]] }
        }

        if(length(sigmaCI) == 1L) { sigmaCI <- sigmaCI[[1]] }
    }

    ## sigmaCI might be empty list when no ES is given
    Filter(function(l) { !is.null(l) && (length(l) > 0L) },
           list(sigma=sigma, sigmaCI=sigmaCI))
}

## TODO
## interpolate p
qRange <-
function(p, sigma=1, nPerGroup=5, nGroups=1, stat=c("ES", "FoM", "D", "RS"),
         lower.tail=TRUE) {
    stat      <- match.arg(toupper(stat), choices=c("ES", "FOM", "D", "RS"))
    stat      <- c(ES="ES", FOM="FoM", D="D")[stat]
    nPerGroup <- as.integer(nPerGroup[1])
    nGroups   <- as.integer(nGroups[1])
    stopifnot(nPerGroup > 1L, nPerGroup <= max(shotGroups::DFdistr$n),
              nGroups   > 0L, nGroups   <= max(shotGroups::DFdistr$nGroups))

    sigma <- sigma[1]
    stopifnot(is.numeric(sigma), sigma > 0)

    p_var <- if(lower.tail) {
        paste0(stat, "_Q", sprintf("%.03d", 1000*p))
    } else {
        paste0(stat, "_Q", sprintf("%.03d", 1000*(1-p)))
    }
    qq       <- setNames(rep(NA_real_, length(p)), p_var)
    idxGroup <- shotGroups::DFdistr$nGroups == nGroups
    idxN     <- shotGroups::DFdistr$n       == nPerGroup
    haveN    <- unique(shotGroups::DFdistr$n[idxGroup])
    havePV   <- hasName(shotGroups::DFdistr, p_var)

    if(sum(idxGroup) < 1L) {
        warning(paste0("Lookup table does not have quantile(s) for nGroups=", nGroups))
    } else {
        if(all(havePV)) {
            ## all probabilities available
            qq <- if(nPerGroup %in% haveN) {
                ## nPerGroup available
                c(data.matrix(shotGroups::DFdistr[idxN & idxGroup, p_var[havePV], drop=FALSE]))
            } else {
                ## interpolate nPerGroup
                warning("Quantile(s) based on monotone spline interpolation for nPerGroup")
                vapply(p_var[havePV], function(pvo) {
                    splinefun(haveN, shotGroups::DFdistr[idxGroup, pvo], method="monoH.FC")(nPerGroup) },
                    FUN.VALUE=numeric(1))
            }
        } else {
            ## interpolate p, but only within available min/max prob
            ## available probabilities
            allPV <- regmatches(names(shotGroups::DFdistr),
                                regexpr(paste0("^", stat, "_Q[[:digit:]]{3}$"),
                                        names(shotGroups::DFdistr)))
            allP <- as.numeric(sub(paste0(stat, "_Q([[:digit:]]{3})$"), "\\1" , allPV)) / 1000
            ## make sure probabilities are sorted
            allPV <- allPV[order(allP)]
            allP  <- allP[order(allP)]
            ## check range of p is within limits
            p_mm  <- range(allP)
            keep  <- (p >= p_mm[1]) & (p <= p_mm[2])
            if(nPerGroup %in% haveN) {
                warning("Quantile(s) based on monotone spline interpolation for p")
                ## nPerGroup available
                ## get all quantiles in correct order
                allQ <- DFdistr[idxN & idxGroup, allPV]
                ## interpolate
                qq[keep] <- splinefun(allP, DFdistr[idxN & idxGroup, allPV],
                                      method="monoH.FC")(p[keep])
            } else if(requireNamespace("akima", quietly=TRUE)) {
                warning("Quantile(s) based on bilinear interpolation for p and nPerGroup")
            # } else if(requireNamespace("mgcv", quietly=TRUE)) {
            #     warning("Quantile(s) based on bivariate spline interpolation for p and nPerGroup")
                grid_npq <- expand.grid(n=haveN, p=allP)
                allQ     <- vapply(haveN, function(n) {
                                   qRange(allP, sigma=sigma,
                                          nPerGroup=n,
                                          nGroups=nGroups,
                                          stat=stat,
                                          lower.tail=lower.tail) },
                                   FUN.VALUE=numeric(length(allP)))
                grid_npq[["q"]] <- c(t(allQ))
                # fit_gam <- mgcv::gam(q ~ te(n, p, bs="cr", fx=TRUE), data=grid_npq)
                # mgcv::vis.gam(fit_gam)
                # qq[keep] <- mgcv::predict.gam(fit_gam,
                #                               newdata=data.frame(n=nPerGroup,
                #                                                  p=p[keep]))
                qq[keep] <- akima::interp(grid_npq[["n"]], grid_npq[["p"]], grid_npq[["q"]],
                                          xo=nPerGroup, yo=p[keep],
                                          linear=TRUE, extrap=FALSE)[["z"]]
            } else {
                warning("Install package 'akima' to enable bivariate interpolation\\n
                         for p and nPerGroup")
            }
        }
    }
    
    qq*sigma
}

## simulates range statistics for 1 group of shots with
## circular bivariate normal distribution
getRange <- function(nPerGroup, sigma) {
    xy  <- matrix(rnorm(2*nPerGroup, mean=0, sd=sigma), ncol=2)
    H   <- chull(xy)       # convex hull indices (vertices ordered clockwise)
    ES  <- max(dist(xy[H, ], method="euclidean"))  # extreme spread
    bbW <- abs(diff(range(xy[ , 1])))
    bbH <- abs(diff(range(xy[ , 2])))
    FoM <- (bbW + bbH) / 2                         # figure of merit
    D   <- sqrt(bbW^2 + bbH^2)                     # diagonal
    c(ES=ES, FoM=FoM, D=D)
}

rRange <- function(n, sigma=1, nPerGroup=5, nGroups=1, stat=c("ES", "FoM", "D")) {
    stat      <- match.arg(toupper(stat), choices=c("ES", "FOM", "D"))
    stat      <- c(ES="ES", FOM="FoM", D="D")[unique(stat)]
    nPerGroup <- as.integer(nPerGroup[1])
    nGroups   <- as.integer(nGroups[1])
    sigma     <- sigma[1]

    n <- if(length(n) > 1L) { length(n) } else { n }
    stopifnot(is.numeric(sigma), sigma > 0)
    stopifnot(nPerGroup > 1L, nPerGroup <= max(shotGroups::DFdistr$n),
              nGroups   > 0L, nGroups   <= max(shotGroups::DFdistr$nGroups))

    getOneRange <- function() {
        rs <- vapply(integer(nGroups), function(x) { getRange(nPerGroup, sigma) },
                     FUN.VALUE=numeric(3))
        rowMeans(rs)[stat]
    }
    
    unname(replicate(n, getOneRange()))
}
