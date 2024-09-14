## asumme Rayleigh case
## get range statistic CIs
## using lookup table from 1000000 runs for each
## combination of n*nGroups
## http://ballistipedia.com/index.php?title=Range_Statistics

getRangeStatCI <-
function(x, stat="ES", n, nGroups, CIlevel=0.95, collapse=TRUE,
         dstTarget, conversion) {
    n       <- as.integer(n[1])
    nGroups <- as.integer(nGroups[1])
    stopifnot(all(x > 0),
              n       > 1L, n       <= max(shotGroups::DFdistr[["n"]]),
              nGroups > 0L, nGroups <= max(shotGroups::DFdistr[["nGroups"]]),
              CIlevel > 0)
    
    stat <- match.arg(toupper(stat),
                      choices=c("ES", "FOM", "D"),
                      several.ok=TRUE)
    
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
    idxGroup <- which(shotGroups::DFdistr[["nGroups"]] == nGroups)
    haveN    <- unique(shotGroups::DFdistr[["n"]][idxGroup])
    haveCI   <- c(0.5, 0.8, 0.9, 0.95, 0.99)

    ## Rayleigh sigma: use lookup table or monotone spline interpolation
    ## CI: use lookup table or Grubbs-Patnaik chi^2 approximation
    if(n %in% haveN) {
        ## can use lookup table for sigma estimate
        idx <- which((shotGroups::DFdistr[["n"]]       == n) &
                     (shotGroups::DFdistr[["nGroups"]] == nGroups))
        
        M <- data.matrix(shotGroups::DFdistr[idx, paste0(stat, "_M"), drop=FALSE])
        M <- setNames(c(M), stat)

        ## for Grubbs-Patnaik ES-CI approximation
        if(!(CIlevel %in% haveCI)) {
            ES_V   <- shotGroups::DFdistr[["ES_V"]][idx]
            ESSQ_M <- shotGroups::DFdistr[["ESSQ_M"]][idx]
            ESSQ_V <- shotGroups::DFdistr[["ESSQ_V"]][idx]
            # ES_SKEW <- shotGroups::DFdistr[["ES_SKEW"]][idx]
            # ES_KURT <- shotGroups::DFdistr[["ES_KURT"]][idx]
        }
    } else {
        ## spline interpolation for sigma estimate - M is monotonically increasing in n
        warning("Rayleigh sigma estimate based on monotone spline interpolation for n")

        ## only interpolate for requested number of groups
        M <- setNames(numeric(length(x)), stat)
        M[names(M) == "ES"]  <- splinefun(haveN, shotGroups::DFdistr[["ES_M"]][idxGroup],  method="monoH.FC")(n)
        M[names(M) == "FoM"] <- splinefun(haveN, shotGroups::DFdistr[["FoM_M"]][idxGroup], method="monoH.FC")(n)
        M[names(M) == "D"]   <- splinefun(haveN, shotGroups::DFdistr[["D_M"]][idxGroup],   method="monoH.FC")(n)

        ## spline interpolation for Grubbs-Patnaik ES-CI approximation
        ## M is monotonically increasing, but V, SKEW, KURT are not
        if(!(CIlevel %in% haveCI)) {
            ES_V   <- splinefun(haveN, shotGroups::DFdistr[["ES_V"]][idxGroup],    method="fmm")(n)
            ESSQ_M <- splinefun(haveN, shotGroups::DFdistr[["ESSQ_M"]][idxGroup],  method="monoH.FC")(n)
            ESSQ_V <- splinefun(haveN, shotGroups::DFdistr[["ESSQ_V"]][idxGroup],  method="fmm")(n)
            # ES_SKEW <- splinefun(haveN, shotGroups::DFdistr[["ES_SKEW"]][idxGroup], method="fmm")(n)
            # ES_KURT <- splinefun(haveN, shotGroups::DFdistr[["ES_KURT"]][idxGroup], method="fmm")(n)
        }
    }

    ## M is for the sigma=1 case, range statistics are proportional to sigma
    ## normalize CI bounds to 1 via division by mean or median
    ## then multiply by provided point estimate
    ## need extreme spread for sigma CI
    ## 1st: table entries
    mES  <- setNames(M[names(x) == "ES"],  NULL)
    mFoM <- setNames(M[names(x) == "FoM"], NULL)
    mD   <- setNames(M[names(x) == "D"],   NULL)
    
    ## 2nd: provided point estimates
    xES  <- setNames(x[names(x) == "ES"],  NULL)
    xFoM <- setNames(x[names(x) == "FoM"], NULL)
    xD   <- setNames(x[names(x) == "D"],   NULL)
    if(CIlevel %in% haveCI) {
        CIlo <- sprintf("%04.1f", round(   (alpha/2) *100, digits=1))
        CIup <- sprintf("%04.1f", round((1-(alpha/2))*100, digits=1))
        if(n %in% haveN) {
            ES_CIlo  <-  xES*shotGroups::DFdistr[[sub("\\.", "", paste0("ES_Q",  CIlo))]][idx] / mES
            ES_CIup  <-  xES*shotGroups::DFdistr[[sub("\\.", "", paste0("ES_Q",  CIup))]][idx] / mES
            FoM_CIlo <- xFoM*shotGroups::DFdistr[[sub("\\.", "", paste0("FoM_Q", CIlo))]][idx] / mFoM
            FoM_CIup <- xFoM*shotGroups::DFdistr[[sub("\\.", "", paste0("FoM_Q", CIup))]][idx] / mFoM
            D_CIlo   <-   xD*shotGroups::DFdistr[[sub("\\.", "", paste0("D_Q",   CIlo))]][idx] / mD
            D_CIup   <-   xD*shotGroups::DFdistr[[sub("\\.", "", paste0("D_Q",   CIup))]][idx] / mD
        } else {
            ES_CIlo  <-  xES*splinefun(haveN, shotGroups::DFdistr[[sub("\\.", "", paste0("ES_Q",  CIlo))]][idxGroup],
                                       method="fmm")(n) / mES
            ES_CIup  <-  xES*splinefun(haveN, shotGroups::DFdistr[[sub("\\.", "", paste0("ES_Q",  CIup))]][idxGroup],
                                       method="fmm")(n) / mES
            FoM_CIlo <- xFoM*splinefun(haveN, shotGroups::DFdistr[[sub("\\.", "", paste0("FoM_Q", CIlo))]][idxGroup],
                                       method="fmm")(n) / mFoM
            FoM_CIup <- xFoM*splinefun(haveN, shotGroups::DFdistr[[sub("\\.", "", paste0("FoM_Q", CIup))]][idxGroup],
                                       method="fmm")(n) / mFoM
            D_CIlo   <-   xD*splinefun(haveN, shotGroups::DFdistr[[sub("\\.", "", paste0("D_Q",   CIlo))]][idxGroup],
                                       method="fmm")(n) / mD
            D_CIup   <-   xD*splinefun(haveN, shotGroups::DFdistr[[sub("\\.", "", paste0("D_Q",   CIup))]][idxGroup],
                                       method="fmm")(n) / mD
        }
    } else {
        if("ES" %in% stat) {
            warning("CI estimate based on Grubbs-Patnaik chi^2 approximation")
            ## Patnaik chi-approximation
            m <- ESSQ_M
            v <- ESSQ_V
            # m <- ES_M^2 + ES_V
            # v <- ES_KURT*ES_V^2 + 4*ES_SKEW*sqrt(ES_V)^3*ES_M + 4*ES_V*ES_M^2 - ES_V^2
            ES_CIlo  <- xES*qChisqGrubbs(   alpha/2,  m=m, v=v, n=2*m^2/v) / mES
            ES_CIup  <- xES*qChisqGrubbs(1-(alpha/2), m=m, v=v, n=2*m^2/v) / mES
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
    x_out  <- makeMOA(x, dst=dstTarget, conversion=conversion)
    ES_CI  <- lapply(seq_along(ES_CIlo),  function(i) {
        makeMOA(c("ES ("=ES_CIlo[i], "ES )" =ES_CIup[i]),
                dst=dstTarget, conversion=conversion) })
    ES_CI  <- if(length(ES_CI) > 0L) {
        setNames(ES_CI,  paste0("ES_",  round(xES, digits=2)))
    } else { NULL }

    FoM_CI <- lapply(seq_along(FoM_CIlo), function(i) {
        makeMOA(c("FoM ("=FoM_CIlo[i], "FoM )"=FoM_CIup[i]),
                dst=dstTarget, conversion=conversion) })
    FoM_CI <- if(length(FoM_CI) > 0L) {
        setNames(FoM_CI, paste0("FoM_", round(xFoM, digits=2)))
    } else { NULL }

    D_CI   <- lapply(seq_along(D_CIlo),   function(i) {
        makeMOA(c("D ("=D_CIlo[i], "D )"  =D_CIup[i]),
                dst=dstTarget, conversion=conversion) })
    D_CI   <- if(length(D_CI) > 0L) {
        setNames(D_CI,   paste0("D_",   round(xD,   digits=2)))
    } else { NULL }

    ## weed out non existing CIs
    CI <- Filter(Negate(is.null), list(ES=ES_CI, FoM=FoM_CI, D=D_CI))

    ## collapse CI list if required and possible
    if(collapse) {
        for(i in seq_along(CI)) {
            if(length(CI[[i]]) == 1L) { CI[[i]] <- CI[[c(i, 1)]] }
        }

        if(length(CI) == 1L) { CI <- CI[[1]] }
    }

    ## sigmaCI might be empty list when no ES is given
    Filter(function(l) { !is.null(l) && (length(l) > 0L) },
           list(range_stat=x_out, CI=CI))
}
