pRangeStat <-
function(q, sigma=1, nPerGroup=5, nGroups=1, stat=c("ES", "FoM", "D", "RS"),
         lower.tail=TRUE) {

    stat      <- match.arg(toupper(stat), choices=c("ES", "FOM", "D", "RS"))
    stat      <- c(ES="ES", FOM="FoM", D="D")[stat]
    nPerGroup <- as.integer(nPerGroup[1])
    nGroups   <- as.integer(nGroups[1])
    stopifnot(nPerGroup > 1L, nPerGroup <= max(shotGroups::DFdistr$n),
              nGroups   > 0L, nGroups   <= max(shotGroups::DFdistr$nGroups))
    
    sigma <- sigma[1]
    stopifnot(is.numeric(sigma), sigma > 0)

    # pp   <- numeric(length(q))               # initialize probabilities to 0
    pp   <- rep(NA_real_, length(q))         # initialize probabilities to NA
    keep <- which((q >= 0) | !is.finite(q))  # keep non-negative q, NA, NaN, -Inf, Inf
    
    if(lower.tail) {
        ## TODO
        ## special cases not caught so far
        pp[q == -Inf] <- 0
        pp[q ==  Inf] <- 1
    } else {
        ## TODO
        ## special cases not caught so far
        pp[q < 0]    <- 1
        pp[q == Inf] <- 0
    }
    
    return(pp)
}

## TODO
## bivariate *spline* interpolation for nPerGroup & p over regular grid
qRangeStat <-
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
                    qRangeStat(allP, sigma=sigma,
                               nPerGroup=n,
                               nGroups=nGroups,
                               stat=stat,
                               lower.tail=lower.tail) },
                    FUN.VALUE=numeric(length(allP)))
                grid_npq[["q"]] <- c(t(allQ))
                # fit_gam <- mgcv::gam(q ~ te(n, p, bs="cr", fx=TRUE), data=grid_npq)
                # fit_gam <- mgcv::gam(q ~ ti(n, fx=TRUE) + ti(p, fx=TRUE) + ti(n, p, fx=TRUE), data=grid_npq)
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
getRangeStat <-
function(nPerGroup, sigma) {
    xy  <- matrix(rnorm(2*nPerGroup, mean=0, sd=sigma), ncol=2)
    H   <- chull(xy)       # convex hull indices (vertices ordered clockwise)
    ES  <- max(dist(xy[H, ], method="euclidean"))  # extreme spread
    bbW <- abs(diff(range(xy[ , 1])))
    bbH <- abs(diff(range(xy[ , 2])))
    FoM <- (bbW + bbH) / 2                         # figure of merit
    D   <- sqrt(bbW^2 + bbH^2)                     # diagonal
    c(ES=ES, FoM=FoM, D=D)
}

## random deviates for range statistics
rRangeStat <-
function(n, sigma=1, nPerGroup=5, nGroups=1, stat=c("ES", "FoM", "D")) {
    stat      <- match.arg(toupper(stat), choices=c("ES", "FOM", "D"))
    stat      <- c(ES="ES", FOM="FoM", D="D")[unique(stat)]
    nPerGroup <- as.integer(nPerGroup[1])
    nGroups   <- as.integer(nGroups[1])
    sigma     <- sigma[1]
    
    n <- if(length(n) > 1L) { length(n) } else { n }
    stopifnot(is.numeric(sigma), sigma > 0)
    stopifnot(nPerGroup > 1L, nPerGroup <= max(shotGroups::DFdistr$n),
              nGroups   > 0L, nGroups   <= max(shotGroups::DFdistr$nGroups))
    
    getOneRangeStat <- function() {
        rs <- vapply(integer(nGroups), function(x) {
            getRangeStat(nPerGroup, sigma) }, FUN.VALUE=numeric(3))
        rowMeans(rs)[stat]
    }
    
    unname(replicate(n, getOneRangeStat()))
}
