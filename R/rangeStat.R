pRangeStat <-
function(q, sigma=1, nPerGroup=5, nGroups=1, stat=c("ES", "FoM", "D"),
         lower.tail=TRUE, loUp) {

    stat      <- match.arg(toupper(stat), choices=c("ES", "FOM", "D"))
    stat      <- c(ES="ES", FOM="FoM", D="D")[stat]
    nPerGroup <- as.integer(nPerGroup[1])
    nGroups   <- as.integer(nGroups[1])
    stopifnot(nPerGroup > 1L, nPerGroup <= max(shotGroups::DFdistr$n),
              nGroups   > 0L, nGroups   <= max(shotGroups::DFdistr$nGroups))
    
    sigma <- sigma[1]
    stopifnot(is.numeric(sigma), sigma > 0)

    ## available probabilities -> range is search interval for uniroot()
    allPV <- regmatches(names(shotGroups::DFdistr),
                        regexpr(paste0("^", stat, "_Q[[:digit:]]{3}$"),
                                names(shotGroups::DFdistr)))
    
    if(missing(loUp)) {
        ## use whole probability range from lookup table for search
        loUp <- range(as.numeric(sub(paste0(stat, "_Q([[:digit:]]{3})$"), "\\1" , allPV)) / 1000)   
    }

    # pp   <- numeric(length(q))               # initialize probabilities to 0
    pp   <- rep(NA_real_, length(q))         # initialize probabilities to NA
    keep <- which((q >= 0) | !is.finite(q))  # keep non-negative q, NA, NaN, -Inf, Inf

    qdf <- function(p, q, sigma, nPerGroup, nGroups, stat, lower.tail) {
        qobs <- qRangeStat(p=p, sigma=sigma, nPerGroup=nPerGroup, nGroups=nGroups,
                   stat=stat, lower.tail=lower.tail)
        # print(c(p, q, qobs, qobs-q))
        qobs - q
    }
    
    getP <- function(q, sigma, nPerGroup, nGroups, stat, loUp, lower.tail) {
        tryCatch(uniroot(qdf, interval=loUp, q=q, sigma=sigma,
                         nPerGroup=nPerGroup, nGroups=nGroups,
                         stat=stat, lower.tail=lower.tail)$root,
                 error=function(e) { return(NA_real_) })
    }
    
    pp[keep] <- unlist(Map(getP, q=q[keep], sigma=list(sigma),
                           nPerGroup=nPerGroup, nGroups=nGroups,
                           stat=stat, loUp=list(loUp),
                           lower.tail=lower.tail[1]))
    
    ## special cases that are distribution independent
    if(lower.tail) {
        pp[q == -Inf] <- 0
        pp[q ==  Inf] <- 1
    } else {
        pp[q < 0]    <- 1
        pp[q == Inf] <- 0
    }
    
    return(pp)
}

qRangeStat <-
function(p, sigma=1, nPerGroup=5, nGroups=1, stat=c("ES", "FoM", "D"),
         method=c("linear", "spline"),
         lower.tail=TRUE) {
    stat      <- match.arg(toupper(stat), choices=c("ES", "FOM", "D"))
    stat      <- c(ES="ES", FOM="FoM", D="D")[stat]
    method    <- match.arg(toupper(method), choices=c("LINEAR", "SPLINE"))
    nPerGroup <- as.integer(nPerGroup[1])
    nGroups   <- as.integer(nGroups[1])
    stopifnot(nPerGroup > 1L, nPerGroup <= max(shotGroups::DFdistr$n),
              nGroups   > 0L, nGroups   <= max(shotGroups::DFdistr$nGroups))
    
    sigma <- sigma[1]
    stopifnot(is.numeric(sigma), sigma > 0)
    
    if(!lower.tail) { p <- 1-p}

    p_var    <- paste0(stat, "_Q", sprintf("%.03d", round(1000*p)))
    qq       <- setNames(rep(NA_real_, length(p)), p_var)
    idxGroup <- shotGroups::DFdistr$nGroups == nGroups
    idxN     <- shotGroups::DFdistr$n       == nPerGroup
    haveN    <- unique(shotGroups::DFdistr$n[idxGroup])
    ## in checking whether probability is in lookup table, use 1 more decimal place
    havePV   <- paste0(stat, "_Q", sprintf("%.04d", trunc(10000*p))) %in%
        paste0(names(shotGroups::DFdistr), "0")
    # hasName(shotGroups::DFdistr, p_var)
    
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
                allQ <- shotGroups::DFdistr[idxN & idxGroup, allPV]
                ## interpolate
                qq[keep] <- splinefun(allP, shotGroups::DFdistr[idxN & idxGroup, allPV],
                                      method="monoH.FC")(p[keep])
            } else {
                ## bivariate interpolation over support grid
                grid_npq <- expand.grid(n=haveN, p=allP)
                allQ     <- vapply(haveN, function(n) {
                    qRangeStat(allP, sigma=sigma,
                               nPerGroup=n,
                               nGroups=nGroups,
                               stat=stat,
                               lower.tail=lower.tail) },
                    FUN.VALUE=numeric(length(allP)))
                grid_npq[["q"]] <- c(t(allQ))

                if(method == "LINEAR") {
                    if(requireNamespace("interp", quietly=TRUE)) {
                        warning("Quantile(s) based on bilinear interpolation for p and nPerGroup")
                        qq[keep] <- interp::interp(grid_npq[["n"]], grid_npq[["p"]], grid_npq[["q"]],
                                                   xo=rep(nPerGroup, length(p[keep])),
                                                   yo=p[keep],
                                                   linear=TRUE, extrap=FALSE, output="points")[["z"]]
                    } else {
                        warning("Install package 'interp' to enable bilinear interpolation\\n
                                 for p and nPerGroup")
                    }
                } else if(method == "SPLINE") {
                    if(requireNamespace("MBA", quietly=TRUE)) {
                        warning("Quantile(s) based on bivariate spline interpolation for p and nPerGroup")
                        surf  <- MBA::mba.surf(grid_npq, no.X=500, no.Y=500)
                        idx_x <- which.min(abs(surf$xyz.est$x - nPerGroup))
                        idx_y <- vapply(p[keep], function(pp) {
                            which.min(abs(surf$xyz.est$y - pp)) }, FUN.VALUE=integer(1))
                        qq[keep] <- surf$xyz.est$z[cbind(rep(idx_x, length(idx_y)), idx_y)]
                    } else {
                        warning("Install package 'interp' to enable bivariate spline interpolation\\n
                                 for p and nPerGroup")
                    }
                }
            }
        }
    }
    
    qq*sigma
}

## simulates range statistics for 1 group of shots with
## circular bivariate normal distribution
simRangeStat <-
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
    
    simOneRangeStat <- function() {
        rs <- vapply(integer(nGroups), function(x) {
            simRangeStat(nPerGroup, sigma) }, FUN.VALUE=numeric(3))
        rowMeans(rs)[stat]
    }
    
    unname(replicate(n, simOneRangeStat()))
}
