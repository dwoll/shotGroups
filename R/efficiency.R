## asumme Rayleigh case
## http://ballistipedia.com/index.php?title=Range_Statistics

efficiency <-
function(n, nGroups, CIlevel=0.95, CIwidth,
         stat=c("Rayleigh", "ES", "FoM", "D")) {
    stopifnot(is.numeric(n),
              is.numeric(CIlevel),
              all(n > 1L),
              all(n <= max(shotGroups::DFdistr[["n"]])),
              all(CIlevel > 0))

    if(!missing(nGroups)) {
        stopifnot(is.numeric(nGroups),
                  nGroups > 0)
        
        nGroups <- as.integer(nGroups)
        argsL   <- recycle(n, nGroups)
        n       <- argsL[[1]]
        nGroups <- argsL[[2]]
        nGroups_use <- nGroups
    } else {
        nGroups_use <- rep(1L, length(n))
    }

    stat  <- match.arg(toupper(stat),
                       choices=c("RAYLEIGH", "ES", "FOM", "D"),
                       several.ok=FALSE)
    
    ## number of shots per group and number of groups
    ## that is tabulated in DFdistr
    have_n       <- sort(unique(shotGroups::DFdistr[["n"]]))
    have_nGroups <- sort(unique(shotGroups::DFdistr[["nGroups"]]))
    
    CV_map  <- c("RAYLEIGH"="RS", "ES"="ES", "FOM"="FoM", "D"="D")
    CV_name <- unname(CV_map[stat])
    
    ## check if CIlevel is given in percent
    if(CIlevel >= 1) {
        while(CIlevel >= 1) { CIlevel <- CIlevel / 100 }
        warning(c("CIlevel must be in (0,1) and was set to ", CIlevel))
    }

    CIlevel <- round(CIlevel[1], digits=2)
    alpha   <- 1 - CIlevel
    z       <- qnorm(1-(alpha/2), mean=0, sd=1)

    ## TODO: do this on a case-by-case basis
    ## get coefficient of variation
    get_CV <- function(i) {
        n_here  <- n[i]
        nG_here <- nGroups_use[i]

        if(n_here %in% have_n) {
            ## can use lookup table for ES_CV/FoM_CV/D_CV/RS_CV
            ## get index for n/nGroups combination
            idx <- which((shotGroups::DFdistr[["n"]]       == n_here) &
                         (shotGroups::DFdistr[["nGroups"]] == nG_here))
            
            CV_names <- c("ES", "FoM", "D", "RS")
            data.matrix(setNames(DFdistr[idx, paste0(CV_names, "_CV")], CV_names))[1, , drop=TRUE]
        } else {
            ## do spline interpolation
            idx     <- shotGroups::DFdistr[["nGroups"]] == nG_here
            DF_here <- shotGroups::DFdistr[idx, ]
            n_all   <- DF_here[["n"]]
            c( ES =splinefun(n_all, DF_here[["ES_CV"]],  method="monoH.FC")(n_here),
               FoM=splinefun(n_all, DF_here[["FoM_CV"]], method="monoH.FC")(n_here),
               D  =splinefun(n_all, DF_here[["D_CV"]],   method="monoH.FC")(n_here),
               RS =splinefun(n_all, DF_here[["RS_CV"]],  method="monoH.FC")(n_here))
        }
    }
    
    ## coefficient of variation
    CV_m <- vapply(seq_along(n), get_CV, numeric(4))
    
    if(missing(nGroups) && !missing(CIwidth)) {
        ## nGroups is requested, CI width is given
        CIwidth <- CIwidth[1]
        stopifnot(is.numeric(CIwidth),
                  CIwidth > 0)

        ## check if CIwidth is given in percent
        if(CIwidth >= 1) {
            while(CIwidth >= 1) { CIwidth <- CIwidth / 100 }
            warning(c("CIwidth must be in (0,1) and was set to ", CIwidth))
        }

        E              <- CIwidth/2
        nGroupsReq     <- (z*CV_m[CV_name, ]/E)^2
        nGroupsReqCeil <- ceiling(nGroupsReq)
        data.frame(n             =n,
                   nGroupsReq    =nGroupsReq,
                   nGroupsReqCeil=nGroupsReqCeil,
                   nShotsReq     =nGroupsReq    *n,
                   nShotsReqCeil =nGroupsReqCeil*n,
                   CIlevel       =CIlevel,
                   CIwidth       =CIwidth)
    } else if(!missing(nGroups) && missing(CIwidth)) {
        ## nGroups is given, CI width is requested
        E <- z*CV_m[CV_name, ]/sqrt(nGroups)
        data.frame(n      =n,
                   nGroups=nGroups,
                   nShots =nGroups*n,
                   CIlevel=CIlevel,
                   CIwidth=2*E)

    } else {
        stop("One of nGroups or CIwidth must be supplied (but not both)")
    }
}
