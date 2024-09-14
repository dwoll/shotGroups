## http://ballistipedia.com/index.php?title=Range_Statistics
getRangeStatEff <-
function(n, nGroups) {
    stopifnot(is.numeric(n),
              all(n > 1L),
              all(n     <= max(shotGroups::DFdistr[["n"]])),
              all(nGroups %in% shotGroups::DFdistr[["nGroups"]]))
    
    n        <- as.integer(n)
    nGroups  <- as.integer(nGroups)
    args     <- recycle(n, nGroups)
    n        <- args[[1]]
    nGroups  <- args[[2]]

    have_n       <- sort(unique(shotGroups::DFdistr[["n"]]))
    have_nGroups <- sort(unique(shotGroups::DFdistr[["nGroups"]]))

    get_CV <- function(i) {
        n_here  <- n[i]
        nG_here <- nGroups[i]

        ## can use lookup table for ES_CV/FoM_CV/D_CV/RS_CV or do spline interpolation
        if(n_here %in% have_n) {
            ## get index for n/nGroups combinations
            # x: have_n, y: n
            # x[match(y, x)]
            idx <- which((shotGroups::DFdistr[["n"]]       == n_here) &
                         (shotGroups::DFdistr[["nGroups"]] == nG_here))

            CV_names <- c("ES", "FoM", "D", "RS")
            data.matrix(setNames(DFdistr[idx, paste0(CV_names, "_CV")], CV_names))[1, , drop=TRUE]
        } else {
            idx     <- shotGroups::DFdistr[["nGroups"]] == nG_here
            DF_here <- shotGroups::DFdistr[idx, ]
            n_all   <- DF_here[["n"]]
            c( ES=splinefun(n_all, DF_here[["ES_CV"]],  method="monoH.FC")(n_here),
              FoM=splinefun(n_all, DF_here[["FoM_CV"]], method="monoH.FC")(n_here),
                D=splinefun(n_all, DF_here[["D_CV"]],   method="monoH.FC")(n_here),
               RS=splinefun(n_all, DF_here[["RS_CV"]],  method="monoH.FC")(n_here))
        }
    }

    ## Efficiency: 1 / (nTotal*CV_m^2)
    nTotal <- n*nGroups
    CV_m   <- vapply(seq_along(n), get_CV, numeric(4))
    Eff_m  <- 1 / (CV_m^2 %*% diag(nTotal, ncol=length(nTotal)))

    data.frame(n             =n,
               nGroups       =nGroups,
               nTotal        =nTotal,
               ES_efficiency =Eff_m["ES",  , drop=TRUE],
               FoM_efficiency=Eff_m["FoM", , drop=TRUE],
               D_efficiency  =Eff_m["D",   , drop=TRUE],
               RS_efficiency =Eff_m["RS",  , drop=TRUE])
}
