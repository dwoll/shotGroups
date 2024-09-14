## asumme Rayleigh case
## estimate CEP based on range statistics
## extreme spread / figure of merit / bounding box diagonal
## to Rayleigh sigma using lookup table from 1000000 runs for each
## combination of n*nGroups
## http://ballistipedia.com/index.php?title=Range_Statistics

range2CEP <-
function(x, stat="ES", n, nGroups, CEPlevel=0.5, CIlevel=0.95,
         collapse=TRUE, dstTarget, conversion) {
    sigma <- range2sigma(x=x, stat=stat, n=n, nGroups=nGroups,
                         CIlevel=CIlevel, collapse=FALSE,
                         dstTarget=dstTarget, conversion=conversion)

    CEP <- qRayleigh(CEPlevel, scale=sigma$sigma)
    CEP <- if(is.matrix(sigma$sigma)) {
        dim(CEP)      <- dim(sigma$sigma)
        rownames(CEP) <- rownames(sigma$sigma)
        colnames(CEP) <- gsub("sigma", "CEP", colnames(sigma$sigma))
        CEP
    } else {
        setNames(CEP, names(sigma$sigma))
    }
    
    if(hasName(sigma, "sigmaCI")) {
        CEPCI <- lapply(sigma$sigmaCI, function(y) {
            lapply(y, function(z) {
                res <- qRayleigh(CEPlevel, scale=z)
                if(is.matrix(z)) {
                    dim(res)      <- dim(z)
                    rownames(res) <- rownames(z)
                    colnames(res) <- gsub("sigma", "CEP", colnames(z))
                }
                
                res
            })
        })
        
        list(CEP=CEP, CEPCI=CEPCI)
    } else {
        list(CEP=CEP)
    }
    
    ## collapse sigmaCI list if required and possible
    if(hasName(sigma, "sigmaCI") && collapse) {
        for(i in seq_along(CEPCI)) {
            if(length(CEPCI[[i]]) == 1L) { CEPCI[[i]] <- CEPCI[[c(i, 1)]] }
        }
        
        if(length(CEPCI) == 1L) { CEPCI <- CEPCI[[1]] }
        
        list(CEP=CEP, CEPCI=CEPCI)
    } else {
        list(CEP=CEP)
    }
}
