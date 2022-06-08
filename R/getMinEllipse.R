## getMinEllipse() requires convex hull without collinear points
## https://stackoverflow.com/a/1768440

## minimal enclosing ellipse
getMinEllipse <-
function(xy, tol=0.001, max_iter=1000) {
    UseMethod("getMinEllipse")
}

getMinEllipse.data.frame <-
function(xy, tol=0.001, max_iter=1000) {
    xy <- getXYmat(xy, xyTopLeft=FALSE, relPOA=FALSE)
    NextMethod("getMinEllipse")
}

getMinEllipse.default <-
function(xy, tol=0.001, max_iter=1000) {
    if(!is.matrix(xy))  { stop("xy must be a matrix") }
    if(!is.numeric(xy)) { stop("xy must be numeric") }
    if(nrow(xy) < 2L)   { stop("There must be at least two points") }
    # if(!(ncol(xy) %in% c(2L, 3L))) { stop("xy must have two or three columns") }
    if(ncol(xy) != 2L)  { stop("xy must have two columns") } # 3D untested
    
    hPts <- if(ncol(xy) == 2L) {
        H <- chull(xy)     # convex hull indices (vertices ordered clockwise)
        t(xy[H, ])         # points that make up the convex hull
    } else {
        t(xy)
    }

    p    <- nrow(hPts) # dimension
    N    <- ncol(hPts)
    Q    <- rbind(hPts, rep(1, N))
    iter <- 1
    err  <- 1
    u    <- rep(1/N, N)

    # Khachiyan's algorithm
    while((err > tol) && (iter < max_iter)) {
        X         <- Q %*% diag(u) %*% t(Q)
        m         <- diag(t(Q) %*% solve(X) %*% Q)
        maximum   <- max(m)
        j         <- which.max(m)
        step_size <- (maximum - p-1) / ((p+1)*(maximum-1))
        new_u     <- (1 - step_size)*u
        new_u[j]  <- new_u[j] + step_size
        err       <- sqrt(sum((new_u-u)^2))
        iter     <- iter + 1
        u         <- new_u
    }
    
    if(iter >= max_iter) {
        warning(paste("Maximum number of iterations reached. Error is still:", err))
    }
    
    ctr  <- hPts %*% u
    E    <- (1/p) * solve((hPts %*% diag(u) %*% t(hPts)) - tcrossprod(ctr))
    S    <- solve(E)
    Seig <- eigen(S)
    e    <- Seig$vectors[ , 1]
    eUp  <- e * sign(e[2])                  # rotate upwards 180 deg if necessary
    lens <- sqrt(Seig$values)
    deg  <- atan2(eUp[2], eUp[1])*180 / pi  # angle in degrees

    ## radii confidence ellipse
    size <- lens
    # size <- makeMOA(lens, dst=dstTarget, conversion=conversion)
    # colnames(size) <- if(p == 2L) {
    #     c("semi-major", "semi-minor")
    # } else {
    #     paste("semi-axis", seq_len(p), sep="-")
    # }

    ## ellipse characteristics -> radii = sqrt of eigenvalues
    ## aspect ratio of ellipse = sqrt of kappa condition index
    aspRat <- sqrt(kappa(S, exact=TRUE))
    flat   <- 1 - (1/aspRat)             # flattening
    detS   <- det(S)
    trS    <- sum(diag(S))
    v0     <- if(p == 2) { pi } else { (4/3)*pi } # volume unit hypersphere: (pi^(p/2) / gamma(p/2 + 1)) * 1^p
    vol    <- v0 * sqrt(detS) # area / volume
    shape  <- c(angle=deg, aspectRatio=aspRat, flattening=flat, trace=trS, det=detS)
    
    list(ctr=c(ctr), E=E, cov=S, area=vol, shape=shape, size=size)
}
