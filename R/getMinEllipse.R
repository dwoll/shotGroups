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
        xy[H, ]            # points that make up the convex hull
    } else {
        xy
    }

    p     <- ncol(hPts) # dimension
    N     <- nrow(hPts)
    Q     <- rbind(t(hPts), rep(1, N))
    count <- 1
    err   <- 1
    u     <- rep(1/N, N)

    # Khachiyan's algorithm
    while((err > tol) && (count < max_iter)) {
        X         <- Q %*% diag(u) %*% t(Q)
        M         <- diag(t(Q) %*% solve(X) %*% Q)
        maximum   <- max(M)
        j         <- which.max(M)
        step_size <- (maximum - p-1)/((p+1)*(maximum-1))
        new_u     <- (1 - step_size)*u
        new_u[j]  <- new_u[j] + step_size
        err       <- sqrt(sum((new_u-u)^2))
        count     <- count + 1
        u         <- new_u
    }
    
    if(count >= max_iter) { warning("Maximum number of iterations reached") }
    
    U   <- diag(u)
    E   <- (1/p) * solve((t(hPts) %*% U %*% hPts) - ((t(hPts) %*% u) %*% t(t(hPts) %*% u)))
    ctr <- c(t(hPts) %*% u)
    
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
    
    list(ctr=ctr, E=E, cov=S, area=vol, shape=shape, size=size)
}
