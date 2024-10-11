#####---------------------------------------------------------------------------
## Saddlepoint approximation for the sum of non-central chi^2 variables
## Kuonen D. Saddlepoint Approximations for Distributions of Quadratic Forms
## in Normal Variables. Biometrika 1999; 86(4): 929-935.
## https://www.jstor.org/stable/2673596
## https://infoscience.epfl.ch/record/84834/files/860929.pdf
## However, equations for K, K', K'' are incorrect as zeta is missing from
## the numerator of the second term in the cumulant generating function K
## see Imhof(1961) equation 2.3 https://doi.org/10.1093/biomet/48.3-4.419
## code adapted from Han Chen <Han.Chen.2 at uth.tmc.edu>
## https://cran.r-project.org/package=GMMAT
## https://github.com/hanchenphd/GMMAT/blob/master/R/SMMAT.R
#####---------------------------------------------------------------------------

## lambda = eigenvalues
## h      = multiplicities of lambda -> will be df of non-central chi^2
## delta  = non-centrality parameters of non-central chi^2
saddlepoint <-
function(x, lambda, h=rep(1, length(lambda)), delta=rep(0, length(lambda))) {
    n <- length(lambda)
    stopifnot(length(x)     == 1L,
              length(h)     == n,
              length(delta) == n,
              !anyNA(lambda),
              ## function would work for negative eigenvalues (but not 0),
              ## but not relevant for covariance matrices
              all(lambda > 0))

    ## 1D case
    if(n == 1L) {
        return(pchisq(x/lambda, df=h, ncp=delta, lower.tail=FALSE))
    }

    ## scale by max eigenvalue
    lambda_scl <- lambda / max(lambda)
    x_scl      <- x / max(lambda)
    
    ## cumulant generating function
    K <- function(zeta) {
        -0.5*sum(h*log(1 - 2*zeta*lambda_scl)) + sum((delta*lambda_scl*zeta)/(1 - 2*zeta*lambda_scl))
    }

    ## first derivative - vectorized as required by uniroot() below
    Kprime <- function(zeta) {
        vapply(zeta, function(zz) {
            sum(((delta + h)*lambda_scl)/(1 - 2*zz*lambda_scl) + 2*(delta*zz*lambda_scl^2)/(1 - 2*zz*lambda_scl)^2)
        }, numeric(1))
    }

    ## second derivative    
    Kprime_prime <- function(zeta) {
        sum((2*(2*delta + h)*lambda_scl^2)/(1 - 2*zeta*lambda_scl)^2 + 8 *
                delta*zeta*lambda_scl^3/(1 - 2*zeta*lambda_scl)^3)
    }

    lmax <- min(1/(2 * lambda_scl[lambda_scl > 0])) * 0.99999
    lmin <- if(any(lambda_scl < 0)) {
        max(1/(2 * lambda_scl[lambda_scl < 0])) * 0.99999
    } else if(x_scl > sum((h+delta)*lambda_scl)) {
        -0.01
    } else {
        -n*max(h+delta)/(2*x_scl)
    }

    zeta_hat <- tryCatch(uniroot(function(zeta) { Kprime(zeta) - x_scl }, 
                                 lower=lmin, upper=lmax, tol=1e-08)$root,
                         error=function(e) return(NA_real_))
    
    w <- sign(zeta_hat)*sqrt(2*(zeta_hat*x_scl - K(zeta_hat)))
    v <- zeta_hat*sqrt(Kprime_prime(zeta_hat))

    if(abs(zeta_hat) < 1e-4) {
        NA_real_
    } else {
        pnorm(w + log(v/w)/w, lower.tail=FALSE)
    }
}
