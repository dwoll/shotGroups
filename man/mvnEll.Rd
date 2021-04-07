\name{mvnEll}
\alias{mvnEll}
\alias{pmvnEll}
\alias{qmvnEll}
\alias{rmvnEll}
\title{Multivariate normal offset ellipse probabilities}
\description{
Probability of an offset ellipsoid for a correlated multivariate normal distribution. Offset circle probabilities are a special case.
}
\usage{
pmvnEll(r=1, sigma = diag(2), mu, e, x0, lower.tail = TRUE,
        method_cdf = c('integrate', 'saddlepoint'))
        
qmvnEll(p,   sigma = diag(2), mu, e, x0, lower.tail = TRUE,
        loUp=NULL, method_cdf = c('integrate', 'saddlepoint'))

rmvnEll(n,   sigma = diag(2), mu, e, x0,
        method = c('eigen', 'chol', 'cdf'), loUp=NULL,
        method_cdf = c('integrate', 'saddlepoint'))
}
\arguments{
  \item{r}{vector of radii for the offset ellipse defined by \code{e}.}
  \item{p}{vector of probabilities.}
  \item{n}{number of observations. If \code{length(n) > 1}, the length is taken to be the number required.}
  \item{sigma}{true positive definite covariance matrix of multivariate normal distribution.}
  \item{mu}{true center of multivariate normal distribution.}
  \item{e}{positive definite matrix characterizing the offset ellipse defined by (x-x0)' e (x-x0) < r^2. If the ellipse defined by \code{e} has semi-axis lengths equal to the square root of the eigenvalues of a matrix S, and is oriented along the eigenvectors of S, then e = S^-1. By default a circle.}
  \item{x0}{center of the offset ellipse.}
  \item{method}{string indicating which method to use for generating random deviates. See details.}
  \item{loUp}{search interval for numerical root finding. Either a vector with the lower and upper interval boundary, a list of such vectors, or an (n x 2)-matrix. See details.}
  \item{lower.tail}{logical. If \code{TRUE} (default), probabilities are \eqn{P[X \le x]} otherwise, \eqn{P[X > x]}.}
  \item{method_cdf}{string indicating which method to use for calculating the sum of non-central chi^2 variables. See details.}
}
\value{
\code{pmvnEll} integrates the multivariate normal distribution over an arbitrary ellipsoid and thus gives the cumulative distribution function. \code{qmvnEll} gives the quantile function, \code{rmvnEll} generates random deviates.

The functions are vectorized in \code{r} and \code{p} but not in the remaining parameters.
}
\details{
\code{pmvnEll} is implemented by first transforming the integration region to the unit disc/sphere, then decorrelating the normal distribution through rotation. Finally, the quadratic form (sum of non-central chi^2 variables) is calculated depending on \code{method_cdf}. For \code{method_cdf='integrate'}, numerical integrationusing \code{\link[CompQuadForm]{farebrother}} is chosen, for \code{method_cdf='saddlepoint'}, Kuonen's (1999) saddlepoint approximation. Note that the equation for the cumulant generating function K has a missing zeta in the numerator of the second term in Kuonen (1999), see Imhof (1961) instead. Lower tail probabilities are calculated as '1 - upper tail probability', so loss of accuracy is likely when the true upper tail probability is very small (<1e-9).

\code{qmvnEll} is implemented through numerical root finding of \code{pmvnEll}. If no search interval for \code{\link[stats]{uniroot}} is provided, the quantiles of an approximating non-central chi^2 distribution are used to determine the search intervals.

\code{rmvnEll} with \code{method='eigen'} or with \code{method='chol'} simulates 2D normal deviates based on \code{sigma} and \code{mu}, and then determines the radius around \code{x0}. \code{rmvnEll} with \code{method='cdf'} is much slower as it performs numerical root finding of \code{pmvnEll} given simulated quantiles from a uniform random variable in (0,1). If no search interval for \code{\link[stats]{uniroot}} is provided, the quantiles of an approximating non-central chi^2 distribution are used to determine the search intervals.

See \code{\link[shotGroups]{Hoyt}} for the distribution of radial error around the true center of correlated bivariate normal variables with unequal variances. See \code{\link[shotGroups]{Rice}} for the distribution of radial error around an offset center for uncorrelated bivariate normal variables with equal variances. See \code{\link[shotGroups]{Rayleigh}} for the distribution of radial error around the true center of uncorrelated bivariate normal variables with equal variances.
}
\references{
DiDonato, A. R., & Jarnagin, M. P. (1961a). Integration of the general bivariate Gaussian
distribution over an offset circle. Mathematics of Computation, 15 (76), 375-382.

DiDonato, A. R., & Jarnagin, M. P. (1961b). Integration of the general bivariate Gaussian distribution over an offset ellipse (NWL TR 1710). Dahlgren, VA: U.S. Naval Weapons Laboratory.

Duchesne, P., & Lafaye de Micheaux, P. (2010). Computing the distribution of quadratic forms:
Further comparisons between the Liu-Tang-Zhang approximation and exact methods.
Computational Statistics and Data Analysis, 54, 858-862.

Imhof, J. P. (1961). Computing the distribution of quadratic forms in normal variables. Biometrika, 48, 419-426.

Kuonen D. (1999). Saddlepoint Approximations for Distributions of Quadratic Forms
## in Normal Variables. Biometrika, 86, 929-935.
}
\seealso{
\code{\link[shotGroups]{Hoyt}},
\code{\link[CompQuadForm]{farebrother}},
\code{\link[stats]{uniroot}}
}
\examples{
# define a bivariate normal distribution
mu    <- c(2, -1)                        # true mean
sigma <- cbind(c(10, 6), c(6, 10))       # covariance matrix

# define circular integration region
ctr <- c(1, 0)                           # center
e1  <- diag(2)                           # circle
r   <- 2                                 # radius
pmvnEll(r,   sigma=sigma, mu=mu, e=e1, x0=ctr) # probability
qmvnEll(0.5, sigma=sigma, mu=mu, e=e1, x0=ctr) # quantile
rmvnEll(5,   sigma=sigma, mu=mu, e=e1, x0=ctr) # random numbers

# define elliptical integration region
S  <- cbind(c(3.5, -0.3), c(-0.3, 1.7))
e2 <- solve(S)
pmvnEll(r,   sigma=sigma, mu=mu, e=e2, x0=ctr) # probability
qmvnEll(0.5, sigma=sigma, mu=mu, e=e2, x0=ctr) # quantile
rmvnEll(5,   sigma=sigma, mu=mu, e=e2, x0=ctr) # random numbers

# plot all regions
evSig <- eigen(sigma)$values
evS   <- eigen(S)$values
xLims <- range(c( mu[1]+c(-1, 1)*sqrt(evSig[1]),
                 ctr[1]+c(-r, r)*sqrt(evS[1])))
yLims <- range(c( mu[2]+c(-1.25, 1.25)*sqrt(evSig[1]),
                 ctr[2]+c(-r, r)*sqrt(evS[1])))

plot(xLims, yLims, type="n", asp=1)
points(mu[1],  mu[2],  pch=16, cex=2, col="black")
points(ctr[1], ctr[2], pch=15, cex=2, col="blue")
drawEllipse(mu, sigma, r=0.75, fg="black")
drawEllipse(mu, sigma, r=1,    fg="black")
drawEllipse(mu, sigma, r=1.25, fg="black")
drawEllipse(mu, sigma, r=1.5,  fg="black")
drawEllipse(ctr, e1, r=r, fg="blue")
drawEllipse(ctr, S,  r=r, fg="red")
legend(x="bottomright", legend=c("normal iso-densities",
       "integration circle", "integration ellipse"),
       lty=1, col=c("black", "blue", "red"))
}
