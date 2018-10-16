\name{Rice}
\alias{Rice}
\alias{dRice}
\alias{pRice}
\alias{qRice}
\alias{rRice}
\title{The Rice Distribution}
\description{
Density, distribution function, quantile function, and random deviate generation for the Rice distribution. The radius around the origin in a bivariate uncorrelated normal random variable with equal variances and an offset mean, re-written in polar coordinates (radius and angle), follows a Rice distribution.
}
\usage{
dRice(x, nu, sigma)
pRice(q, nu, sigma, lower.tail = TRUE)
qRice(p, nu, sigma, lower.tail = TRUE)
rRice(n, nu, sigma, method = c('eigen', 'chol', 'cdf'))
}
\arguments{
  \item{x, q}{vector of quantiles.}
  \item{p}{vector of probabilities.}
  \item{n}{number of observations. If \code{length(n) > 1}, the length is taken to be the number required.}
  \item{nu}{vector of Rice shape parameters nu. See details.}
  \item{sigma}{vector of Rice scale parameter sigma. See details.}
  \item{method}{string indicating which method to use for generating random deviates. See details.}
  \item{lower.tail}{logical. If \code{TRUE} (default), probabilities are \eqn{P[X \le x]} otherwise, \eqn{P[X > x]}.}
}
\value{
\code{dRice} gives the density, \code{pRice} gives the cumulative distribution function, \code{qRice} gives the quantile function, \code{rRice} generates random deviates.

The length of the result is determined by \code{n} for \code{rRice}, and is the maximum of the lengths of the numerical parameters for the other functions.  
  
In \code{dRice}, \code{pRice} and \code{qRice}, the numerical parameters are recycled to the length of the result. Only the first element of the logical parameters is used. In \code{rRice}, only the first element of \code{nu} and \code{sigma} is used.
}
\details{
\code{dRice} involves the modified Bessel function of the first kind and order 0. \code{pRice} and \code{qRice} are implemented using the Marcum Q-function. The Marcum Q-function is the cdf of a non-central chi^2 variable (Nuttall, 1975).

\code{rRice} with \code{method='eigen'} or with \code{method='chol'} simulates 2D normal deviates based on the diagonal covariance matrix with entries \code{sigma}^2, and then determines the radius. \code{rRice} with \code{method='cdf'} uses \code{qRice} with simulated quantiles from a uniform random variable in (0,1).

See \code{\link[shotGroups]{Hoyt}} for the distribution of radial error around the true center of correlated bivariate normal variables with unequal variances. See \code{\link[shotGroups]{Rayleigh}} for the distribution of radial error around the true center of uncorrelated bivariate normal variables with equal variances. See \code{\link[shotGroups]{mvnEll}} for the distribution of radial error around an offset center for correlated normal variables with unequal variances.

For very large signal-to-noise ratios (nu/sigma), a normal approximation is used.
}
\references{
Nuttall, AH. (1975). Some integrals involving the Q-M function.
IEEE Transactions on Information Theory, 21 (1), 95-96

\url{http://reference.wolfram.com/language/ref/RiceDistribution.html}
}
\seealso{
\code{\link[shotGroups]{Rayleigh}},
\code{\link[shotGroups]{Hoyt}},
\code{\link[shotGroups]{mvnEll}},
\code{\link[base]{Bessel}},
\code{\link[stats]{Chisquare}},
\code{\link[stats]{uniroot}}
}
\examples{
dRice(1, nu=c(0.1, 0.5, 0.9), sigma=10)
pRice(c(0.1, 0.5, 0.9), nu=0.5, sigma=10)
qRice(0.5, nu=0.5, sigma=c(5, 10, 15))
rRice(5, nu=0.5, sigma=10)
}
\keyword{distribution}
