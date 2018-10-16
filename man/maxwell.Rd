\name{Maxwell}
\alias{Maxwell}
\alias{dMaxwell}
\alias{pMaxwell}
\alias{qMaxwell}
\alias{rMaxwell}
\title{The Maxwell-Boltzmann Distribution}
\description{
Density, distribution function, quantile function, and random deviate generation for the Maxwell-Boltzmann distribution. The radius around the true mean in a trivariate uncorrelated normal random variable with equal variances, re-written in polar coordinates (radius, azimuth, elevation), follows a Maxwell-Boltzmann distribution.
}
\usage{
dMaxwell(x, sigma)
pMaxwell(q, sigma, lower.tail = TRUE)
qMaxwell(p, sigma, lower.tail = TRUE)
rMaxwell(n, sigma)
}
\arguments{
  \item{x, q}{vector of quantiles.}
  \item{p}{vector of probabilities.}
  \item{n}{number of observations. If \code{length(n) > 1}, the length is taken to be the number required.}
  \item{sigma}{vector of parameter sigma (common standard deviation of the underlying normal distribution of each 3D-coordinate).}
  \item{lower.tail}{logical. If \code{TRUE} (default), probabilities are \eqn{P[X \le x]} otherwise, \eqn{P[X > x]}.}
}
\value{
\code{dMaxwell} gives the density, \code{pMaxwell} gives the cumulative distribution function, \code{qMaxwell} gives the quantile function, \code{rMaxwell} generates random deviates.

The length of the result is determined by \code{n} for \code{rMaxwell}, and is the maximum of the lengths of the numerical parameters for the other functions.  
  
In \code{dMaxwell}, \code{pMaxwell} and \code{qMaxwell} are recycled to the length of the result. Only the first element of the logical parameters is used. In \code{rRayleigh}, only the first element of \code{sigma} is used.
}
\details{
The parameter \code{sigma} may be determined with \code{\link[shotGroups]{getRayParam}}.

See \code{\link[shotGroups]{Rayleigh}} for the distribution of radial error around the true center of uncorrelated bivariate normal variables with equal variances. See \code{\link[shotGroups]{Hoyt}} for the distribution of radial error around the true center of correlated bivariate normal variables with unequal variances. See \code{\link[shotGroups]{Rice}} for the distribution of radial error around an offset center for uncorrelated bivariate normal variables with equal variances. See \code{\link[shotGroups]{mvnEll}} for the distribution of radial error around an offset center for correlated normal variables with unequal variances.
}
\references{
\url{http://reference.wolfram.com/language/ref/MaxwellDistribution.html}
}
\seealso{
\code{\link[shotGroups]{getRayParam}},
\code{\link[shotGroups]{Rayleigh}},
\code{\link[shotGroups]{Hoyt}},
\code{\link[shotGroups]{Rice}},
\code{\link[shotGroups]{mvnEll}}
}
\examples{
dMaxwell(1, sigma=10)
pMaxwell(c(0.1, 0.5, 0.9), sigma=10)
qMaxwell(0.5, sigma=c(5, 10, 15))
rMaxwell(5, sigma=10)
}
\keyword{distribution}
