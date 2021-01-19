\name{Rayleigh}
\alias{Rayleigh}
\alias{dRayleigh}
\alias{pRayleigh}
\alias{qRayleigh}
\alias{rRayleigh}
\title{The Rayleigh Distribution}
\description{
Density, distribution function, quantile function, and random deviate generation for the Rayleigh distribution. The radius around the true mean in a bivariate uncorrelated normal random variable with equal variances, re-written in polar coordinates (radius and angle), follows a Rayleigh distribution.
}
\usage{
dRayleigh(x, scale)
pRayleigh(q, scale, lower.tail = TRUE)
qRayleigh(p, scale, lower.tail = TRUE)
rRayleigh(n, scale)
}
\arguments{
  \item{x, q}{vector of quantiles.}
  \item{p}{vector of probabilities.}
  \item{n}{number of observations. If \code{length(n) > 1}, the length is taken to be the number required.}
  \item{scale}{vector of Rayleigh scale parameters (common standard deviation of the underlying normal distribution of each 2D-coordinate).}
  \item{lower.tail}{logical. If \code{TRUE} (default), probabilities are \eqn{P[X \le x]} otherwise, \eqn{P[X > x]}.}
}
\value{
\code{dRayleigh} gives the density, \code{pRayleigh} gives the cumulative distribution function, \code{qRayleigh} gives the quantile function, \code{rRayleigh} generates random deviates.

The length of the result is determined by \code{n} for \code{rRayleigh}, and is the maximum of the lengths of the numerical parameters for the other functions.  
  
In \code{dRayleigh}, \code{pRayleigh} and \code{qRayleigh}, the numerical parameters are recycled to the length of the result. Only the first element of the logical parameters is used. In \code{rRayleigh}, only the first element of \code{scale} is used.
}
\details{
The parameter \code{scale} may be determined with \code{\link[shotGroups]{getRayParam}}.

See \code{\link[shotGroups]{Maxwell}} for the distribution of radial error around the true center of uncorrelated trivariate normal variables with equal variances. See \code{\link[shotGroups]{Hoyt}} for the distribution of radial error around the true center of correlated bivariate normal variables with unequal variances. See \code{\link[shotGroups]{Rice}} for the distribution of radial error around an offset center for uncorrelated bivariate normal variables with equal variances. See \code{\link[shotGroups]{mvnEll}} for the distribution of radial error around an offset center for correlated normal variables with unequal variances.
}
\references{
\url{https://reference.wolfram.com/language/ref/RayleighDistribution.html}
}
\seealso{
\code{\link[shotGroups]{getRayParam}},
\code{\link[shotGroups]{Maxwell}},
\code{\link[shotGroups]{Rice}},
\code{\link[shotGroups]{Hoyt}},
\code{\link[shotGroups]{mvnEll}}
}
\examples{
dRayleigh(1, scale=10)
pRayleigh(c(0.1, 0.5, 0.9), scale=10)
qRayleigh(0.5, scale=c(5, 10, 15))
rRayleigh(5, scale=10)
}
\keyword{distribution}
