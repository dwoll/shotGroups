\name{Hoyt}
\alias{Hoyt}
\alias{dHoyt}
\alias{pHoyt}
\alias{qHoyt}
\alias{rHoyt}
\title{The Hoyt Distribution}
\description{
Density, distribution function, quantile function, and random deviate generation for the Hoyt distribution. The radius around the true mean in a bivariate normal random variable, re-written in polar coordinates (radius and angle), follows a Hoyt distribution. Equivalently, the modulus of a complex normal random variable does.
}
\usage{
dHoyt(x, qpar, omega)
pHoyt(q, qpar, omega, lower.tail = TRUE)
qHoyt(p, qpar, omega, lower.tail = TRUE, loUp = NULL)
rHoyt(n, qpar, omega, method = c('eigen', 'chol', 'cdf'), loUp = NULL)
}
\arguments{
  \item{x, q}{vector of quantiles.}
  \item{p}{vector of probabilities.}
  \item{n}{number of observations. If \code{length(n) > 1}, the length is taken to be the number required.}
  \item{qpar}{vector of Hoyt shape parameters q. See details.}
  \item{omega}{vector of Hoyt scale parameters omega. See details.}
  \item{method}{string indicating which method to use for generating random deviates. See details.}
  \item{loUp}{search interval for numerical root finding. Either a vector with the lower and upper interval boundary, a list of such vectors, or an (n x 2)-matrix. See details.}
  \item{lower.tail}{logical. If \code{TRUE} (default), probabilities are \eqn{P[X \le x]} otherwise, \eqn{P[X > x]}.}
}
\value{
\code{dHoyt} gives the density, \code{pHoyt} gives the cumulative distribution function, \code{qHoyt} gives the quantile function, \code{rHoyt} generates random deviates.

The length of the result is determined by \code{n} for \code{rHoyt}, and is the maximum of the lengths of the numerical parameters for the other functions.  
  
In \code{dHoyt}, \code{pHoyt} and \code{qHoyt}, the numerical parameters are recycled to the length of the result. Only the first element of the logical parameters is used. In \code{rHoyt}, only the first element of \code{qpar} and \code{omega} is used.
}
\details{
The parameters \code{qpar} and \code{omega} may be determined with \code{\link[shotGroups]{getHoytParam}}.

\code{dHoyt} involves the modified Bessel function of the first kind and order 0 (Chew & Boye, 1962; Hoyt, 1947). \code{pHoyt} is implemented as the symmetric difference between two Marcum Q-functions (Paris, 2009). The Marcum Q-function is the cdf of a non-central chi^2 variable (Nuttall, 1975).

\code{qHoyt} is implemented through numerical root finding of \code{pHoyt}. If no search interval for \code{\link[stats]{uniroot}} is provided, the quantiles of an approximating central chi^2 distribution are used to determine the search intervals.

\code{rHoyt} with \code{method='eigen'} or with \code{method='chol'} simulates 2D normal deviates based on the covariance matrix corresponding to parameters \code{qpar} and \code{omega}, and then determines the radius. \code{rHoyt} with \code{method='cdf'} is much slower as it performs numerical root finding of \code{pHoyt} given simulated quantiles from a uniform random variable in (0,1). If no search interval for \code{\link[stats]{uniroot}} is provided, the quantiles of an approximating central chi^2 distribution are used to determine the search intervals.

See \code{\link[shotGroups]{Rice}} for the distribution of radial error around an offset center for uncorrelated bivariate normal variables with equal variances. See \code{\link[shotGroups]{Rayleigh}} for the distribution of radial error around the true center of uncorrelated bivariate normal variables with equal variances. See \code{\link[shotGroups]{mvnEll}} for the distribution of radial error around an offset center for correlated normal variables with unequal variances.
}
\references{
Chew, V. & Boyce, R. (1962). Distribution of radial error in bivariate elliptical normal distributions. Technometrics, 4(1), 138-140.

Hoyt, R. S. (1947). Probability functions for the modulus and angle of the normal complex variate.
Bell System Technical Journal, 26(2), 318-359.

Nuttall, AH. (1975). Some integrals involving the Q-M function.
IEEE Transactions on Information Theory, 21 (1), 95-96

Paris, JF. 2009. Nakagami-q (Hoyt) distribution function with applications.
Electronics Letters, 45(4). 210-211. Erratum: doi:10.1049/el.2009.0828

\url{https://reference.wolfram.com/language/ref/HoytDistribution.html}
}
\seealso{
\code{\link[shotGroups]{getHoytParam}},
\code{\link[shotGroups]{Rayleigh}},
\code{\link[shotGroups]{Rice}},
\code{\link[shotGroups]{mvnEll}},
\code{\link[base]{Bessel}},
\code{\link[stats]{Chisquare}},
\code{\link[stats]{uniroot}}
}
\examples{
dHoyt(1, qpar=c(0.1, 0.5, 0.9), omega=10)
pHoyt(c(0.1, 0.5, 0.9), qpar=0.5, omega=10)
qHoyt(0.5, qpar=0.5, omega=c(5, 10, 15))
rHoyt(5, qpar=0.5, omega=10)
}
\keyword{distribution}
