\name{range}
\alias{range}
\alias{qRange}
\alias{rRange}
\title{Distribution of range statistics}
\description{
Quantile function and random deviates of range statistics. Includes extreme spread (ES), figure of merit (FoM), bounding box diagonal (D). This function assumes a circular bivariate normal shot distribution with 0 mean.
}
\usage{
qRange(p, sigma=1, nPerGroup = 5, nGroups = 1, stat = c("ES", "FoM", "D"),
       lower.tail = TRUE)
rRange(n, sigma=1, nPerGroup = 5, nGroups = 1, stat = c("ES", "FoM", "D"))
}
\arguments{
  \item{p}{vector of probabilities. Currently restricted to 0.005, 0.025, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.975, 0.995.}
  \item{n}{number of observations. If \code{length(n) > 1}, the length is taken to be the number required.}
  \item{stat}{character string indicating the range statistic. One of \code{"ES"} (extreme spread), \code{"FoM"} (figure of merit), or \code{"D"} (bounding box diagonal).}
  \item{nPerGroup}{integer between 2 and 100. Number of shots in each group.}
  \item{nGroups}{integer between 1 and 10. Number of groups. For \code{nGroups} > 1, the quantile of the average range statistic is returned.}
  \item{sigma}{numeric value > 0 indicating the Rayleigh scale parameter (common standard deviation of the underlying normal distribution of each 2D-coordinate). See \code{\link[shotGroups]{Rayleigh}}.}
  \item{dstTarget}{a numerical value giving the distance to the target - used in MOA calculation. See \code{\link[shotGroups]{getMOA}}.}
  \item{conversion}{how to convert the measurement unit for distance to target to that of the (x,y)-coordinates in MOA calculation. Example \code{'m2cm'}. See \code{\link[shotGroups]{getMOA}}.}
  \item{lower.tail}{logical. If \code{TRUE} (default), probabilities are \eqn{P[X \le x]} otherwise, \eqn{P[X > x]}.}
}
\details{
Based on the lookup table \code{\link[shotGroups]{DFdistr}} with results form a Monte Carlo simulation. If the value of \code{n} is not among those simulated (but is less than 100), a spline interpolation between the neighboring simulated values of the statistic's coefficient of variation is used.
}
\value{
\code{qRange} gives the quantile function, \code{rRange} generates random deviates. If \code{dstTarget} and \code{conversion} are supplied, the result is also given in angular size measures.}
\references{
\url{http://ballistipedia.com/index.php?title=Range_Statistics}
}
\seealso{
\code{\link[shotGroups]{DFdistr}},
\code{\link[shotGroups]{range2sigma}},
\code{\link[shotGroups]{Rayleigh}},
}
\examples{
qRange(c(0.05, 0.1), stat="ES", n=5, nGroups=3, sigma=1,
       dstTarget=100, conversion="m2cm")
}
\keyword{distribution}
