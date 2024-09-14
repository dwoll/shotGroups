\name{getRangeStatCI}
\alias{getRangeStatCI}
\title{Confidence interval for range statistics}
\description{Returns the confindence interval for given range statistics: extreme spread, figure of merit, bounding box diagonal. Calculations assume a circular bivariate normal shot distribution with 0 mean.}
\usage{getRangeStatCI(x, stat="ES", n, nGroups, CIlevel=0.95,
              collapse=TRUE, dstTarget, conversion5)}
\arguments{
  \item{x}{a numerical vector with values for extreme spread (ES), figure of merit (FoM), or the diagonal of the bounding box (D).}
  \item{stat}{a character vector with elements \code{"ES"} (extreme spread), \code{"FoM"} (figure of merit), or \code{"D"} (bounding box diagonal) indicating which range statistic is given in \code{x}. Elements correspond to those in \code{x} in the sense that the second element of \code{stat} indicates the statistic for the second element of \code{x}. If all elements of \code{x} are the same kind of statistic, \code{stat} only needs to indicate it once.}
  \item{n}{integer between 2 and 100. Number of shots in each group.}
  \item{nGroups}{integer between 1 and 10. Number of groups when \code{x} is the average of individually-measured range statistics from several groups.}
  \item{CIlevel}{confidence level (coverage probability) for the confidence interval. If one of \code{0.5}, \code{0.9}, \code{0.95}, \code{0.99}, the CI is based on the corresponding quantiles of the Monte Carlo distribution of the range statistic for given \code{n} and \code{nGroups}. If not, CI can only be calculated for extreme spread using a Patnaik chi^2 approximation to the conditional distribution as suggested by Taylor and Grubbs (1975).}
  \item{collapse}{logical: should the list with CIs be simplified if possible?}
  \item{dstTarget}{a numerical value giving the distance to the target - used in MOA calculation. See \code{\link[shotGroups]{getMOA}}.}
  \item{conversion}{how to convert the measurement unit for distance to target to that of the (x,y)-coordinates in MOA calculation. Example \code{'m2cm'}. See \code{\link[shotGroups]{getMOA}}.}
}
\value{
A named list with first component \code{range_stat}. This is a matrix with columns \code{ES} (extreme spread), \code{FoM} (figure of merit), \code{D} (bounding box diagonal). The second component \code{CI} gives the corresponding confidence interval boundaries as a named list with components \code{ES}, \code{FoM}, \code{D}.
}
\details{
Based on the lookup table \code{\link[shotGroups]{DFdistr}} with results from a Monte Carlo simulation. If the value of \code{n} is not among those simulated (but is less than 100), a monotonic spline interpolation between the neighboring simulated values is used.

The CI is calculated based on implicit re-scaling by the Rayleigh sigma parameter. First, the tabulated CI boundaries are normalized to a range statistic with point estimate of 1 (via division by the tabulated mean). Then, the normalized CI boundaries are multiplied with the provided range statistic.
}
\references{
Grubbs, F. E. (1964b). Statistical measures of accuracy for riflemen and missile engineers. Ann Arbor, ML: Edwards Brothers.
}
\seealso{
\code{\link[shotGroups]{getRangeStat}},
\code{\link[shotGroups]{range2sigma}}
}
\examples{
getRangeStatCI(c(1, 2, 3),
               stat=c("ES", "FoM", "D"),
               n=10,
               nGroups=1)
}
