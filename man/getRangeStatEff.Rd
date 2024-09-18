\name{getRangeStatEff}
\alias{getRangeStatEff}
\title{Confidence interval for range statistics}
\description{Returns the efficiency for range statistics: extreme spread, figure of merit, bounding box diagonal. Calculations assume a circular bivariate normal shot distribution with 0 mean. Efficiency for Rayleigh sigma estimate is also provided for comparison.}
\usage{getRangeStatEff(n, nGroups=1)}
\arguments{
  \item{n}{integer between 2 and 100. Number of shots in each group.}
  \item{nGroups}{integer between 1 and 10. Number of groups when \code{x} is the average of individually-measured range statistics from several groups.}
}
\value{
A data frame with variables \code{n}, \code{nGroups}, \code{nTotal}, as well as efficiency estimates \code{ES_efficiency} (extreme spread), \code{FoM_efficiency} (figure of merit), \code{D_efficiency} (bounding box diagonal), \code{RS_efficiency} (Rayleigh sigma).
}
\details{
Based on the lookup table \code{\link[shotGroups]{DFdistr}} with results from a Monte Carlo simulation. If the value of \code{n} is not among those simulated (but is less than 100), a monotonic spline interpolation between the neighboring simulated values is used. The efficiency is one over the square root of the total number of shots times the squared coefficient of variation.
}
\references{
Grubbs, F. E. (1964b). Statistical measures of accuracy for riflemen and missile engineers. Ann Arbor, ML: Edwards Brothers.
}
\seealso{
\code{\link[shotGroups]{getRangeStat}},
\code{\link[shotGroups]{range2sigma}}
}
\examples{
getRangeStatEff(c(5, 10, 15), nGroups=1:2)
}
