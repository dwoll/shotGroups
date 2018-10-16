\name{DFdistr}
\alias{DFdistr}
\docType{data}
\title{Lookup table for distribution of range statistics and Rayleigh sigma}
\description{
Lookup table for the distribution of range statistics and Rayleigh sigma from a Monte Carlo simulation of circular bivariate normal shot groups with 0 mean and variance 1 in both directions. Includes the first four moments and several quantiles of the distribution of extreme spread, figure of merit, bounding box diagonal, and Rayleigh sigma for each combination of number of shots per group and number of groups, repeated 2000000 times.}
\usage{data(DFdistr)}
\format{
  A data frame with 590 observations on the following 73 variables.
  \describe{
  \item{n}{number of shots in each group. One of 2, 3, ..., 49, 50, 45, ..., 95, 100.}
  \item{nGroups}{number of groups with individual simulated range statistics that were averaged over to yield the final value. One of 1, 2, ..., 9, 10.}
  \item{nShots}{total number of shots, i.e., \code{n*nGroups}.}
  \item{ES_M}{Extreme spread mean over all Monte Carlo simulations}
  \item{ES_V}{Extreme spread variance over all Monte Carlo simulations}
  \item{ES_SD}{Extreme spread standard deviation over all Monte Carlo simulations}
  \item{ES_CV}{Extreme spread coefficient of variation over all Monte Carlo simulations}
  \item{ESSQ_M}{Squard extreme spread mean over all Monte Carlo simulations}
  \item{ESSQ_V}{Squared extreme spread variance over all Monte Carlo simulations}
  \item{ES_SKEW}{Extreme spread skewness over all Monte Carlo simulations}
  \item{ES_KURT}{Extreme spread kurtosis over all Monte Carlo simulations}
  \item{ES_MED}{Extreme spread median (50\% quantile) over all Monte Carlo simulations}
  \item{ES_Q005}{Extreme spread 0.5\% quantile over all Monte Carlo simulations}
  \item{ES_Q025}{Extreme spread 2.5\% quantile over all Monte Carlo simulations}
  \item{ES_Q050}{Extreme spread 5\% quantile over all Monte Carlo simulations}
  \item{ES_Q100}{Extreme spread 10\% quantile over all Monte Carlo simulations}
  \item{ES_Q250}{Extreme spread 25\% quantile over all Monte Carlo simulations}
  \item{ES_Q500}{Extreme spread 50\% quantile (median) over all Monte Carlo simulations}
  \item{ES_Q750}{Extreme spread 75\% quantile over all Monte Carlo simulations}
  \item{ES_Q900}{Extreme spread 90\% quantile over all Monte Carlo simulations}
  \item{ES_Q950}{Extreme spread 95\% quantile over all Monte Carlo simulations}
  \item{ES_Q975}{Extreme spread 97.5\% quantile over all Monte Carlo simulations}
  \item{ES_Q995}{Extreme spread 99.5\% quantile over all Monte Carlo simulations}
  \item{FoM_M}{Figure of merit mean over all Monte Carlo simulations}
  \item{FoM_V}{Figure of merit variance over all Monte Carlo simulations}
  \item{FoM_SD}{Figure of merit standard deviation over all Monte Carlo simulations}
  \item{FoM_CV}{Figure of merit coefficient of variation over all Monte Carlo simulations}
  \item{FoM_SKEW}{Figure of merit skewness over all Monte Carlo simulations}
  \item{FoM_KURT}{Figure of merit kurtosis over all Monte Carlo simulations}
  \item{FoM_MED}{Figure of merit median (50\% quantile) over all Monte Carlo simulations}
  \item{FoM_Q005}{Figure of merit 0.5\% quantile over all Monte Carlo simulations}
  \item{FoM_Q025}{Figure of merit 2.5\% quantile over all Monte Carlo simulations}
  \item{FoM_Q050}{Figure of merit 0.25\% quantile over all Monte Carlo simulations}
  \item{FoM_Q100}{Figure of merit 10\% quantile over all Monte Carlo simulations}
  \item{FoM_Q250}{Figure of merit 25\% quantile over all Monte Carlo simulations}
  \item{FoM_Q500}{Figure of merit 50\% quantile (median) over all Monte Carlo simulations}
  \item{FoM_Q750}{Figure of merit 75\% quantile over all Monte Carlo simulations}
  \item{FoM_Q900}{Figure of merit 90\% quantile over all Monte Carlo simulations}
  \item{FoM_Q950}{Figure of merit 95\% quantile over all Monte Carlo simulations}
  \item{FoM_Q975}{Figure of merit 97.5\% quantile over all Monte Carlo simulations}
  \item{FoM_Q995}{Figure of merit 99.5\% quantile over all Monte Carlo simulations}
  \item{D_M}{Bounding box diagonal mean over all Monte Carlo simulations}
  \item{D_V}{Bounding box diagonal variance over all Monte Carlo simulations}
  \item{D_SD}{Bounding box diagonal standard deviation over all Monte Carlo simulations}
  \item{D_CV}{Bounding box diagonal coefficient of variation over all Monte Carlo simulations}
  \item{D_SKEW}{Bounding box diagonal skewness over all Monte Carlo simulations}
  \item{D_KURT}{Bounding box diagonal kurtosis over all Monte Carlo simulations}
  \item{D_MED}{Bounding box diagonal median (50\% quantile) over all Monte Carlo simulations}
  \item{D_Q005}{Bounding box diagonal 0.5\% quantile over all Monte Carlo simulations}
  \item{D_Q025}{Bounding box diagonal 2.5\% quantile over all Monte Carlo simulations}
  \item{D_Q050}{Bounding box diagonal 5\% quantile over all Monte Carlo simulations}
  \item{D_Q100}{Bounding box diagonal 10\% quantile over all Monte Carlo simulations}
  \item{D_Q250}{Bounding box diagonal 25\% quantile over all Monte Carlo simulations}
  \item{D_Q500}{Bounding box diagonal 50\% quantile (median) over all Monte Carlo simulations}
  \item{D_Q750}{Bounding box diagonal 75\% quantile over all Monte Carlo simulations}
  \item{D_Q900}{Bounding box diagonal 90\% quantile over all Monte Carlo simulations}
  \item{D_Q950}{Bounding box diagonal 95\% quantile over all Monte Carlo simulations}
  \item{D_Q975}{Bounding box diagonal 97.5\% quantile over all Monte Carlo simulations}
  \item{D_Q995}{Bounding box diagonal 99.5\% quantile over all Monte Carlo simulations}
  \item{RS_M}{Rayleigh sigma mean over all Monte Carlo simulations}
  \item{RS_V}{Rayleigh sigma variance over all Monte Carlo simulations}
  \item{RS_SD}{Rayleigh sigma standard deviation over all Monte Carlo simulations}
  \item{RS_CV}{Rayleigh sigma coefficient of variation over all Monte Carlo simulations}
  \item{RS_SKEW}{Rayleigh sigma skewness over all Monte Carlo simulations}
  \item{RS_KURT}{Rayleigh sigma kurtosis over all Monte Carlo simulations}
  \item{RS_MED}{Rayleigh sigma median (50\% quantile) over all Monte Carlo simulations}
  \item{RS_Q005}{Rayleigh sigma 0.5\% quantile over all Monte Carlo simulations}
  \item{RS_Q025}{Rayleigh sigma 2.5\% quantile over all Monte Carlo simulations}
  \item{RS_Q050}{Rayleigh sigma 5\% quantile over all Monte Carlo simulations}
  \item{RS_Q100}{Rayleigh sigma 10\% quantile over all Monte Carlo simulations}
  \item{RS_Q250}{Rayleigh sigma 25\% quantile over all Monte Carlo simulations}
  \item{RS_Q500}{Rayleigh sigma 50\% quantile (median) over all Monte Carlo simulations}
  \item{RS_Q750}{Rayleigh sigma 75\% quantile over all Monte Carlo simulations}
  \item{RS_Q900}{Rayleigh sigma 90\% quantile over all Monte Carlo simulations}
  \item{RS_Q950}{Rayleigh sigma 95\% quantile over all Monte Carlo simulations}
  \item{RS_Q975}{Rayleigh sigma 97.5\% quantile over all Monte Carlo simulations}
  \item{RS_Q995}{Rayleigh sigma 99.5\% quantile over all Monte Carlo simulations}
 }
}
\details{
The Monte Carlo distribution used 2000000 repetitions in each scenario. One scenario was a combination of the \code{n} shots in each group, and the \code{nGroups} groups over which individual range statistics were averaged. Values for \code{n} were 2, 3, ..., 49, 50, 45, ..., 95, 100. Values for \code{nGroups} were 1, 2, ... 9, 10.

Used in \code{\link[shotGroups]{range2sigma}} to estimate Rayleigh parameter sigma from range statistics, and in \code{\link[shotGroups]{efficiency}} to estimate the number of groups and total shots required to estimate the confidence interval (CI) for Rayleigh sigma with a given coverage probability (CI level) and width.

See the following source for an independent simulation, and for the rationale behind using it to estimate Rayleigh sigma:

\url{http://ballistipedia.com/index.php?title=Range_Statistics}

An older eqivalent simulation with less repetitions was done by Taylor and Grubbs (1975).
}
\references{
Taylor, M. S., & Grubbs, F. E. (1975). Approximate Probability Distributions for the Extreme Spread (BRL-MR-2438). Aberdeen Proving Ground, MD: U.S. Ballistic Research Laboratory.
}
\seealso{
\code{\link[shotGroups]{range2sigma}},
\code{\link[shotGroups]{efficiency}},
\code{\link[shotGroups]{getMaxPairDist}},
\code{\link[shotGroups]{getBoundingBox}},
\code{\link[shotGroups]{getRayParam}}
}
\examples{
data(DFdistr)
str(DFdistr)
}
\keyword{datasets}
