\name{getRangeStat}
\alias{getRangeStat}
\alias{getRangeStat.data.frame}
\alias{getRangeStat.default}
\title{Range statistics}
\description{Returns range statistics: extreme spread, figure of merit, bounding box diagonal, each with corresponding confidence interval assuming a circular bivariate normal shot distribution with 0 mean.}
\usage{getRangeStat(xy, dstTarget, conversion, CIlevel=0.95)

\method{getRangeStat}{data.frame}(xy, dstTarget, conversion, CIlevel=0.95)

\method{getRangeStat}{default}(xy, dstTarget, conversion, CIlevel=0.95)}
\arguments{
  \item{xy}{either a numerical (n x p)-matrix with the coordinates of n points (1 row of p coordinates per point), or a data frame with either the variables \code{x}, \code{y} or \code{point.x}, \code{point.y} (optionally \code{z} or \code{point.z}).}
  \item{dstTarget}{a numerical value giving the distance to the target - used in MOA calculation. Acts as override if variable \code{distance} is already included in \code{xy}. See \code{\link[shotGroups]{getMOA}}.}
  \item{conversion}{how to convert the measurement unit for distance to target to that of the (x,y)-coordinates in MOA calculation. Acts as override if variables \code{dist.unit} and \code{point.unit} are already included in \code{xy}. Example \code{'m2cm'}. See \code{\link[shotGroups]{getMOA}}.}
  \item{CIlevel}{a numerical value with the coverage for the confidence interval.}
}
\value{
A named list with first components \code{range_stat}. This is a matrix with columns \code{ES} (extreme spread), \code{FoM} (figure of merit), \code{D} (bounding box diagonal). The second component \code{CI} gives the corresponding confidence interval boundaries as a named list with components \code{ES}, \code{FoM}, \code{D}.
}
\references{
Grubbs, F. E. (1964b). Statistical measures of accuracy for riflemen and missile engineers. Ann Arbor, ML: Edwards Brothers.
}
\seealso{
\code{\link[shotGroups]{getMaxPairDist}},
\code{\link[shotGroups]{getBoundingBox}}
}
\examples{
getRangeStat(DFscar17)
}
