\name{simRingCount}
\alias{simRingCount}
\alias{simRingCount.data.frame}
\alias{simRingCount.default}
\title{Calculate simulated ring count for a given group and target}
\description{Calculates the simulated ring count given a group, bullet diameter, and target type.}
\usage{simRingCount(xy, center = FALSE, target, caliber, unit = 'cm')

\method{simRingCount}{data.frame}(xy, center = FALSE, target, caliber, unit = 'cm')

\method{simRingCount}{default}(xy, center = FALSE, target, caliber, unit='cm')}
\arguments{
  \item{xy}{either a numerical (n x 2)-matrix with the (x,y)-coordinates of n points (1 row of coordinates per point), or a data frame with either the variables \code{x}, \code{y} or \code{point.x}, \code{point.y} as well as \code{aim.x}, \code{aim.y} giving the point of aim (= bullseye). If missing, point of aim (bullseye) is assumed to be in (0,0).}
  \item{center}{logical: center groups to mean (0,0) first? If variable \code{series} does not exist, all shots are treated as belonging to the same group. Only available in method \code{simRingCount.data.frame()}.}
  \item{target}{either a character value with the name of a target in \code{\link[shotGroups]{targets}} or a list with a target definition containing the same components as those in \code{\link[shotGroups]{targets}} (\code{name}, \code{unitTarget}, \code{nRings}, \code{ringD10}, \code{ringD10i}, \code{ringW}, \code{cols}, \code{colsTxt}).}
  \item{caliber}{a numerical value indicating the bullet diameter in mm.}
  \item{unit}{measurement unit of the (x,y)-coordinates in \code{xy}. Possible values are \code{'cm', 'mm', 'm', 'in', 'ft', 'yd'}.}
}
\details{
The returned ring count assumes that bullet holes exactly have the diameter given by \code{caliber}, and that rings exactly have the diameter/width given in the definition of \code{target}. The count thus ignores the possibility of ragged bullet holes as well as the physical width of the ring markings. The simulated ring count therefore need not be equal to the calculated ring count from the corresponding physical target.
}
\value{
A list with the following components:
\item{count}{the total ring count.}
\item{max}{the maximum ring count achievable with the given number of shots.}
\item{rings}{the individual ring count for each shot.}
}
\seealso{
\code{\link[shotGroups]{targets}},
\code{\link[shotGroups]{getDistToCtr}}
}
\examples{
simRingCount(DFscar17, target='ISSF_100m', caliber=5.56, unit='in')

# ring count for all groups in DFcm data set
rc <- by(DFcm, DFcm$series, FUN=simRingCount, target='BDS9',
         caliber=9, unit='cm')

sapply(rc, function(x) with(x, c(count=count, max=max)))
}
