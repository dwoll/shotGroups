\name{getKuchnost}
\alias{getKuchnost}
\alias{getKuchnost.data.frame}
\alias{getKuchnost.default}
\title{Kuchnost precision estimate}
\description{Estimates the Kuchnost precision measure. The maximum distance to the group center after removing outliers.}
\usage{getKuchnost(xy, dstTarget, conversion,
       center = FALSE, doRob = FALSE, strict = FALSE)

\method{getKuchnost}{data.frame}(xy, dstTarget, conversion,
       center = FALSE, doRob = FALSE, strict = FALSE)

\method{getKuchnost}{default}(xy, dstTarget, conversion,
       center = FALSE, doRob = FALSE, strict = FALSE)}
\arguments{
  \item{xy}{either a numerical (n x p)-matrix with the coordinates of n points (1 row of p coordinates per point), or a data frame with either the variables \code{x}, \code{y} or \code{point.x}, \code{point.y} (optionally \code{z} or \code{point.z}).}
  \item{dstTarget}{a numerical value giving the distance to the target - used in MOA calculation. Acts as override if variable \code{distance} is already included in \code{xy}. See \code{\link[shotGroups]{getMOA}}.}
  \item{conversion}{how to convert the measurement unit for distance to target to that of the (x,y)-coordinates in MOA calculation. Acts as override if variables \code{dist.unit} and \code{point.unit} are already included in \code{xy}. Example \code{'m2cm'}. See \code{\link[shotGroups]{getMOA}}.}
  \item{center}{logical: center groups to mean (0,0) first? If variable \code{series} does not exist, all shots are treated as belonging to the same group. Only available in method \code{getCEP.data.frame()}.}
  \item{doRob}{logical: use robust estimation of center and covariance matrix as basis for estimators?}
  \item{strict}{logical: enable check that \code{xy} contains exactly 4 shots.}
}
\details{
An outlier is defined as a shot that is at least 2.5 times further away from the group center defined by all remaining shots compared to the maximum distance to center of those shots themselves.
}
\value{
A list with 3 components.
\item{Kuchnost}{The Kuchnost precision measure.}
\item{outlier}{Indices of outliers, if any.}
\item{ctr}{group center after removing outliers}
}
\references{
\url{https://github.com/lstange/mcgs}
}
\seealso{
\code{\link[robustbase]{covMcd}}
}
\examples{
getKuchnost(DF300BLK)
}
