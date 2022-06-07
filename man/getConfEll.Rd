\name{getConfEll}
\alias{getConfEll}
\alias{getConfEll.data.frame}
\alias{getConfEll.default}
\title{Confidence ellipse}
\description{Calculates the confidence ellipse for the true mean of shot coordinates under the assumption of multivariate normality. Also includes the ellipse based on a robust estimate for the covariance matrix of the shot coordinates.}
\usage{getConfEll(xy, level = 0.5, dstTarget, conversion,
          center = FALSE, doRob=TRUE)

\method{getConfEll}{data.frame}(xy, level = 0.5, dstTarget, conversion,
          center = FALSE, doRob=TRUE)

\method{getConfEll}{default}(xy, level = 0.5, dstTarget, conversion,
          center = FALSE, doRob=TRUE)}
\arguments{
  \item{xy}{Shot coordinates of n points: either a numerical (n x p)-matrix (1 row of p coordinates per point), or a data frame with either the variables \code{x}, \code{y} or \code{point.x}, \code{point.y} (optionally \code{z} or \code{point.z}).}
  \item{level}{a numerical value with the coverage for the confidence ellipse.}
  \item{dstTarget}{a numerical value giving the distance to the target - used in MOA calculation. Acts as override if variable \code{distance} is already included in \code{xy}. See \code{\link[shotGroups]{getMOA}}.}
  \item{conversion}{how to convert the measurement unit for distance to target to that of the (x,y)-coordinates in MOA calculation. Acts as override if variables \code{dist.unit} and \code{point.unit} are already included in \code{xy}. Example \code{'m2cm'}. See \code{\link[shotGroups]{getMOA}}.}
  \item{center}{logical: center groups to mean (0,0) first? If variable \code{series} does not exist, all shots are treated as belonging to the same group. Only available in method \code{getConfEll.data.frame()}.}
  \item{doRob}{logical: should robust covariance matrix estimate be used as well?}
}
\details{No coordinate transforms are done (unlike in \code{\link[shotGroups]{groupLocation}}, \code{\link[shotGroups]{groupShape}}, \code{\link[shotGroups]{groupSpread}}), i.e., origin is not assumed to be in top-left corner, and points are not taken relative to point of aim.

Robust estimate for the covariance matrix of coordinates is from \code{\link[robustbase]{covMcd}} using the MCD algorithm. See \code{\link[shotGroups]{getCEP}} for estimates of the circular/spherical error probable.}
\value{
A list with the confidence ellipse measures.
\item{ctr}{coordinates group center.}
\item{ctrRob}{coordinates robust estimate of group center.}
\item{cov}{covariance matrix.}
\item{covRob}{robust estimate of covariance matrix.}
\item{size}{a numerical matrix with the lengths of the semi-axes of the ellipse (in original measurement units, MOA, SMOA, milliradian).}
\item{sizeRob}{a numerical matrix with the lengths of the semi-axes of the ellipse based on a robust estimate for the covariance matrix of shot coordinates (in original measurement units, MOA, SMOA, milliradian).}
\item{shape}{a numerical vector with the angle, the aspect ratio of the ellipse (square root of condition index \code{\link{kappa}}), its flattening (1 - inverse aspect ratio) as well as the trace and determinant of the covariance matrix.}
\item{shapeRob}{a numerical vector with the aspect ratio and the flattening of the ellipse as well as the trace and determinant based on a robust estimate for the covariance matrix of shot coordinates.}
\item{magFac}{magnification factor used to turn the error ellipse into the confidence ellipse as determined by the F(p, n-1)-distribution.}
}
\seealso{
\code{\link[shotGroups]{getCEP}},
\code{\link[robustbase]{covMcd}},
\code{\link[shotGroups]{drawEllipse}}
}
\examples{
# coordinates given by a suitable data frame
(ce <- getConfEll(DFsavage, level=0.5, dstTarget=100,
                  conversion='yd2in'))

# plot points, center, parametric confidence
# ellipse, and its axes
plot(point.y ~ point.x, data=DFsavage, asp=1, pch=16)
drawEllipse(ce, axes=TRUE, fg='blue', colCtr='blue', lwd=2, pch=4, cex=2)

# coordinates given by a matrix
\dontrun{
xy <- matrix(round(rnorm(100, 0, 5), 2), ncol=2)
getConfEll(xy, level=0.5, dstTarget=25, conversion='m2cm')
}
}
