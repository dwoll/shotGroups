\name{groupLocation}
\alias{groupLocation}
\alias{groupLocation.data.frame}
\alias{groupLocation.default}
\title{Accuracy: Location measures for a single group of bullet holes}
\description{Calculates location measures for a single group of bullet holes.}
\usage{groupLocation(xy, level = 0.95, plots = TRUE, bootCI = 'none',
              dstTarget, conversion)

\method{groupLocation}{data.frame}(xy, level = 0.95, plots = TRUE, bootCI = 'none',
              dstTarget, conversion)

\method{groupLocation}{default}(xy, level = 0.95, plots = TRUE, bootCI = 'none',
              dstTarget, conversion)}
\arguments{
  \item{xy}{either a numerical (n x 2)-matrix with the (x,y)-coordinates of n points (1 row of coordinates per point), or a data frame with either the variables \code{x}, \code{y} or \code{point.x}, \code{point.y} as well as \code{aim.x}, \code{aim.y} giving the point of aim. If missing, point of aim is assumed to be in (0,0).}
  \item{level}{a numerical value giving the level for the confidence intervals for the center (x,y)-coordinates.}
  \item{plots}{logical: show a 2D-scatterplot?}
  \item{bootCI}{a character vector to select which bootstrap confidence interval type to report. Possible types are \code{'none'} (no bootstrap CI), \code{'norm'}, \code{'basic'}, \code{'perc'}, \code{'bca'}. See \code{\link[boot]{boot.ci}}.}
  \item{dstTarget}{a numerical value giving the distance to the target - used in MOA calculation. Acts as override if variable \code{distance} is already included in \code{xy}. See \code{\link[shotGroups]{getMOA}}.}
  \item{conversion}{how to convert the measurement unit for distance to target to that of the (x,y)-coordinates in MOA calculation. Acts as override if variables \code{dist.unit} and \code{point.unit} are already included in \code{xy}. Example \code{'m2cm'}. See \code{\link[shotGroups]{getMOA}}.}
}
\details{
The number of replicates for the reported bootstrap confidence intervals is at least 1499. If the BCa interval is reported, it is at least the number of points.

If package \code{shiny} is installed, an interactive web app for this functionality can be run with \code{runGUI("analyze")}.
}
\value{
A list with the results from the numerical analyses and statistical tests.
\item{ctr}{(x,y)-offset of group center relative to point of aim.}
\item{ctrXci}{parametric (t) and bootstrap confidence intervals for center x-coordinate.}
\item{ctrYci}{parametric (t) and bootstrap confidence intervals for center y-coordinate.}
\item{ctrRob}{robust estimate of group center offset relative to point of aim (MCD algorithm).}
\item{distPOA}{distance from group center to point of aim (in original measurement units, MOA, SMOA, milliradian).}
\item{distPOArob}{distance from robust estimate of group center to point of aim (in original measurement units, MOA, SMOA, milliradian).}
\item{Hotelling}{Hotelling's T^2-Test result from testing if group center equals point of aim.}
}
\seealso{
\code{\link[shotGroups]{getMOA}},
\code{\link[robustbase]{covMcd}},
\code{\link[stats]{anova.mlm}},
\code{\link[boot]{boot}},
\code{\link[boot]{boot.ci}}
}
\examples{
# coordinates given by a suitable data frame
res <- groupLocation(DFsavage, dstTarget=100, conversion='m2mm',
                     level=0.95, plots=2, bootCI='basic')
names(res)
res$ctr
res$distPOA
res$ctrXci
res$ctrYci

# coordinates given by a matrix
\dontrun{
# assume data from pistol shooting at 25m with 9mm ammo
# metric units
xy <- matrix(round(rnorm(100, 0, 5), 2), ncol=2)
groupLocation(xy, dstTarget=25, conversion='m2cm', plots=2)
}
}
