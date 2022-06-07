\name{groupSpread}
\alias{groupSpread}
\alias{groupSpread.data.frame}
\alias{groupSpread.default}
\title{Precision: Spread measures of a single group of bullet holes}
\description{Provides spread measures and their graphical representations for a single group of bullet holes.}
\usage{groupSpread(xy, center = FALSE, plots = TRUE, CEPlevel = 0.5,
            CIlevel = 0.95, CEPtype = 'CorrNormal', bootCI = 'none',
            dstTarget, conversion)

\method{groupSpread}{data.frame}(xy, center = FALSE, plots = TRUE, CEPlevel = 0.5,
            CIlevel = 0.95, CEPtype = 'CorrNormal', bootCI = 'none',
            dstTarget, conversion)

\method{groupSpread}{default}(xy, center = FALSE, plots = TRUE, CEPlevel = 0.5,
            CIlevel = 0.95, CEPtype = 'CorrNormal', bootCI = 'none',
            dstTarget, conversion)}
\arguments{
  \item{xy}{either a numerical (n x 2)-matrix with the (x,y)-coordinates of n points (1 row of coordinates per point), or a data frame with either the variables \code{x}, \code{y} or \code{point.x}, \code{point.y} as well as \code{aim.x}, \code{aim.y} giving the point of aim. If missing, point of aim is assumed to be in (0,0).}
  \item{center}{logical: center groups to mean (0,0) first? If variable \code{series} does not exist, all shots are treated as belonging to the same group. Only available in method \code{groupSpread.data.frame()}.}
  \item{plots}{logical: show diagrams?}
  \item{CEPlevel}{a numerical value giving the coverage for the CEP and for the confidence ellipse.}
  \item{CIlevel}{a numerical value giving the level for the confidence intervals (for standard deviations as well as for Rayleigh sigma, RSD, MR).}
  \item{CEPtype}{string indicating which CEP estimate to report from \code{\link[shotGroups]{getCEP}}.}
  \item{bootCI}{a character vector to select which bootstrap confidence interval type to report. Possible types are \code{'none'} (no bootstrap CI), \code{'norm'}, \code{'basic'}, \code{'perc'}, \code{'bca'}. See \code{\link[boot]{boot.ci}}.}
  \item{dstTarget}{a numerical value giving the distance to the target - used in MOA calculation. Acts as override if variable \code{distance} is already included in \code{xy}. See \code{\link[shotGroups]{getMOA}}.}
  \item{conversion}{how to convert the measurement unit for distance to target to that of the (x,y)-coordinates in MOA calculation. Acts as override if variables \code{dist.unit} and \code{point.unit} are already included in \code{xy}. Example \code{'m2cm'}. See \code{\link[shotGroups]{getMOA}}.}
}
\details{
Explanations and formula for many reported precision measures like CEP, sigma, RSD, MR, FoM can be found in the references.\cr
Robust estimate for the covariance matrix of (x,y)-coordinates is from \code{\link[robustbase]{covMcd}} using the MCD algorithm. \cr
The number of replicates for the reported bootstrap confidence intervals is at least 1499. If the BCa interval is reported, it is at least the number of points. \cr
In addition to the numerical results listed below, this function produces the following diagrams:
\itemize{
\item a scatterplot of the (x,y)-coordinates together with group center, circle with average distance to center, and 100*\code{level}\%-confidence ellipse - the latter also based on a robust estimate for the covariance matrix
\item a scatterplot of the (x,y)-coordinates together with the bounding box, minimum bounding box, minimum enclosing circle, and maximum group spread
\item a histogram of distances to group center including a fitted Rayleigh distribution as well as a non-parametric kernel density estimate
}

If package \code{shiny} is installed, an interactive web app for this functionality can be run with \code{runGUI("analyze")}.
}
\value{
A list with the results from the numerical analyses and statistical tests.
\item{sdXY}{standard deviations of x- and y-coordinates (in original measurement units, MOA, SMOA, milliradian).}
\item{sdXci}{parametric (chi^2) and bootstrap confidence intervals for the standard deviation of x-coordinates (in original measurement units, MOA, SMOA, milliradian).}
\item{sdYci}{parametric (chi^2) and bootstrap confidence intervals for the standard deviation of y-coordinates (in original measurement units, MOA, SMOA, milliradian).}
\item{sdXYrob}{robust standard deviations of x- and y-coordinates (in original measurement units, MOA, SMOA, milliradian).}
\item{covXY}{covariance matrix of (x,y)-coordinates.}
\item{covXYrob}{robust estimate of covariance matrix of (x,y)-coordinates.}
\item{distToCtr}{mean, median and maximum distance from points to their center as well as estimated Rayleigh parameters sigma (precision), radial standard deviation RSD, and mean radius MR (in original measurement units, MOA, SMOA, milliradian).}
\item{sigmaCI}{parametric (chi^2) and bootstrap confidence intervals for sigma (in original measurement units, MOA, SMOA, milliradian).}
\item{RSDci}{parametric (chi^2) and bootstrap confidence intervals for radial standard deviation RSD (number of points), in original measurement units, MOA, SMOA, milliradian).}
\item{MRci}{parametric (chi^2) and bootstrap confidence intervals for mean radius MR (in original measurement units, MOA, SMOA, milliradian).}
\item{maxPairDist}{maximum pairwise distance between points (center-to-center, = maximum spread, in original measurement units, MOA, SMOA, milliradian).}
\item{groupRect}{width and height of bounding box with diagonal and figure of merit FoM (average side length, in original measurement units, MOA, SMOA, milliradian).}
\item{groupRectMin}{width and height of minimum-area bounding box with diagonal and figure of merit FoM (average side length, in original measurement units, MOA, SMOA, milliradian).}
\item{minCircleRad}{radius for the minimum enclosing circle (in original measurement units, MOA, SMOA, milliradian).}
\item{minEll}{length of semi-major and semi-minor axis of the minimum enclosing ellipse (in original measurement units, MOA, SMOA, milliradian).}
\item{confEll}{length of semi-major and semi-minor axis of the confidence ellipse (in original measurement units, MOA, SMOA, milliradian).}
\item{confEllRob}{length of semi-major and semi-minor axis of the confidence ellipse based on a robust estimate for the covariance matrix (in original measurement units, MOA, SMOA, milliradian).}
\item{confEllShape}{aspect ratio of the confidence ellipse (square root of condition index \code{\link{kappa}}), its flattening (1 - inverse aspect ratio) as well as the trace and determinant of the covariance matrix.}
\item{confEllShapeRob}{aspect ratio and flattening of the confidence ellipse based on a robust estimate for the covariance matrix as well as its trace and determinant.}
\item{CEP}{estimate(s) for the circular error probable (CEP, in original measurement units, MOA, SMOA, milliradian).}
}
\references{
\url{http://ballistipedia.com/index.php?title=Describing_Precision}

\url{http://ballistipedia.com/index.php?title=Measuring_Precision}
}
\seealso{
\code{\link[shotGroups]{getDistToCtr}},
\code{\link[shotGroups]{getMaxPairDist}},
\code{\link[shotGroups]{getBoundingBox}},
\code{\link[shotGroups]{getMinBBox}},
\code{\link[shotGroups]{getMinCircle}},
\code{\link[shotGroups]{getConfEll}},
\code{\link[shotGroups]{getCEP}},
\code{\link[shotGroups]{getRayParam}},
\code{\link[shotGroups]{getMOA}},
\code{\link[graphics]{hist}},
\code{\link[boot]{boot}},
\code{\link[boot]{boot.ci}},
\code{\link[stats]{kernel}},
\code{\link[robustbase]{covMcd}}
}
\examples{
# coordinates given by a suitable data frame
res <- groupSpread(DFtalon, CEPtype=c('Grubbs', 'Rayleigh'), CEPlevel=0.5,
                   CIlevel=0.95, bootCI='none', dstTarget=10, conversion='m2mm')
names(res)
res$sdXYrob
res$distToCtr
res$maxPairDist
res$CEP

# coordinates given by a matrix
\dontrun{
xy <- matrix(round(rnorm(200, 0, 5), 2), ncol=2)
groupSpread(xy, CEPlevel=0.5, CIlevel=0.95, dstTarget=25, conversion='m2cm')
}
}
