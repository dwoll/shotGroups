\name{analyzeGroup}
\alias{analyzeGroup}
\title{Analysis for a single group of bullet holes}
\description{Performs a comprehensive numerical and graphical analysis of a single group of bullet holes.}
\usage{analyzeGroup(DF, xyTopLeft = TRUE, center = FALSE,
             dstTarget, conversion, bandW = 0.5,
             CEPtype = 'CorrNormal', bootCI = 'none')}
\arguments{
  \item{DF}{a data frame containing (at least) either the variables \code{point.x}, \code{point.y} or \code{x}, \code{y} defining the bullet holes. Variables \code{distance} (distance to target), \code{aim.x}, \code{aim.y} (point of aim) are useful - if they are missing, a warning is given and a default assumed.}
  \item{xyTopLeft}{logical: is the origin of the absolute coordinate system in the top-left corner? See details.}
  \item{center}{logical: center groups to mean (0,0) first? If variable \code{series} does not exist, all shots are treated as belonging to the same group.}
  \item{dstTarget}{a numerical value giving the distance to the target - used in MOA calculation. Acts as override if variable \code{distance} is already included in \code{DF}. See \code{\link[shotGroups]{getMOA}}.}
  \item{conversion}{how to convert the measurement unit for distance to target to that of the (x,y)-coordinates in MOA calculation. Acts as override if variables \code{dist.unit} and \code{point.unit} are already included in \code{DF}. Example \code{'m2cm'}. See \code{\link[shotGroups]{getMOA}}.}
  \item{bandW}{for argument \code{bandwith} of \code{\link[graphics]{smoothScatter}}.}
  \item{CEPtype}{string vector indicating which CEP estimate to report in \code{\link[shotGroups]{getCEP}}.}
  \item{bootCI}{a character vector to select which bootstrap confidence interval type to report. Possible types are \code{'none'} (no bootstrap CI), \code{'norm'}, \code{'basic'}, \code{'perc'}, \code{'bca'}. See \code{\link[boot]{boot.ci}}.}
}
\details{
By default, OnTarget PC/TDS' 'Export Point Data' places the origin of the absolute coordinate system in the top-left corner. In OnTarget TDS, this setting can be changed by checking the box 'Tools -> Options -> Options tab -> Data Export -> Invert Y-Axis on Export'. In that case, use \code{xyTopLeft=FALSE}. If groups appear to be upside-down, \code{xyTopLeft} is the setting to change.

Robust estimates for the group center and the covariance matrix of (x,y)-coordinates are from \code{\link[robustbase]{covMcd}} using the MCD algorithm. \cr
This function is a wrapper for \code{\link[shotGroups]{groupShape}}, \code{\link[shotGroups]{groupLocation}}, and \code{\link[shotGroups]{groupSpread}}. \cr
If the data is missing information about the point of aim, (0,0) is assumed. If distance to target is missing, 100 is assumed. \cr
The number of replicates for the reported bootstrap confidence intervals is at least 1499. If the BCa interval is reported, it is at least the number of points.

In addition to the numerical results listed below, this function produces the following diagrams:
\itemize{
\item a combined plot for multivariate outlier identification as produced by \code{\link[mvoutlier]{aq.plot}}
\item a scatterplot of the (x,y)-coordinates together with group center, circle with average distance to center, 50\%-confidence ellipse - the latter also based on a robust estimate for the covariance matrix
\item a scatterplot of the (x,y)-coordinates together with the minimum bounding box, minimum enclosing circle, and maximum group spread
\item a chi-square Q-Q-plot for eyeballing multivariate normality as produced by \code{\link[mvoutlier]{chisq.plot}}, including a reference line with intercept 0 and slope 1
\item a heatmap of a 2D-kernel density estimate for the (x,y)-coordinates as produced by \code{\link[graphics]{smoothScatter}} together with group center and error ellipse based on a robust estimate for the covariance matrix
\item a Q-Q-plot of x-coordinates for eyeballing normality
\item a Q-Q-plot of y-coordinates for eyeballing normality
\item a histogram of x-coordinates including a fitted normal distribution as well as a non-parametric kernel density estimate
\item a histogram of y-coordinates including a fitted normal distribution as well as a non-parametric kernel density estimate
\item a histogram of distances to group center including a fitted Rayleigh distribution as well as a non-parametric kernel density estimate
}
}
\value{
A list with the results from the numerical analyses and statistical tests.
\item{corXY}{correlation matrix of (x,y)-coordinates.}
\item{corXYrob}{robust estimate of correlation matrix of (x,y)-coordinates.}
\item{Outliers}{a vector of row indices for observations identified as outliers.}
\item{ShapiroX}{Shapiro-Wilk-Test result for normality of x-coordinates.}
\item{ShapiroY}{Shapiro-Wilk-Test result for normality of y-coordinates.}
\item{multNorm}{E-statistic-Test result for multivariate normality of (x,y)-coordinates.}

\item{sdXY}{standard deviations of x- and y-coordinates (in original measurement units, MOA, SMOA, milliradian).}
\item{sdXci}{parametric and bootstrap confidence intervals for the standard deviation of x-coordinates (in original measurement units, MOA, SMOA, milliradian).}
\item{sdYci}{parametric and bootstrap confidence intervals for the standard deviation of y-coordinates (in original measurement units, MOA, SMOA, milliradian).}
\item{sdXYrob}{robust standard deviations of x- and y-coordinates (in original measurement units, MOA, SMOA, milliradian).}
\item{covXY}{covariance matrix of (x,y)-coordinates.}
\item{covXYrob}{robust estimate of covariance matrix of (x,y)-coordinates.}
\item{distToCtr}{mean and median distance from points to their center as well as estimated Rayleigh parameters sigma (precision), radial standard deviation RSD, and mean radius MR (in original measurement units, MOA, SMOA, milliradian).}
\item{sigmaCI}{95\%-parametric and bootstrap confidence intervals for sigma (in original measurement units, MOA, SMOA, milliradian).}
\item{RSDci}{95\%-parametric and bootstrap confidence intervals for radial standard deviation RSD (in original measurement units, MOA, SMOA, milliradian).}
\item{MRci}{95\%-parametric and bootstrap confidence intervals for mean radius MR (in original measurement units, MOA, SMOA, milliradian).}
\item{maxPairDist}{maximum pairwise distance between points (center-to-center, = maximum spread, in original measurement units, MOA, SMOA, milliradian).}
\item{groupRect}{width and height of bounding box with diagonal and figure of merit FoM (average side length, in original measurement units, MOA, SMOA, milliradian).}
\item{groupRectMin}{width and height of minimum-area bounding box with diagonal and figure of merit FoM (average side length, in original measurement units, MOA, SMOA, milliradian).}
\item{minCircleRad}{radius for the minimum enclosing circle (in original measurement units, MOA, SMOA, milliradian).}
\item{confEll}{length of semi-major and semi-minor axis of the 50\%-confidence ellipse (in original measurement units, MOA, SMOA, milliradian).}
\item{confEllRob}{length of semi-major and semi-minor axis of the 50\%-confidence ellipse based on a robust estimate for the covariance matrix (in original measurement units, MOA, SMOA, milliradian).}
\item{confEllShape}{aspect ratio and flattening of the 50\%-confidence ellipse.}
\item{confEllShapeRob}{aspect ratio and flattening of the 50\%-confidence ellipse based on a robust estimate for the covariance matrix.}
\item{CEP}{estimate(s) for the 50\%-circular error probable (CEP, in original measurement units, MOA, SMOA, milliradian).}
\item{ctr}{(x,y)-offset of group center relative to point of aim.}
\item{ctrXci}{95\%-parametric and bootstrap confidence intervals for center x-coordinate.}
\item{ctrYci}{95\%-parametric and bootstrap confidence intervals for center y-coordinate.}
\item{ctrRob}{robust estimate of group center offset relative to point of aim (MCD algorithm).}
\item{distPOA}{distance from group center to point of aim (in original measurement units, MOA, SMOA, milliradian).}
\item{distPOArob}{distance from robust estimate of group center to point of aim (in original measurement units, MOA, SMOA, milliradian).}
\item{Hotelling}{Hotelling's T^2-Test result from testing if group center equals point of aim.}
}
\seealso{
\code{\link[shotGroups]{groupShape}},
\code{\link[shotGroups]{groupLocation}},
\code{\link[shotGroups]{groupSpread}},
\code{\link[shotGroups]{compareGroups}},
\code{\link[shotGroups]{getDistToCtr}},
\code{\link[shotGroups]{getMaxPairDist}},
\code{\link[shotGroups]{getBoundingBox}},
\code{\link[shotGroups]{getMinBBox}},
\code{\link[shotGroups]{getMinCircle}},
\code{\link[shotGroups]{getConfEll}},
\code{\link[shotGroups]{getCEP}},
\code{\link[shotGroups]{getRayParam}},
\code{\link[shotGroups]{getMOA}},
\code{\link[graphics]{smoothScatter}},
\code{\link[mvoutlier]{chisq.plot}},
\code{\link[mvoutlier]{aq.plot}},
\code{\link[mvoutlier]{pcout}},
\code{\link[stats]{qqnorm}},
\code{\link[graphics]{hist}},
\code{\link[stats]{kernel}},
\code{\link[stats]{shapiro.test}},
\code{\link[energy]{mvnorm.etest}},
\code{\link[stats]{anova.mlm}},
\code{\link[boot]{boot}},
\code{\link[boot]{boot.ci}},
\code{\link[robustbase]{covMcd}}
}
\examples{
data(DFinch)

# select combined data from only first 2 series
DF  <- subset(DFinch, series \%in\% 1:2)
res <- analyzeGroup(DF, conversion='yd2in', bootCI='none')
names(res)
res$multNorm
res$corXY
res$ctrRob
res$ctrXci
res$ctrYci
}
