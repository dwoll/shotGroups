\name{getMinCircle}
\alias{getMinCircle}
\alias{getMinCircle.data.frame}
\alias{getMinCircle.default}
\title{Minimum enclosing circle for a set of 2D-points}
\description{Calculates center and radius of the minimum enclosing circle given a set of 2D-coordinates.}
\usage{getMinCircle(xy)

\method{getMinCircle}{data.frame}(xy)

\method{getMinCircle}{default}(xy)}
\arguments{
   \item{xy}{either a numerical (n x 2)-matrix with the (x,y)-coordinates of n >= 2 points (1 row of coordinates per point), or a data frame with either the variables \code{x}, \code{y} or \code{point.x}, \code{point.y}.}
}
\details{No coordinate transforms are done (unlike in \code{\link[shotGroups]{groupLocation}}, \code{\link[shotGroups]{groupShape}}, \code{\link[shotGroups]{groupSpread}}), i.e., origin is not assumed to be in top-left corner, and points are not taken relative to point of aim. \cr \cr
Uses the Skyum algorithm based on the convex hull.}
\value{
A list containing the center and radius of the circle.
\item{ctr}{a numerical 2-vector giving the (x,y)-coordinates of the circle's center.}
\item{rad}{a numerical value giving the radius of the circle.}
}
\references{
Computational Geometry Algorithms Library. 2021. CGAL Chapter 65: Bounding Volumes. \url{https://doc.cgal.org/Manual/latest/doc_html/cgal_manual/Bounding_volumes/Chapter_main.html}
Fischer, K.; Gaertner, B.; Kutz, M. 2003. Fast smallest-enclosing-ball computation in high dimensions. In: Proceedings of the 11th  European Symposium on Algorithms (ESA), 630-641. \url{https://github.com/hbf/miniball}
Gaertner, B. 2021. Miniball: Smallest Enclosing Balls of Points. \url{https://people.inf.ethz.ch/gaertner/subdir/software/miniball.html}
Skyum, S. 1991. A simple algorithm for computing the smallest enclosing circle. Information Processing Letters 37(3), 121-125.
Welzl, E. 1991. Smallest enclosing disks (balls and ellipsoids). In: Maurer H. (eds), New Results and New Trends in Computer Science 555, 359-370. doi 10.1007/BFb0038202.
}
\seealso{
\code{\link[shotGroups]{drawCircle}},
\code{\link[shotGroups]{getMinBBox}},
\code{\link[shotGroups]{getBoundingBox}}
}
\examples{
# coordinates given by a suitable data frame
mc <- getMinCircle(DFsavage)

# determine axis limits so that circle will be visible
xLims <- mc$ctr[1] + c(-mc$rad, mc$rad)
yLims <- mc$ctr[2] + c(-mc$rad, mc$rad)
plot(point.y ~ point.x, data=DFsavage,
     pch=16, asp=1, xlim=xLims, ylim=yLims)
drawCircle(mc, fg='blue')

# coordinates given by a matrix
\dontrun{
xy <- matrix(round(rnorm(20, 100, 15), 2), ncol=2)
getMinCircle(xy)
}
}
