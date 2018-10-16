\name{getMinBBox}
\alias{getMinBBox}
\alias{getMinBBox.data.frame}
\alias{getMinBBox.default}
\title{Minimum-area bounding box for a set of 2D-points}
\description{Calculates the vertices of the minimum-area, possibly oriented bounding box given a set of 2D-coordinates.}
\usage{getMinBBox(xy)

\method{getMinBBox}{data.frame}(xy)

\method{getMinBBox}{default}(xy)}
\arguments{
  \item{xy}{either a numerical (n x 2)-matrix with the (x,y)-coordinates of n >= 2 points (1 row of coordinates per point), or a data frame with either the variables \code{x}, \code{y} or \code{point.x}, \code{point.y}.}
}
\details{No coordinate transforms are done (unlike in \code{\link[shotGroups]{groupLocation}}, \code{\link[shotGroups]{groupShape}}, \code{\link[shotGroups]{groupSpread}}), i.e., origin is not assumed to be in top-left corner, and points are not taken relative to point of aim.\cr
Uses the rotating calipers algorithm (Toussaint, 1983).}
\value{
A list with the following information about the minimum-area bounding box:
\item{pts}{a (4 x 2)-matrix containing the coordinates of the (ordered) vertices.}
\item{width}{width of the box.}
\item{height}{height of the box.}
\item{FoM}{figure of merit, i.e., the average side length of the box: (\code{width} + \code{height}) / 2.}
\item{diag}{length of box diagonal.}
\item{angle}{orientation of the box' longer edge pointing up as returned by \code{\link{atan2}}, but in degree.}
}
\references{
Toussaint, G. T. (1983). Solving geometric problems with the rotating calipers. In Proceedings of the 1983 IEEE MELECON. Athens, Greece: IEEE Computer Society.
}
\seealso{
\code{\link[shotGroups]{drawBox2}},
\code{\link[shotGroups]{getBoundingBox}},
\code{\link[shotGroups]{getMinCircle}}
}
\examples{
# coordinates given by a suitable data frame
bb <- getMinBBox(DFsavage)               # minimum bounding box

# plot points and minimum bounding box
plot(point.y ~ point.x, data=DFsavage, asp=1,
     xlim=range(bb$pts[ , 1]), ylim=range(bb$pts[ , 2]), pch=16)
drawBox2(bb, fg='blue', colCtr='blue', pch=4, cex=2)

bb$FoM                                   # figure of merit
bb$angle                                 # box orientation

# coordinates given by a matrix
\dontrun{
xy <- matrix(round(rnorm(16, 100, 15)), ncol=2)
getMinBBox(xy)
}
}

