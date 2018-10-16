\name{getBoundingBox}
\alias{getBoundingBox}
\alias{getBoundingBox.data.frame}
\alias{getBoundingBox.default}
\title{Bounding box for a set of 2D-points}
\description{Calculates the vertices of the (axis-parallel) bounding box given a set of 2D-coordinates.}
\usage{getBoundingBox(xy)

\method{getBoundingBox}{data.frame}(xy)

\method{getBoundingBox}{default}(xy)}
\arguments{
  \item{xy}{either a numerical (n x 2)-matrix with the (x,y)-coordinates of n points (1 row of coordinates per point), or a data frame with either the variables \code{x}, \code{y} or \code{point.x}, \code{point.y}.}
}
\details{No coordinate transforms are done (unlike in \code{\link[shotGroups]{groupLocation}}, \code{\link[shotGroups]{groupShape}}, \code{\link[shotGroups]{groupSpread}}), i.e., origin is not assumed to be in top-left corner, and points are not taken relative to point of aim.}
\value{
A list with the following information about the bounding box:
  \item{pts}{a numerical 4-vector giving the coordinates \code{xleft}, \code{ybottom}, \code{xright}, \code{ytop}.}
  \item{width}{width of the box.}
  \item{height}{height of the box.}
  \item{FoM}{figure of merit, i.e., the average side length of the box: (\code{width} + \code{height}) / 2.}
  \item{diag}{length of box diagonal.}
}
\seealso{
\code{\link[shotGroups]{drawBox}},
\code{\link[shotGroups]{getMinBBox}},
\code{\link[shotGroups]{getMinCircle}}
}
\examples{
# coordinates given by a suitable data frame
bb <- getBoundingBox(DFsavage)

# draw points and bounding box
plot(point.y ~ point.x, data=DFsavage, asp=1, pch=16)
drawBox(bb, fg='blue', colCtr='blue', pch=4, cex=2)

bb$FoM                                   # figure of merit

# coordinates given by a matrix
\dontrun{
xy <- matrix(round(rnorm(20, 100, 15), 1), ncol=2)
getBoundingBox(xy)
}
}

