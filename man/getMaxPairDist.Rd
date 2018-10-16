\name{getMaxPairDist}
\alias{getMaxPairDist}
\alias{getMaxPairDist.data.frame}
\alias{getMaxPairDist.default}
\title{Maximum pairwise distance for a set of points}
\description{Calculates the maximum of pairwise distances between points given a set of coordinates.}
\usage{getMaxPairDist(xy)

\method{getMaxPairDist}{data.frame}(xy)

\method{getMaxPairDist}{default}(xy)}
\arguments{
  \item{xy}{either a numerical (n x p)-matrix with the coordinates of n points in p-dimensional space (1 row of coordinates per point), or a data frame with either the variables \code{x}, \code{y} or \code{point.x}, \code{point.y}.}
}
\value{
A list with the following components
\item{d}{numerical value with the maximum pairwise distance between points.}
\item{idx}{a vector with two entries giving the row indices of the points that are farthest apart.}
}
\examples{
# coordinates given by a suitable data frame
(maxPD <- getMaxPairDist(DFsavage))

# plot points and point pair with maximum distance
plot(point.y ~ point.x, data=DFsavage, asp=1, pch=16)
x0 <- DFsavage$point.x[maxPD$idx[1]]     # 1st point x
y0 <- DFsavage$point.y[maxPD$idx[1]]     # 1st point y
x1 <- DFsavage$point.x[maxPD$idx[2]]     # 2nd point x
y1 <- DFsavage$point.y[maxPD$idx[2]]     # 2nd point y
segments(x0, y0, x1, y1, col="green3", lwd=2)

# coordinates given by a matrix
\dontrun{
xy <- matrix(round(rnorm(20, 100, 15), 1), ncol=2)
getMaxPairDist(xy)
}
}
