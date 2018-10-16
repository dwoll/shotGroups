\name{getDistToCtr}
\alias{getDistToCtr}
\alias{getDistToCtr.data.frame}
\alias{getDistToCtr.default}
\title{Distances to center for a set of points}
\description{Calculates the distances of a set of points to their center.}
\usage{getDistToCtr(xy)

\method{getDistToCtr}{data.frame}(xy)

\method{getDistToCtr}{default}(xy)}
\arguments{
  \item{xy}{either a numerical (n x p)-matrix with the coordinates of n points in p-dimensional space (1 row of coordinates per point), or a data frame with either the variables \code{x}, \code{y} or \code{point.x}, \code{point.y}.}
}
\value{A numerical vector with the distances from each point to the center of the set.}
\examples{
# coordinates given by a suitable data frame
getDistToCtr(DFtalon)

# coordinates given by a matrix
\dontrun{
xy <- matrix(round(rnorm(20, 100, 15), 1), ncol=2)
getDistToCtr(xy)
}
}
