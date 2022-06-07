\name{getMinEllipse}
\alias{getMinEllipse}
\alias{getMinEllipse.data.frame}
\alias{getMinEllipse.default}
\title{Minimum enclosing ellipse for a set of 2D-points}
\description{Calculates center, shape matrix, and area of the minimum enclosing ellipse given a set of 2D-coordinates using Khachiyan's algorithm.}
\usage{getMinEllipse(xy, tol = 0.001, max_iter = 1000)

\method{getMinEllipse}{data.frame}(xy, tol = 0.001, max_iter = 1000)

\method{getMinEllipse}{default}(xy, tol = 0.001, max_iter = 1000)
}
\arguments{
   \item{xy}{either a numerical (n x 2)-matrix with the (x,y)-coordinates of n >= 2 points (1 row of coordinates per point), or a data frame with either the variables \code{x}, \code{y} or \code{point.x}, \code{point.y}.}
   \item{tol}{numerical tolerance value. Should be < 1.}
   \item{max_iter}{maximum number of iterations.}
}
\details{No coordinate transforms are done (unlike in \code{\link[shotGroups]{groupLocation}}, \code{\link[shotGroups]{groupShape}}, \code{\link[shotGroups]{groupSpread}}), i.e., origin is not assumed to be in top-left corner, and points are not taken relative to point of aim.}
\value{
A list containing the center, (2 x 2)-shape matrix, and area of the ellipse.
\item{ctr}{a numerical 2-vector giving the (x,y)-coordinates of the ellipse's center.}
\item{E}{a numerical positive definite (2 x 2)-matrix defining the ellipse in the form ((x-c)' E (x-c)) <= 1 }
\item{cov}{matrix, whose eigen-structure determines shape of ellipse. Inverse of E. Useful for plotting with \code{\link[shotGroups]{drawEllipse}}.}
\item{area}{a numerical value giving the area of the ellipse.}
\item{shape}{a numerical vector with the orientation of the ellipse's major axis pointing up as returned by \code{\link{atan2}} (but in degree), the aspect ratio of the ellipse (square root of condition index \code{\link{kappa}}), its flattening (1 - inverse aspect ratio) as well as the trace and determinant of the covariance matrix.}
\item{size}{a numerical vector with the lengths of the semi-axes of the ellipse.}
}
\references{
Computational Geometry Algorithms Library. 2021. CGAL Chapter 65: Bounding Volumes. \url{https://doc.cgal.org/Manual/latest/doc_html/cgal_manual/Bounding_volumes/Chapter_main.html}

Todd MJ and Yildrim EA. On Khachiyan's Algorithm for the Computation of Minimum Volume Enclosing Ellipsoids. Discrete Applied Mathematics 2007: 155: 1731-1744. \url{https://people.orie.cornell.edu/miketodd/TYKhach.pdf}

Jacob. Bounding ellipse. \url{https://stackoverflow.com/a/1768440}.
}
\seealso{
\code{\link[shotGroups]{drawEllipse}},
\code{\link[shotGroups]{getMinCircle}},
\code{\link[shotGroups]{getMinBBox}},
\code{\link[shotGroups]{getBoundingBox}}
}
\examples{
# coordinates given by a suitable data frame
me <- getMinEllipse(DFsavage, tol=0.001)

plot(point.y ~ point.x, data=DFsavage, pch=16, asp=1)
drawEllipse(me, fg='blue')

# coordinates given by a matrix
\dontrun{
xy <- matrix(round(rnorm(20, 100, 15), 2), ncol=2)
getMinEllipse(xy, tol=0.001)
}
}
