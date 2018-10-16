\name{getXYmat}
\alias{getXYmat}
\title{Extract (x,y)-coordinates (relative to point of aim) from a data frame}
\description{Extracts (x,y)- or (x,y,z)-coordinates of the points of impact (relative to the point of aim) from a data frame and returns them as a matrix.}
\usage{getXYmat(DF, xyTopLeft = TRUE, relPOA = TRUE, center = FALSE)}
\arguments{
  \item{DF}{a data frame containing (at least) either the variables \code{point.x}, \code{point.y} or \code{x}, \code{y} defining the bullet holes. For three-dimensional data, variables \code{point.z} or \code{z} are recognized. Variables \code{aim.x}, \code{aim.y}, and for three-dimensional data \code{point.z} can define the point of aim. If missing, point of aim is assumed to be in the origin 0.}
  \item{xyTopLeft}{logical: is the origin of the absolute coordinate system in the top-left corner? See details.}
  \item{relPOA}{logical: should returned coordinates be relative to the point of aim?}
  \item{center}{logical: center groups to mean (0,0)? If variable \code{series} does not exist, all shots are treated as belonging to the same group.}
}
\seealso{
\code{\link[shotGroups]{groupLocation}},
\code{\link[shotGroups]{groupShape}},
\code{\link[shotGroups]{groupSpread}}
}
\details{
By default, OnTarget PC/TDS' 'Export Point Data' places the origin of the absolute coordinate system in the top-left corner. In OnTarget TDS, this setting can be changed by checking the box 'Tools -> Options -> Options tab -> Data Export -> Invert Y-Axis on Export'. In that case, use \code{xyTopLeft=FALSE}. If groups appear to be upside-down, \code{xyTopLeft} is the setting to change.
}
\value{A numerical matrix with the (x,y)- or (x,y,z)-coordinates.}
\examples{
data(DFcm)

# select data from only first series
DFsub <- subset(DFcm, series == 1)
getXYmat(DFsub, xyTopLeft=TRUE, relPOA=TRUE)
}
