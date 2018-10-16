\name{DF300BLKhl}
\alias{DF300BLKhl}
\docType{data}
\title{Combined bullet hole data}
\description{
Example of a data frame from one file with one group of bullet holes.}
\usage{data(DF300BLK)}
\format{
  A data frame with 60 observations on the following 9 variables.
  \describe{
    \item{\code{group}}{a factor with level \code{1}.}
    \item{\code{distance}}{a numerical vector giving the distance to the target.}
    \item{\code{aim.x}}{a numerical vector of x-coordinates giving the point of aim.}
    \item{\code{aim.y}}{a numerical vector of y-coordinates giving the of point of aim.}
    \item{\code{point.x}}{a numerical vector of absolute x-coordinates of bullet holes.}
    \item{\code{point.y}}{a numerical vector of absolute y-coordinates of bullet holes.}
    \item{\code{origin}}{a factor with level \code{1}. This factor codes from which original output file the data is.}
    \item{\code{orgser}}{a factor with levels \code{1.1}, \code{1.2}, \code{1.3}. This factor results from \code{\link{droplevels}(\link{interaction}(origin, group))}, and codes each separate group across original files. The order of the factor levels is alphabetical.}
    \item{\code{series}}{a factor with levels \code{1}, \code{2}, \code{3}. This factor codes each separate group as defined by \code{orgser}, but more conveniently as a number that runs consecutively across original files.}
    \item{\code{distance.unit}}{Measurement unit distance to target}
    \item{\code{point.unit}}{Measurement unit (x,y)-coordinates}
  }
}
\details{
Three groups of shooting a Noveske AR-15 rifle in 300BLK at 100yd with handloaded ammunition. The measurement unit for coordinates is inch, for distance yards. \cr
This data frame is like those returned by \code{\link[shotGroups]{readDataOT1}}, \code{\link[shotGroups]{readDataOT2}}, or \code{\link[shotGroups]{readDataMisc}} with option \code{combine=TRUE}. \cr
Data courtesy of David Bookstaber, 2014. \url{http://ballistipedia.com/}
}
\seealso{
\code{\link[shotGroups]{combineData}},
\code{\link[shotGroups]{analyzeGroup}},
\code{\link[shotGroups]{compareGroups}}
}
\examples{
data(DF300BLK)
str(DF300BLK)
}
\keyword{datasets}
