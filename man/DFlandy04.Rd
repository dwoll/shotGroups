\name{DFlandy04}
\alias{DFlandy04}
\docType{data}
\title{Combined bullet hole data}
\description{
Example of a data frame from one file with one group of bullet holes.}
\usage{data(DFlandy04)}
\format{
  A data frame with 100 observations on the following 12 variables.
  \describe{
    \item{\code{group}}{a numerical vector with group numbers 1, 2.}
    \item{\code{distance}}{a numerical vector giving the distance to the target.}
    \item{\code{aim.x}}{a numerical vector of x-coordinates giving the point of aim.}
    \item{\code{aim.y}}{a numerical vector of y-coordinates giving the of point of aim.}
    \item{\code{point.x}}{a numerical vector of absolute x-coordinates of bullet holes.}
    \item{\code{point.y}}{a numerical vector of absolute y-coordinates of bullet holes.}
    \item{\code{ammunition}}{a character vector giving ammunition and lot number.}
    \item{\code{velocity}}{a numerical vector of chronograph readings in ft/s.}
    \item{\code{file}}{a character vector with the original file name.}
    \item{\code{groupVerb}}{a character vector designating the group by combining the original file name and ammunition.}
    \item{\code{series}}{a character vector that codes each separate group in an alternative way.}
    \item{\code{seriesNum}}{like \code{series} but numeric.}
    \item{\code{distance.unit}}{Measurement unit distance to target}
    \item{\code{point.unit}}{Measurement unit (x,y)-coordinates}
}}
\details{
6 groups with 25 shots each (groups 1-5) or 50 shots (group 6) of .22LR shot at a distance of 50yd. The measurement unit for coordinates is inch, for distance yards. \cr
Groups 1-3 shot with a Stiller Copperhead action with Shilen Octagon Barrel. Group 4-5 shot with a Baity Falcon action with Shilen Ratchet Barrel. Group 6 shot with a Stiller 2500X action with Shilen Octagon Barrel. \cr
This data frame is like those returned by \code{\link[shotGroups]{readDataOT1}}, \code{\link[shotGroups]{readDataOT2}}, or \code{\link[shotGroups]{readDataMisc}} with option \code{combine=TRUE}. \cr
Data courtesy of Larry Landercasper, 2017.
}
\seealso{
\code{\link[shotGroups]{combineData}},
\code{\link[shotGroups]{analyzeGroup}},
\code{\link[shotGroups]{compareGroups}}
}
\examples{
data(DFlandy04)
str(DFlandy04)
}
\keyword{datasets}
