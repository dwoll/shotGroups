\name{DFlandy02}
\alias{DFlandy02}
\docType{data}
\title{Combined bullet hole data}
\description{
Example of a data frame from one file with one group of bullet holes.}
\usage{data(DFlandy02)}
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
2 groups with 50 shots each of .22LR shot using a Stiller 2500X action at a distance of 50yd. The measurement unit for coordinates is inch, for distance yards. Group 1 is from the best of 3 lots, group 2 from worst of 3 lots. \cr
This data frame is like those returned by \code{\link[shotGroups]{readDataOT1}}, \code{\link[shotGroups]{readDataOT2}}, or \code{\link[shotGroups]{readDataMisc}} with option \code{combine=TRUE}. \cr
Data courtesy of Larry Landercasper, 2017.
}
\seealso{
\code{\link[shotGroups]{combineData}},
\code{\link[shotGroups]{analyzeGroup}},
\code{\link[shotGroups]{compareGroups}}
}
\examples{
data(DFlandy02)
str(DFlandy02)
}
\keyword{datasets}
