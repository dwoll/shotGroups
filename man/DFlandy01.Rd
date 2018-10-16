\name{DFlandy01}
\alias{DFlandy01}
\docType{data}
\title{Combined bullet hole data}
\description{
Example of a data frame from one file with one group of bullet holes.}
\usage{data(DFlandy01)}
\format{
  A data frame with 530 observations on the following 15 variables.
  \describe{
    \item{\code{group}}{a numerical vector with group numbers 1 to 53.}
    \item{\code{distance}}{a numerical vector giving the distance to the target.}
    \item{\code{aim.x}}{a numerical vector of x-coordinates giving the point of aim.}
    \item{\code{aim.y}}{a numerical vector of y-coordinates giving the of point of aim.}
    \item{\code{point.x}}{a numerical vector of absolute x-coordinates of bullet holes.}
    \item{\code{point.y}}{a numerical vector of absolute y-coordinates of bullet holes.}
    \item{\code{ammunition}}{a character vector giving ammunition and lot number.}
    \item{\code{velocity}}{a numerical vector of chronograph readings in ft/s.}
    \item{\code{control}}{undocumented.}
    \item{\code{phase1}}{undocumented.}
    \item{\code{phase2}}{undocumented.}
    \item{\code{file}}{a character vector with the original file name.}
    \item{\code{groupVerb}}{a character vector designating the group by combining the original file name and ammunition.}
    \item{\code{series}}{a character vector that codes each separate group in an alternative way.}
    \item{\code{seriesNum}}{like \code{series} but numeric.}
    \item{\code{distance.unit}}{Measurement unit distance to target}
    \item{\code{point.unit}}{Measurement unit (x,y)-coordinates}
  }
}
\details{
53 groups with 10 shots each of .22LR shot at Eley test center on Oct 2 2016 using a Stiller 2500X action at a distance of 50m. The measurement unit for coordinates is mm, for distance m. \cr
This data frame is like those returned by \code{\link[shotGroups]{readDataOT1}}, \code{\link[shotGroups]{readDataOT2}}, or \code{\link[shotGroups]{readDataMisc}} with option \code{combine=TRUE}. \cr
Data courtesy of Larry Landercasper, 2017. Analyzed by Albert Highe: \url{http://www.rimfireaccuracy.com/Forums/showthread.php/10397-Fun-with-Probabilities}
}
\seealso{
\code{\link[shotGroups]{combineData}},
\code{\link[shotGroups]{analyzeGroup}},
\code{\link[shotGroups]{compareGroups}}
}
\examples{
data(DFlandy01)
str(DFlandy01)
}
\keyword{datasets}
