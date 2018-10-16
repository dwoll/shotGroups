\name{DFcm}
\alias{DFcm}
\docType{data}
\title{Combined bullet hole data}
\description{
Example of a combined data frame from several files exported by OnTarget PC/TDS, each with several groups of bullet holes.}
\usage{data(DFcm)}
\format{
  A data frame with 487 observations on the following 13 variables.
  \describe{
    \item{\code{project.title}}{a character vector giving the OnTarget PC/TDS project title.}
    \item{\code{group}}{a factor with levels \code{1} \code{2} \code{3}. This is the original \code{Group} variable as defined by OnTarget PC/TDS.}
    \item{\code{ammunition}}{a character vector describing the ammo.}
    \item{\code{distance}}{a numerical vector giving the distance to the target.}
    \item{\code{aim.x}}{a numerical vector of x-coordinates giving the point of aim.}
    \item{\code{aim.y}}{a numerical vector of y-coordinates giving the of point of aim.}
    \item{\code{center.x}}{a numerical vector of x-coordinates giving the group centers.}
    \item{\code{center.y}}{a numerical vector of y-coordinates giving the group centers.}
    \item{\code{point.x}}{a numerical vector of absolute x-coordinates of bullet holes.}
    \item{\code{point.y}}{a numerical vector of absolute y-coordinates of bullet holes.}
    \item{\code{origin}}{a factor with levels \code{1} \code{2} \code{3}. This factor codes from which original output file the data is.}
    \item{\code{orgser}}{a factor with levels \code{1.1} \code{2.1} \code{3.1} \code{1.2} \code{2.2} \code{3.2} \code{1.3} \code{2.3} \code{3.3}. This factor results from \code{\link{droplevels}(\link{interaction}(origin, group))}, and codes each separate group across original files. The order of the factor levels is alphabetical.}
    \item{\code{series}}{a factor with levels \code{1} \code{2} \code{3} \code{4} \code{5} \code{6} \code{7} \code{8} \code{9}. This factor codes each separate group as defined by \code{orgser}, but more conveniently as a number that runs consecutively across original files.}
    \item{\code{distance.unit}}{Measurement unit distance to target}
    \item{\code{point.unit}}{Measurement unit (x,y)-coordinates}
    \item{\code{target}}{Character string \code{"BDS9"} indicating the target face. See \code{\link[shotGroups]{targets}}.}
 }
}
\details{
Several groups of shooting a 9x19mm pistol at 25m. The measurement unit for coordinates is cm, for distance meters. \cr
This data frame is like those returned by \code{\link[shotGroups]{readDataOT1}}, \code{\link[shotGroups]{readDataOT2}}, or \code{\link[shotGroups]{readDataMisc}} with option \code{combine=TRUE}.
}
\seealso{
\code{\link[shotGroups]{combineData}},
\code{\link[shotGroups]{analyzeGroup}},
\code{\link[shotGroups]{compareGroups}}
}
\examples{
data(DFcm)
str(DFcm)
}
\keyword{datasets}
