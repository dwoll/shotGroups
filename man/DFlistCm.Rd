\name{DFlistCm}
\alias{DFlistCm}
\docType{data}
\title{List containing several data frames with bullet hole data}
\description{
Example list containing several data frames with bullet hole data as produced by \code{\link[shotGroups]{readDataOT1}}, \code{\link[shotGroups]{readDataOT2}}, or by \code{\link[shotGroups]{readDataMisc}} with option \code{combine=FALSE}.
}
\usage{data(DFlistCm)}
\details{
Several groups of shooting a 9x19mm pistol at 25m. The measurement unit for coordinates is cm, for distance meters. \cr
This list can be used as an argument for \code{\link[shotGroups]{combineData}}.
}
\seealso{
\code{\link[shotGroups]{combineData}},
\code{\link[shotGroups]{readDataMisc}},
\code{\link[shotGroups]{readDataOT1}},
\code{\link[shotGroups]{readDataOT2}}
}
\examples{
data(DFlistCm)
str(DFlistCm)

## combine list of data frames to one single data frame
DFcm <- combineData(DFlistCm)
str(DFcm)
}
\keyword{datasets}
