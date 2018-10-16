\name{combineData}
\alias{combineData}
\title{Combine list of data frames into one}
\description{Combines a list of data frames (the result from using \code{\link[shotGroups]{readDataOT1}}, \code{\link[shotGroups]{readDataOT2}}, or \code{\link[shotGroups]{readDataMisc}} with \code{combine=FALSE}) into one big data frame.}
\usage{combineData(DFs)}
\arguments{
  \item{DFs}{ a list of data frames with a shared set of variables.}
}
\details{Assumes that the data frames in the list have a non-empty set of shared variables. Among them at least either \code{point.x}, \code{point.y} or \code{x}, \code{y} defining the bullet holes. To be useful for functions \code{\link[shotGroups]{analyzeGroup}} or \code{\link[shotGroups]{compareGroups}}, the data frames should also have variables \code{group}, \code{distance}, \code{aim.x}, \code{aim.y} defining point of aim. If \code{group} is missing, it is set to 1.}
\value{
A data frame with the shared set of variables. In addition, it also contains factors identifying the original file (\code{file}), and a factor identifying all groups from different data frames (\code{series}).
\item{\ldots}{the shared set of variables from the the data frames in the list.}
\item{group}{a factor that is the original \code{group} variable as defined by OnTarget PC/TDS.}
\item{groupVerb}{a factor that codes group with more descriptive levels taken from the original project title, file name and ammunition (if available).}
\item{file}{a factor that codes from which original file the data is.}
\item{series}{a factor that codes each separate group across original files.}
\item{seriesNum}{a factor that codes each separate group as a number that runs consecutively across original files.}
}
\seealso{
\code{\link[shotGroups]{readDataMisc}},
\code{\link[shotGroups]{readDataOT1}},
\code{\link[shotGroups]{readDataOT2}},
\code{\link[shotGroups]{analyzeGroup}},
\code{\link[shotGroups]{compareGroups}}
}
\examples{
## combine list of data frames to one single data frame
data(DFlistCm)
DFcm <- combineData(DFlistCm)
str(DFcm)
head(DFcm)
}
