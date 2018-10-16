\name{readDataOT1}
\alias{readDataOT1}
\title{Read data files exported by OnTarget PC v1.1*}
\description{Reads in data from files exported by OnTarget PC v1.1*. Several files can be read with one call.}
\usage{readDataOT1(fPath = ".", fNames, fPat, combine = TRUE,
            dstTarget, conversion)}
\arguments{
  \item{fPath}{a character string containing the path to the folder with the OnTarget PC output files. E.g., \code{'c:/folder/otFiles'}.}
  \item{fNames}{a character vector containing the file names of the files that should be read in.}
  \item{fPat}{a character string containing the regular-expression that describes all names of files that should be read in. E.g., \code{'^points[[:digit:]]{2}\\\\.txt$'} for filenames \code{'points**.txt'}, where \code{**} are 2 digits. See \code{\link{regex}}, \code{\link[utils]{glob2rx}}.}
  \item{combine}{logical: combine the data into one big data frame with \code{\link[shotGroups]{combineData}}?}
  \item{dstTarget}{a numerical value/vector giving the distance to the target - used in MOA calculation. Acts as override if variable \code{distance} is already included in the data.}
  \item{conversion}{how to convert the measurement unit for distance to target to that of the (x,y)-coordinates in MOA calculation. Example \code{'m2cm'}. See \code{\link[shotGroups]{getMOA}}.}
}
\details{
If \code{fNames} is provided, \code{fPat} is ignored. \cr \cr
If neither \code{fNames} nor \code{fPat} is provided, and we are in interactive mode under Windows, files can be chosen interactively. \cr \cr
This function is basically a wrapper for \code{\link[utils]{read.delim}}. \cr \cr
Output files need to be tab-delimited files from OnTarget PC v1.1*: 'Tools -> Export Point Data'. Files need a trailing tab in each row, and need to contain exactly the following variable names in this order: \code{Project Title}, \code{Group}, \code{Ammunition}, \code{Distance}, \code{Aim X}, \code{Aim Y}, \code{Center X}, \code{Center Y}, \code{Point X}, \code{Point Y}. \cr \cr
Tested with OnTarget PC v1.10. \cr
For reading in files exported from OnTarget PC v2.* or OnTarget TDS v3.*, see \code{\link[shotGroups]{readDataOT2}}. \cr
For reading in other text files, see \code{\link[shotGroups]{readDataMisc}}.
}
\value{
With \code{combine=FALSE}: a list of data frames, each from one file that was read in. This list can then be combined into one big data frame by \code{\link[shotGroups]{combineData}}.
\item{file1}{data frame containing data from the first file}
\item{file2}{data frame containing data from the second file}
\item{\ldots}{more data frames}
With \code{combine=TRUE}: a combined data frame from \code{\link[shotGroups]{combineData}}.
}
\seealso{
\code{\link[utils]{read.delim}},
\code{\link{regex}},
\code{\link[utils]{glob2rx}},
\code{\link[shotGroups]{combineData}},
\code{\link[shotGroups]{readDataOT2}},
\code{\link[shotGroups]{readDataSMT}},
\code{\link[shotGroups]{readDataShotMarker}},
\code{\link[shotGroups]{readDataMisc}}
}
\examples{
\dontrun{
# folder with OnTarget PC v1.1* output files
fPath   <- 'c:/folder/otFiles'
fileNam <- c('pts01.txt', 'pts02.txt')   # desired files in that folder
DFgroup <- readDataOT1(fPath, fNames=fileNam)

## alternatively, specify filename pattern for all files to be read in
fPath   <- 'c:/folder/otFiles'           # folder with data files
fPat    <- '^pts[[:digit:]]{2}\\\\.txt$' # filename pattern
DFgroup <- readDataOT1(fPath, fPat=pattern)
}

## result should look like this
data(DFcm)
head(DFcm)
}
