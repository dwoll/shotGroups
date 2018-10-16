\name{readDataOT2}
\alias{readDataOT2}
\title{Read data files exported by OnTarget PC v2.* or OnTarget TDS v3.*}
\description{Reads in data from files exported by OnTarget PC v2.* or OnTarget TDS v3.*. Several files can be read with one call.}
\usage{readDataOT2(fPath = ".", fNames, fPat, combine = TRUE,
            dstTarget, conversion)}
\arguments{
  \item{fPath}{a character string containing the path to the folder with the OnTarget PC/TDS output files, e.g. \code{'c:/folder/otFiles'}.}
  \item{fNames}{a character vector containing the file names of the files that should be read in.}
  \item{fPat}{a character string containing the regular-expression that describes all names of files that should be read in. E.g., \code{'^points[[:digit:]]{2}\\\\.txt$'} for filenames \code{'points**.txt'}, where \code{**} are 2 digits. See \code{\link{regex}}, \code{\link[utils]{glob2rx}}.}
  \item{combine}{logical: combine the data into one big data frame with \code{\link[shotGroups]{combineData}}?}
  \item{dstTarget}{a numerical value/vector giving the distance to the target - used in MOA calculation. Acts as override if variable \code{distance} is already included in the data.}
  \item{conversion}{how to convert the measurement unit for distance to target to that of the (x,y)-coordinates in MOA calculation. Example \code{'m2cm'}. See \code{\link[shotGroups]{getMOA}}.}
}
\details{
If \code{fNames} is provided, \code{fPat} is ignored. \cr \cr
If neither \code{fNames} nor \code{fPat} is provided, and we are in interactive mode under Windows, files can be chosen interactively. \cr \cr
This function is basically a wrapper for \code{\link[utils]{read.csv}}. \cr \cr
Output files need to be comma-separated files (file type \code{.csv}) from OnTarget PC v2.* or OnTarget TDS v3.*: 'Tools -> Export Point Data'. Files need to contain exactly the following variable names in this order: \code{Project Title}, \code{Group}, \code{Ammunition}, \code{Distance}, \code{Aim X}, \code{Aim Y}, \code{Center X}, \code{Center Y}, \code{Point X}, \code{Point Y}, and optionally \code{Velocity}. \cr \cr
Tested with OnTarget PC v2.10 and v2.28 as well as OnTarget TDS v3.71, v3.89, v6.09.\cr
For reading in files exported from OnTarget PC v1.*, see \code{\link[shotGroups]{readDataOT1}}. \cr
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
\code{\link[utils]{read.csv}},
\code{\link{regex}},
\code{\link[utils]{glob2rx}},
\code{\link[shotGroups]{combineData}},
\code{\link[shotGroups]{readDataOT1}},
\code{\link[shotGroups]{readDataSMT}},
\code{\link[shotGroups]{readDataShotMarker}},
\code{\link[shotGroups]{readDataMisc}}
}
\examples{
\dontrun{
# folder with OnTarget PC v2.* or OnTarget TDS v3.* output files
fPath   <- 'c:/folder/otFiles'
fileNam <- c('pts01.csv', 'pts02.csv')   # desired files in that folder
DFgroup <- readDataOT2(fPath, fNames=fileNam, combine=TRUE)

## alternatively, specify filename pattern for all files to be read in
fPath   <- 'c:/folder/otFiles'            # folder with data files
fPat    <- '^pts[[:digit:]]{2}\\\\.txt$'  # filename pattern
DFgroup <- readDataOT2(fPath, fPat=pattern, combine=TRUE)
}

## result should look like this
data(DFcm)
head(DFcm)
}
