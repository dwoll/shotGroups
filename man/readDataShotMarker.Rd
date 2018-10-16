\name{readDataShotMarker}
\alias{readDataShotMarker}
\title{Read data files exported by the ShotMarker e-target system}
\description{Reads in data from files exported by the ShotMarker e-target system. Either CSV files or backup files. Several files can be read with one call.}
\usage{readDataShotMarker(fPath = ".", fNames, fPat, combine = TRUE)}
\arguments{
  \item{fPath}{a character string containing the path to the folder with the ShotMarker output files, e.g. \code{'c:/folder/smFiles'}.}
  \item{fNames}{a character vector containing the file names of the files that should be read in. Can be either plain text CSV file(s) or complete \code{.tar} backup file(s).}
  \item{fPat}{a character string containing the regular-expression that describes all names of files that should be read in. E.g., \code{'^points[[:digit:]]{2}\\\\.txt$'} for filenames \code{'points**.txt'}, where \code{**} are 2 digits. See \code{\link{regex}}, \code{\link[utils]{glob2rx}}.}
  \item{combine}{logical: combine the data into one big data frame with \code{\link[shotGroups]{combineData}}?}
}

\details{
If \code{fNames} is provided, \code{fPat} is ignored. \cr \cr
If neither \code{fNames} nor \code{fPat} is provided, and we are in interactive mode under Windows, files can be chosen interactively. \cr \cr
For CSV files, this function is basically a wrapper for \code{\link[utils]{read.csv}}. To read in backup files, package \code{jsonlite} must be installed.\cr \cr
Output files can be comma-separated files (file type \code{.csv}) from ShotMarker e-target.\cr \cr
Coordinates are stored in inch, distance is converted to yard.
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
\code{\link[shotGroups]{readDataOT2}},
\code{\link[shotGroups]{readDataSMT}},
\code{\link[shotGroups]{readDataMisc}}
}
\examples{
\dontrun{
# folder with Silver Mountain e-target output files
fPath   <- 'c:/folder/smtFiles'
fileNam <- c('pts01.csv', 'pts02.csv')    # desired files in that folder
DFgroup <- readDataShotMarker(fPath, fNames=fileNam, combine=TRUE)

## alternatively, specify filename pattern for all files to be read in
fPath   <- 'c:/folder/smtFiles'           # folder with data files
fPat    <- '^pts[[:digit:]]{2}\\\\.csv$'    # filename pattern
DFgroup <- readDataShotMarker(fPath, fPat=pattern, combine=TRUE)
}

## result should look like this
data(DFcm)
head(DFcm)
}
