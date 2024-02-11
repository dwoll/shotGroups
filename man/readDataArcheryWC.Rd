\name{readDataArcheryWC}
\alias{readDataArcheryWC}
\title{Read data files from archery World Cup 2013}
\description{Reads in data from files based on results in the archery World Cup 2013. Several files can be read with one call.}
\usage{readDataArcheryWC(fPath = ".", fNames, fPat, combine = TRUE)}
\arguments{
  \item{fPath}{a character string containing the path to the folder with the SIUS output files, e.g. \code{'c:/folder/siusFiles'}.}
  \item{fNames}{a character vector containing the file names of the files that should be read in.}
  \item{fPat}{a character string containing the regular-expression that describes all names of files that should be read in. E.g., \code{'^points[[:digit:]]{2}\\\\.txt$'} for filenames \code{'points**.txt'}, where \code{**} are 2 digits. See \code{\link{regex}}, \code{\link[utils]{glob2rx}}.}
  \item{combine}{logical: combine the data into one big data frame with \code{\link[shotGroups]{combineData}}?}
}

\details{
If \code{fNames} is provided, \code{fPat} is ignored. \cr \cr
If neither \code{fNames} nor \code{fPat} is provided, and we are in interactive mode under Windows, files can be chosen interactively.\cr \cr
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
\code{\link[shotGroups]{readDataShotMarker}},
\code{\link[shotGroups]{readDataMisc}}
}
\examples{
\dontrun{
# folder with output files
fPath   <- 'c:/folder/archeryFiles'
fileNam <- c('pts01.txt', 'pts02.txt')    # desired files in that folder
DFgroup <- readDataArcheryWC(fPath, fNames=fileNam, combine=TRUE)

## alternatively, specify filename pattern for all files to be read in
fPath   <- 'c:/folder/archeryFiles'       # folder with data files
fPat    <- '^pts[[:digit:]]{2}\\\\.txt$'  # filename pattern
DFgroup <- readDataArcheryWC(fPath, fPat=pattern, combine=TRUE)
}

## result should look like this
data(DFcm)
head(DFcm)
}
