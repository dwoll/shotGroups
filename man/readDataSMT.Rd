\name{readDataSMT}
\alias{readDataSMT}
\title{Read data files exported by the Silver Mountain e-target system}
\description{Reads in data from CSV files exported by the Silver Mountain e-target system. Several files can be read with one call.}
\usage{readDataSMT(fPath = ".", fNames, fPat, combine = TRUE)}
\arguments{
  \item{fPath}{a character string containing the path to the folder with the SMT output files, e.g. \code{'c:/folder/smtFiles'}.}
  \item{fNames}{a character vector containing the file names of the files that should be read in.}
  \item{fPat}{a character string containing the regular-expression that describes all names of files that should be read in. E.g., \code{'^points[[:digit:]]{2}\\\\.txt$'} for filenames \code{'points**.txt'}, where \code{**} are 2 digits. See \code{\link{regex}}, \code{\link[utils]{glob2rx}}.}
  \item{combine}{logical: combine the data into one big data frame with \code{\link[shotGroups]{combineData}}?}
}

\details{
If \code{fNames} is provided, \code{fPat} is ignored. \cr \cr
If neither \code{fNames} nor \code{fPat} is provided, and we are in interactive mode under Windows, files can be chosen interactively. \cr \cr
This function is basically a wrapper for \code{\link[utils]{read.csv}}. \cr \cr
Output files need to be comma-separated files (file type \code{.csv}) from Silver Mountain e-target. Files need to contain exactly the following variable names in this order: \code{string}, \code{shooter}, \code{frame}, \code{distance}, \code{date}, \code{score}, \code{moa_x}, \code{moa_y}, \code{scope_x}, \code{scope_y}, \code{adj_x}, \code{adj_y}, \code{v}, \code{adj_y_avg}, \code{adj_y_sd}, \code{v_avg}, \code{v_sd}. \cr \cr
Coordinates are converted from MOA to inch, distance is converted from meter to yard. \cr \cr
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
\code{\link[shotGroups]{readDataOT2}},
\code{\link[shotGroups]{readDataShotMarker}},
\code{\link[shotGroups]{readDataMisc}}
}
\examples{
\dontrun{
# folder with Silver Mountain e-target output files
fPath   <- 'c:/folder/smtFiles'
fileNam <- c('pts01.csv', 'pts02.csv')    # desired files in that folder
DFgroup <- readDataSMT(fPath, fNames=fileNam, combine=TRUE)

## alternatively, specify filename pattern for all files to be read in
fPath   <- 'c:/folder/smtFiles'           # folder with data files
fPat    <- '^pts[[:digit:]]{2}\\\\.csv$'    # filename pattern
DFgroup <- readDataSMT(fPath, fPat=pattern, combine=TRUE)
}

## result should look like this
data(DFcm)
head(DFcm)
}
