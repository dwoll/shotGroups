\name{runGUI}
\alias{runGUI}
\title{Open web-based GUI in browser}
\description{
Opens one of four web-based GUIs for \code{shotGroups} functionality in an external browser.
}
\usage{
runGUI(app=c("analyze", "hitprob", "range", "angular"), ...)
}
\arguments{
  \item{app}{character string. One of \code{"analyze"} - shot group analysis based on data with (x,y)-coordinates of shots, \code{"hitprob"} - calculate hit probability within a region, \code{"range"} - use measured range statistics to estimate Rayleigh sigma parameter or get required number of groups to achieve a desired CI width, \code{"angular"} - angular size conversions}
  \item{...}{arguments passed to \code{\link[shiny]{runApp}}. Supply \code{port=80} if a web browser refuses to connect to the randomly chosen port for security reasons. Requires installing package\code{shiny} first.}
}
\details{
Requires installing package \code{shiny} first. This function calls \code{\link[shiny]{runApp}} to run the included shotGroupsApp application.
}
\seealso{
\code{\link[shiny]{runApp}}
}
\examples{
\dontrun{
runGUI(app="analzye")
}
}
