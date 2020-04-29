\name{shotGroups-package}
\alias{shotGroups-package}
\alias{shotGroups}
\docType{package}
\title{Analyze shot group data}
\description{The \code{shotGroups} package provides functions to read in, plot, statistically describe, analyze, and compare shooting data with respect to group shape, precision, and accuracy. This includes graphical methods, descriptive statistics, and inference tests using standard, but also non-parametric and robust statistical methods. Works with files exported by 'OnTarget PC/TDS', 'Silver Mountain' e-target, 'ShotMarker' e-target, or 'Taran', as well as with custom data files in text format. Supports inference from range statistics like extreme spread. Includes web-based graphical user interface.}
\details{
\tabular{ll}{
Package: \tab shotGroups\cr
Type: \tab Package\cr
Version: \tab 0.7.5.1\cr
Date: \tab 2019-07-15\cr
Depends: \tab R (>= 3.4.0)\cr
Imports: \tab boot, coin, CompQuadForm (>= 1.4.2), graphics, grDevices, KernSmooth, robustbase, stats, tools, utils\cr
Suggests: \tab knitr, energy, mvoutlier, shiny, jsonlite, interp, MBA\cr
License: \tab GPL (>= 2)\cr
}
Use \code{help(package='shotGroups')} for a list of all functions and links to the detailed help pages with information on options, usage and output. For further explanantions and an example walkthrough, see \code{vignette('shotGroups')}.
}
\author{
Daniel Wollschlaeger

Maintainer: Daniel Wollschlaeger \email{dwoll@kuci.org}
}
\keyword{
package
}
\examples{
groupSpread(DFcciHV, dstTarget=100, conversion='yd2in', bootCI='none')
}
