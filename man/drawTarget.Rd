\name{drawTarget}
\alias{drawTarget}
\title{Draw a target pattern}
\description{Draws a target pattern - either from the name of a pre-specified target type or from a supplied list defining the target.}
\usage{drawTarget(x, unit, dstTarget, conversion,
           add = FALSE, cex = par('cex'))}
\arguments{
  \item{x}{either a character value with the name of a target in \code{\link[shotGroups]{targets}} or a list with a target definition containing the same components as those in \code{\link[shotGroups]{targets}} (see below).}
  \item{unit}{the measurement unit that should be used in the plot. Possible values are \code{'cm', 'mm', 'm', 'in', 'ft', 'yd', 'deg', 'MOA', 'SMOA', 'rad', 'mrad', 'mil'}.}
  \item{dstTarget}{a numerical value with the distance to the target - used in MOA calculation. See \code{\link[shotGroups]{getMOA}}.}
  \item{conversion}{how to convert the measurement unit for distance to target to that of the (x,y)-coordinates - used in MOA calculation. Example \code{'m2cm'}. See \code{\link[shotGroups]{getMOA}}.}
  \item{add}{logical: add to existing plot or create new plot?}
  \item{cex}{numerical value specifying the magnification factor for plotting the ring numbers.}
}
\value{
Invisibly returns a list with (at least) the following components defining the target:
  \item{name}{target name.}
  \item{unitTarget}{measurement unit for ring diameters and radii.}
  \item{nRings}{number of rings.}
  \item{maxCount}{highest ring count for scoring.}
  \item{ringD10}{diameter of ring number 10 (highest-valued ring).}
  \item{ringD10i}{diameter of sub-division of ring number 10 (Innenzehn). If target has no sub-division, equal to \code{ringD10}.}
  \item{ringW}{width of the remaining rings number 9, 8, 7, ...}
  \item{cols}{\code{nRings}+1 colors of the rings - right half of the target, starting with the sub-division of ring number 10 and going outwards.}
  \item{colsTxt}{\code{nRings}-1 colors of the ring numbers, starting with ring number 9 and going outwards.}
  \item{ringR}{\code{nRings}+1 ring radii, including sub-division of ring number 10.}
  \item{unitConv}{measurement unit for \code{ringD10u}, \code{ringD10iu}, \code{ringWu}, \code{ringRu} as defined by \code{unit}.}
  \item{ringD10u}{diameter of ring number 10 converted to \code{unit}.}
  \item{ringD10iu}{diameter of sub-division of ring number 10 (Innenzehn) converted to \code{unit}.}
  \item{ringWu}{width of the remaining rings number 9, 8, 7, ... converted to \code{unit}.}
  \item{ringRu}{\code{nRings}+1 ring radii, including sub-division of ring number 10, converted to \code{unit}.}
}
\seealso{
\code{\link[shotGroups]{targets}},
\code{\link[shotGroups]{drawGroup}}
}
\examples{
# draw ISSF 300m target in inch
trgt <- drawTarget('ISSF_300m', unit='in')

# target definition
trgt
}
