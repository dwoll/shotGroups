\name{drawGroup}
\alias{drawGroup}
\alias{drawGroup.data.frame}
\alias{drawGroup.default}
\title{Draw a group of bullet holes with additional measures}
\description{Draws a group with scaled bullet holes on a target background. Spread measures can be selected individually.}
\usage{drawGroup(xy, center = FALSE, xyTopLeft = TRUE,
          bb = FALSE, bbMin = FALSE, bbDiag = FALSE,
          minCirc = FALSE, maxSpread = FALSE, meanDist = FALSE, confEll = FALSE,
          CEP = FALSE, ringID = FALSE, valueID = TRUE, doRob = FALSE,
          level = 0.95, scaled = TRUE, caliber = 9, dstTarget, conversion,
          unit = 'unit', alpha = 0.5, target)

\method{drawGroup}{data.frame}(xy, center = FALSE, xyTopLeft = TRUE,
          bb = FALSE, bbMin = FALSE, bbDiag = FALSE,
          minCirc = FALSE, maxSpread = FALSE, meanDist = FALSE, confEll = FALSE,
          CEP = FALSE, ringID = FALSE, valueID = TRUE, doRob = FALSE,
          level = 0.95, scaled = TRUE, caliber = 9, dstTarget, conversion,
          unit = 'unit', alpha = 0.5, target)

\method{drawGroup}{default}(xy, center = FALSE, xyTopLeft = TRUE,
          bb = FALSE, bbMin = FALSE, bbDiag = FALSE,
          minCirc = FALSE, maxSpread = FALSE, meanDist = FALSE, confEll = FALSE,
          CEP = FALSE, ringID = FALSE, valueID = TRUE, doRob = FALSE,
          level = 0.95, scaled = TRUE, caliber = 9, dstTarget, conversion,
          unit = 'unit', alpha = 0.5, target)}
\arguments{
  \item{xy}{either a numerical (n x 2)-matrix with the (x,y)-coordinates of n points (1 row of coordinates per point), or a data frame with either the variables \code{x}, \code{y} or \code{point.x}, \code{point.y} as well as \code{aim.x}, \code{aim.y} giving the point of aim. If missing, point of aim is assumed to be in (0,0).}
  \item{center}{logical: center groups to mean (0,0) first? If variable \code{series} does not exist, all shots are treated as belonging to the same group. Only available in method \code{drawGroup.data.frame()}.}
  \item{xyTopLeft}{logical: is the origin of the absolute coordinate system in the top-left corner? This is the default for data exported by OnTarget PC/TDS. If an (n x 2)-matrix is supplied for \code{xy}, point of aim is assumed to be in (0,0).}
  \item{bb}{logical: draw bounding box?}
  \item{bbMin}{logical: draw minimum-area bounding box?}
  \item{bbDiag}{logical: draw bounding box diagonal?}
  \item{minCirc}{logical: draw minimum enclosing circle?}
  \item{maxSpread}{logical: draw maximum spread?}
  \item{meanDist}{logical: draw circle with mean distance to group center?}
  \item{confEll}{logical: draw confidence ellipse with coverage \code{level}?}
  \item{CEP}{draw estimate of CEP circle with coverage \code{level}? Either logical or a string defining the CEP type. See \code{\link[shotGroups]{getCEP}}.}
  \item{ringID}{logical: identify and display the ring count for each shot?}
  \item{valueID}{logical: display numerical values of calcualted measures in the diagram?}
  \item{doRob}{logical: use robust estimation of group center and confidence ellipse?}
  \item{scaled}{logical: draw bullet holes to scale?}
  \item{caliber}{a numerical value indicating the bullet diameter in mm.}
  \item{level}{a numerical vector giving the coverages of the confidence ellipses and CEPs.}
  \item{dstTarget}{a numerical value giving the distance to the target - used in MOA calculation. Acts as override if variable \code{distance} is already included in \code{xy}. See \code{\link[shotGroups]{getMOA}}.}
  \item{conversion}{how to convert the measurement unit for distance to target to that of the (x,y)-coordinates in MOA calculation. Acts as override if variables \code{dist.unit} and \code{point.unit} are already included in \code{xy}. Example \code{'m2cm'}. See \code{\link[shotGroups]{getMOA}}.}
  \item{unit}{Measurement unit for the diagram. Defalt \code{'unit'} indicates that the measurement unit given in \code{conversion} should be used. Possible values are \code{'unit', 'm', 'cm', 'mm', 'yd', 'ft', 'in', 'deg', 'MOA', 'SMOA', 'rad', 'mrad', 'mil'}.}
  \item{alpha}{a numerical value in [0,1] which controls the alpha blending for simulated transparency used to draw the bullet holes}
  \item{target}{a character string like 'ISSF_100m' indicating the target type to be drawn in the scatterplot. See \code{\link[shotGroups]{targets}}.}
}
\value{
Invisibly returns a list with the following components, all converted to \code{unit} (if they were requested):
  \item{xy}{(x,y)-coordinates converted to \code{unit}.}
  \item{ctr}{(x,y)-offset of group center relative to point of aim in \code{unit} (robust with \code{doRob=TRUE}).}
  \item{bb}{bounding box as returned by \code{\link[shotGroups]{getBoundingBox}}.}
  \item{bbMin}{minimum-area bounding box as returned by \code{\link[shotGroups]{getMinBBox}}.}
  \item{bbDiag}{length of diagonal of bounding box.}
  \item{bbMinDiag}{length of diagonal of minimum-area bounding box.}
  \item{minCirc}{minimum enclosing circle as returned by \code{\link[shotGroups]{getMinCircle}}.}
  \item{maxPairDist}{maximum pairwise distance between points (center-to-center, = maximum spread).}
  \item{meanDist}{mean distance to group center.}
  \item{confEll}{confidence ellipse with coverage \code{level} as returned by \code{\link[shotGroups]{getConfEll}} (robust with \code{doRob=TRUE}).}
  \item{CEP}{Rayleigh estimate for the circular error probable CEP with coverage \code{level}.}
  \item{target}{Definition of the selected target in original and converted measurement units.}
  \item{ringCount}{Simulated and maximum ring count as returned by \code{\link[shotGroups]{simRingCount}}.}
}
\seealso{
\code{\link[shotGroups]{getBoundingBox}},
\code{\link[shotGroups]{getMinBBox}},
\code{\link[shotGroups]{getMinCircle}},
\code{\link[shotGroups]{getMaxPairDist}},
\code{\link[shotGroups]{getDistToCtr}},
\code{\link[shotGroups]{getConfEll}},
\code{\link[shotGroups]{drawBox}},
\code{\link[shotGroups]{drawBox2}},
\code{\link[shotGroups]{drawCircle}},
\code{\link[shotGroups]{drawEllipse}},
\code{\link[shotGroups]{targets}},
\code{\link[shotGroups]{drawTarget}},
\code{\link[shotGroups]{simRingCount}},
\code{\link[robustbase]{covMcd}}
}
\examples{
# draw group in MOA
dg <- drawGroup(DFcciHV, xyTopLeft=TRUE, bb=TRUE, minCirc=TRUE,
                confEll=TRUE, maxSpread=TRUE, caliber=5.56, unit='MOA',
                dstTarget=100, conversion='yd2in', target='BDS9')

# mininum enclosing circle in MOA
dg$minCirc

# show Grubbs-Patnaik CEP estimator for mulitple levels
drawGroup(DF300BLKhl, CEP="GrubbsPatnaik", level=c(0.5, 0.9, 0.95),
          dstTarget=100, conversion="yd2in", caliber=7.62)
}
