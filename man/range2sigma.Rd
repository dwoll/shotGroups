\name{range2sigma}
\alias{range2sigma}
\title{Estimate Rayleigh sigma based on range statistics}
\description{Estimate the Rayleigh sigma parameter based on range statistics like extreme spread, figure of merit, or the bounding box diagonal. This function assumes a circular bivariate normal shot distribution with 0 mean.}
\usage{range2sigma(x, stat="ES", n=5, nGroups=1, CIlevel=0.95,
            collapse=TRUE, dstTarget, conversion)}
\arguments{
  \item{x}{a numerical vector with values for extreme spread (ES), figure of merit (FoM), or the diagonal of the bounding box (D).}
  \item{stat}{a character vector with elements \code{"ES"} (extreme spread), \code{"FoM"} (figure of merit), or \code{"D"} (bounding box diagonal) indicating which range statistic is given in \code{x}. Elements correspond to those in \code{x} in the sense that the second element of \code{stat} indicates the statistic for the second element of \code{x}. If all elements of \code{x} are the same kind of statistic, \code{stat} only needs to indicate it once.}
  \item{n}{integer between 2 and 100. Number of shots in each group.}
  \item{nGroups}{integer between 1 and 10. Number of groups when \code{x} is the average of individually-measured range statistics from several groups.}
  \item{CIlevel}{confidence level (coverage probability) for the Rayleigh sigma confidence interval. If one of \code{0.5}, \code{0.9}, \code{0.95}, \code{0.99}, the CI is based on the corresponding quantiles of the Monte Carlo distribution of the range statistic for given \code{n} and \code{nGroups}. If not, CI can only be calculated for extreme spread using a Patnaik chi^2 approximation to the conditional distribution as suggested by Taylor and Grubbs (1975).}
  \item{collapse}{logical: should the list with CIs be simplified if possible?}
  \item{dstTarget}{a numerical value giving the distance to the target - used in MOA calculation. See \code{\link[shotGroups]{getMOA}}.}
  \item{conversion}{how to convert the measurement unit for distance to target to that of the (x,y)-coordinates in MOA calculation. Example \code{'m2cm'}. See \code{\link[shotGroups]{getMOA}}.}
}
\details{
Based on the lookup table \code{\link[shotGroups]{DFdistr}} with results form a Monte Carlo simulation. If the value of \code{n} is not among those simulated (but is less than 100), a monotonic spline interpolation between the neighboring simulated values of the statistic's coefficient of variation is used.

For conversion to the circular error probable, see \code{\link[shotGroups]{range2CEP}}.

Details for the calculation can be found under

\url{http://ballistipedia.com/index.php?title=Range_Statistics}

If package \code{shiny} is installed, an interactive web app for this functionality can be run with \code{runGUI("range")}.
}
\references{
Taylor, M. S., & Grubbs, F. E. (1975). Approximate Probability Distributions for the Extreme Spread (BRL-MR-2438). Aberdeen Proving Ground, MD: U.S. Ballistic Research Laboratory.
}
\value{
A list with the calculated values for sigma in one component, and the corresponding CIs in the other component.
\item{sigma}{The calculated values for sigma in the original measurement unit as well as in angular size measures.}
\item{sigmaCI}{The calculated CIs for sigma in the original measurement unit as well as in angular size measures.}
}
\seealso{
\code{\link[shotGroups]{DFdistr}},
\code{\link[shotGroups]{range2CEP}},
\code{\link[shotGroups]{efficiency}},
\code{\link[shotGroups]{getRayParam}},
\code{\link[shotGroups]{getMaxPairDist}},
\code{\link[shotGroups]{getBoundingBox}}
}
\examples{
es  <- getMaxPairDist(DFscar17)$d
fom <- getBoundingBox(DFscar17)$FoM
d   <- getBoundingBox(DFscar17)$diag
range2sigma(c(es, fom, d), stat=c("ES", "FoM", "D"),
            n=nrow(DFscar17), nGroups=1, CIlevel=0.9)

# compare with Rayleigh sigma estimate from using
# (x,y)-coordinates of all shots
getRayParam(DFscar17, level=0.9)
}

