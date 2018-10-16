\name{efficiency}
\alias{efficiency}
\title{Estimate number of required groups for given CI level and width}
\description{Estimates the approximate number of required groups for a given number of shots per group, confidence interval (CI) level and CI width - when using the Rayleigh sigma estimator based on (x,y)-coordinates, or a range statistic like extreme spread, figure of merit, or the bounding box diagonal. The function may also be used to obtain the estimated CI width when the number of shots per group and the number of groups is given. This functions assumes a circular bivariate normal shot distribution with 0 mean.}
\usage{efficiency(n, nGroups, CIlevel=0.95, CIwidth,
           stat=c("Rayleigh", "ES", "FoM", "D"))}
\arguments{
  \item{n}{a vector of integers between 2 and 100. Number of shots in each group.}
  \item{nGroups}{integer between 1 and 10. Number of groups over which individually-measured statistics will be averaged - when given, \code{CIwidth} must be missing, and the estimated CI width that can be achieved with the given \code{n}, \code{nGroups} and chosen statistic is returned.}
  \item{CIlevel}{confidence level - coverage probability of the CI.}
  \item{CIwidth}{CI width as a fraction of the mean of the chosen statistic for given \code{n} and \code{nGroups}. In other sources (see details) \code{CIwidth/2} is also called \code{E} - the width as a fraction of the mean on either side. When given, \code{nGroups} must be missing, and the estimated required number of groups to achieve the desired CI width for the CI coverage probability \code{CIlevel} is returned.}
\item{stat}{a character vector with elements \code{"Rayleigh"} (Rayleigh sigma), \code{"ES"} (extreme spread), \code{"FoM"} (figure of merit), or \code{"D"} (bounding box diagonal) indicating which statistic would be measured.}
}
\details{
Based on the lookup table \code{\link[shotGroups]{DFdistr}} with results form a Monte Carlo simulation. If the value of \code{n} is not among those simulated (but is less than 100), a spline interpolation between the neighboring simulated values of the statistic's coefficient of variation is used.

The number of required groups is approximate as the calculation assumes a normal distribution for the mean statistic. Details for the calculation can be found under

\url{http://ballistipedia.com/index.php?title=Range_Statistics}

\url{http://www.geoffrey-kolbe.com/articles/rimfire_accuracy/group_statistics.htm}

\url{http://ballistipedia.com/images/3/32/Sitton_1990.pdf}

If package \code{shiny} is installed, an interactive web app for this functionality can be run with \code{runGUI("range")}.
}
\value{
When \code{CIwidth} is given, a data frame with the estimated number of required groups and total shots to achieve the given \code{CIlevel}.
\item{n}{Number of shots per group.}
\item{nGroupsReq}{Number of required groups - as calculated (fractional number).}
\item{nGroupsReqCeil}{Number of required groups - rounded upwards to a whole number.}
\item{nShotsReq}{Total number of required shots - using \code{nGroupsReq} as calculated (fractional number).}
\item{nShotsReqCeil}{Total number of required shots - using \code{nGroupsReq} rounded upwards to a whole number.}
\item{CIlevel}{The CI level}
\item{CIwidth}{The CI width}

When \code{nGroups} is given, a data frame with the estimated CI width required to achieve the desired \code{CIlevel}.
\item{n}{Number of shots per group.}
\item{nGroups}{Number of groups that will be averaged over.}
\item{nShots}{The total number of shots}
\item{CIlevel}{The CI level}
\item{CIwidth}{Required CI width as a fraction of the mean statistic.}
}
\seealso{
\code{\link[shotGroups]{DFdistr}},
\code{\link[shotGroups]{range2sigma}},
\code{\link[shotGroups]{getRayParam}},
\code{\link[shotGroups]{getMaxPairDist}},
\code{\link[shotGroups]{getBoundingBox}}
}
\examples{
# get required number of shots to achieve 90% CI with
# a CI width of 20% of the mean (10% on either side)
# using 10 shots per group and measuring extreme spread
efficiency(n=10, CIlevel=0.9, CIwidth=0.2, stat="ES")

# as above, but using Rayleigh sigma
efficiency(n=10, CIlevel=0.9, CIwidth=0.2, stat="Rayleigh")

# check that the result for ES is about right
# -> 5% quantile with 10 groups is about 10% below the mean
# -> 95% quantile with 10 groups is about 10% above the mean
with(subset(DFdistr, (n == 10L) & (nGroups == 10L)),
     c(ES_Q050/ES_M, ES_Q950/ES_M))

# get achievable 90% CI width with 10 groups of 5 shots each
# using extreme spread
efficiency(n=5, nGroups=10, CIlevel=0.9, stat="ES")
}

