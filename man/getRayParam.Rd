\name{getRayParam}
\alias{getRayParam}
\alias{getRayParam.data.frame}
\alias{getRayParam.default}
\title{Estimate Rayleigh parameters sigma, mean and standard deviation}
\description{Estimates the radial precision parameter sigma of the Rayleigh distribution together with the radial mean MR, radial median MEDR, and radial standard deviation RSD, including parametric confidence intervals. For 1D data, it estimates the parameters of the half normal distribution with parameter theta = sqrt(pi/2). For 3D data, it estimates the parameters of the Maxwell-Boltzmann distribution}
\usage{getRayParam(xy, level = 0.95, mu, doRob = FALSE)

\method{getRayParam}{data.frame}(xy, level = 0.95, mu, doRob = FALSE)

\method{getRayParam}{default}(xy, level = 0.95, mu, doRob = FALSE)}
\arguments{
  \item{xy}{either a numerical matrix with the coordinates of n points (1 row of coordinates per point), or a data frame with either the variables \code{x}, \code{y} or \code{point.x}, \code{point.y}.}
  \item{level}{a numerical value with the coverage for the confidence intervals for sigma, MR, RSD.}
  \item{mu}{numerical 2-vector with the true group center (optional). See details.}
  \item{doRob}{logical: use robust estimation of covariance matrix as basis for estimators?}
}
\details{When the true mean \code{mu} of the distribution is given, the sigma estimate uses the sum of squared radii for the variance estimate (the total un-corrected variance of the coordinates), and employs the c4 correction factor for taking the square root. When \code{mu} is missing, the sum of squared radii is Bessel-corrected for estimating the center.

The robust estimate for the covariance matrix of (x,y)-coordinates is from \code{\link[robustbase]{covMcd}} using the MCD algorithm.
}
\value{
A list with the estimates for sigma, RSD, MR, and MEDR including the confidence intervals.
\item{sigma}{A vector with the sigma estimate and confidence interval bounds as named elements \code{sigma}, \code{sigCIlo}, \code{sigCIup}.}
\item{RSD}{A vector with the RSD estimate and confidence interval bounds as named elements \code{RSD}, \code{RSDciLo}, \code{RSDciUp}.}
\item{MR}{A vector with the MR estimate and confidence interval bounds as named elements \code{MR}, \code{MRciLo}, \code{MRciUp}.}
\item{MEDR}{A vector with the MEDR estimate and confidence interval bounds as named elements \code{MEDR}, \code{MEDRciLo}, \code{MEDRciUp}.}
}
\references{
\url{http://ballistipedia.com/index.php?title=Closed_Form_Precision}

Singh, H. P. 1992. Estimation of Circular Probable Error. The Indian Journal of Statistics, Series B 5(3), 289-305.
}
\seealso{
\code{\link[shotGroups]{Rayleigh}},
\code{\link[shotGroups]{Maxwell}},
\code{\link[shotGroups]{getCEP}},
\code{\link[shotGroups]{getHitProb}},
\code{\link[shotGroups]{groupSpread}},
\code{\link[robustbase]{covMcd}}
}
\examples{
# coordinates given by a suitable data frame
getRayParam(DFtalon, level=0.95, doRob=FALSE)

# coordinates given by a matrix
\dontrun{
xy <- matrix(round(rnorm(100, 0, 5), 2), ncol=2)
getRayParam(xy, level=0.95, doRob=FALSE)
}
}
