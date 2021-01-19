\name{getRiceParam}
\alias{getRiceParam}
\alias{getRiceParam.data.frame}
\alias{getRiceParam.default}
\title{Estimate Rice parameters nu and sigma}
\description{Estimates the location parameter nu and the scale parameter sigma of the Rice distribution together with the radial mean MR and radial standard deviation RSD based on a set of 2D-coordinates. Includes the parametric confidence interval for sigma.}
\usage{getRiceParam(xy, level = 0.95, doRob = FALSE, type = c('LiZhangDai', 'MOM'))

\method{getRiceParam}{data.frame}(xy, level = 0.95, doRob = FALSE, type=c('LiZhangDai', 'MOM'))

\method{getRiceParam}{default}(xy, level = 0.95, doRob = FALSE, type=c("LiZhangDai", 'MOM'))}
\arguments{
  \item{xy}{either a numerical (n x 2)-matrix with the (x,y)-coordinates of n points (1 row of coordinates per point), or a data frame with either the variables \code{x}, \code{y} or \code{point.x}, \code{point.y}.}
  \item{level}{a numerical value with the coverage for the confidence interval for sigma.}
  \item{doRob}{logical: use robust estimation of center and covariance matrix as basis for estimators?}
  \item{type}{The initial biased nu estimate is the Euclidean norm of the group center. For \code{'LiZhangDai'}, the bias-correction from Liu et al., 2009. For \code{'MOM'}, the estimated bias is subtracted. If the estimated bias is larger than the initial estimate, the final estimate is then set to 0.}
}
\details{
The sigma estimate uses \code{\link[shotGroups]{getRayParam}}. The robust estimate for the center and for the covariance matrix of (x,y,z)-coordinates is from \code{\link[robustbase]{covMcd}} using the MCD algorithm.
}
\value{
A list with the estimates for nu, sigma, RSD, and MR including the confidence interval for sigma.
\item{nu}{The estimated location parameter nu.}
\item{sigma}{A vector with the sigma estimate and confidence interval bounds as named elements \code{sigma}, \code{sigCIlo}, \code{sigCIup}.}
\item{MR}{The MR estimate.}
\item{RSD}{The RSD estimate.}
}
\references{
\url{https://reference.wolfram.com/language/ref/RiceDistribution.html}

Li, Q., Zhang, J., & Dai, S. (2009). On estimating the non-centrality parameter of a chi-squared distribution. Statistics and Probability Letters 79, 98-114.
}
\seealso{
\code{\link[shotGroups]{Rice}},
\code{\link[shotGroups]{getRayParam}},
\code{\link[shotGroups]{getCEP}},
\code{\link[shotGroups]{getHitProb}},
\code{\link[robustbase]{covMcd}}
}
\examples{
getRiceParam(DF300BLKhl, level=0.95, doRob=FALSE)

# coordinates given by a matrix
\dontrun{
xy <- matrix(round(rnorm(60, 0, 5), 2), ncol=2)
getRiceParam(xy, level=0.95, doRob=FALSE)
}
}
