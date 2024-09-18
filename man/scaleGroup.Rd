\name{scaleGroup}
\alias{scaleGroup}
\alias{scaleGroup.data.frame}
\alias{scaleGroup.default}
\title{
Scale group coordinates to discount excess variance
}
\description{
Artificially scale existing group to change its horizontal or vertical variance. Useful to discount excess variance from varying cross-wind or bullet velocity. Assumes uncorrelated (xy)-coordinates.
}
\usage{
scaleGroup(xy, varX, varY)

\method{scaleGroup}{data.frame}(xy, varX, varY)

\method{scaleGroup}{default}(xy, varX, varY)
}
\arguments{
  \item{xy}{either a numerical (n x 2)-matrix with the (x,y)-coordinates of n points (1 row of coordinates per point), or a data frame with either the variables \code{x}, \code{y} or \code{point.x}, \code{point.y}.}
  \item{varX}{Either \code{logical} or \code{numeric}. If \code{logical}, value \code{TRUE} indicates that the x-coordinates should be scaled to have the same variance as y-coordinates. If \code{numeric}, a single positive value indicating the variance component that should be removed from the variance of x-coordinates. See details.}
  \item{varY}{Either \code{logical} or \code{numeric}. If \code{logical}, value \code{TRUE} indicates that the y-coordinates should be scaled to have the same variance as x-coordinates. If \code{numeric}, a single positive value indicating the variance component that should be removed from the variance of y-coordinates. See details.}
}
\details{
The rationale for this function is that one may be interested in the precision of only some of the components that contribute to the overall precision of a shooting system.

For example, one may be interested in the combined precision of the gun and the ammo. However, in outdoor long-range shooting, the ability to correctly compensate for cross-winds has a strong influence on the horizontal variance of the shots. Under the assumption that the precision of the gun and shooter are independent of direction, one may then regard vertical variance as being representative of the precision components of interest. In this scenario, \code{scaleGroup()} can be used to artificially rescale the shots to make them have the same horizontal variance as the (unchanged) vertical variance. Subsequently, one may use \code{\link[shotGroups]{getRayParam}} to estimate Rayleigh sigma.

In a different application, one may be interested in discounting the vertical variance that is due to the variability in bullet velocity which affects its drop. Bullet velocity can be measured with a chronograph, and can then be converted to bullet drop values using ballistics software. One may then calculate the variance of the bullet drop values and use \code{scaleGroup()} to remove this variance component from the vertical variance of the group without changing the horizontal variance.
}
\value{
A \code{matrix} with the scaled (x,y)-coordinates.
}
\references{
}
\seealso{
\code{\link[shotGroups]{getRayParam}}
}
\examples{
# coordinates of group with excess horizontal
# variance due to cross-winds
xy1 <- cbind(x=c( 2.90,  2.08,  2.16, -1.54, -0.22, -0.03, -0.58,
                  1.32, -2.03, -2.47,  0.06, -0.53,  0.69, -1.51,
                 -0.35,  0.64, -0.57,  0.29, -1.15, -2.59),
             y=c(-0.75, -0.96, -0.52, -0.90, -1.79,  1.05,  0.77,
                  0.67,  0.69, -0.01, -1.04, -0.36,  0.25,  0.16,
                 -0.04, -0.12,  1.08, -0.83, -0.70, -0.30))

# original variances
diag(cov(xy1))

# scale group
xy1_scl <- scaleGroup(xy1, varX=TRUE)

# check that horizontal variance = vertical variance
diag(cov(xy1_scl))

# coordinates of group with excess vertical variance
# due to variability in bullet velocity 
xy2 <- cbind(x=c(2.18, -0.10, -0.17, -0.53, -0.94, -0.23,  0.42,
                 0.38,  0.78,  0.51,  0.19, -0.14, -0.23, -1.44,
                -0.46, -0.43,  0.32,  0.77, -0.43, -0.48),
             y=c(2.10,  1.39,  1.95, -0.16,  0.72,  1.33,  0.10,
                 1.05, -0.33, -0.73,  0.35,  1.04,  0.13, -1.25,
                -1.65, -0.06, -1.90, -0.34, -2.01, -1.72))

# estimated drop from variability in bullet velocity
e_drop <- c( 0.00, -0.97, -0.57, -2.08, -1.44, -0.46, -1.79,
            -1.15, -2.08, -2.73, -1.67, -0.97, -1.73, -2.85,
            -2.97, -1.21, -2.85, -1.21, -2.20, -1.73)

# variance of velocity-related bullet drop
excess_varY <- var(e_drop)

# original variances
diag(cov(xy2))

# scale group
xy2_scl <- scaleGroup(xy2, varY=excess_varY)

# check new vertical variance
diag(cov(xy2_scl))
}
