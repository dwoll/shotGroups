\name{getHoytParam}
\alias{getHoytParam}
\alias{getHoytParam.data.frame}
\alias{getHoytParam.matrix}
\alias{getHoytParam.list}
\alias{getHoytParam.default}
\title{Determine parameters q and omega of the Hoyt distribution}
\description{Determines the Hoyt distribution's shape parameter q and scale parameter omega from the eigenvalues of a (2 x 2)-covariance matrix.}
\usage{getHoytParam(x)

\method{getHoytParam}{matrix}(x)

\method{getHoytParam}{list}(x)

\method{getHoytParam}{data.frame}(x)

\method{getHoytParam}{default}(x)}
\arguments{
  \item{x}{one of the following: a (2 x 2)-covariance matrix, a list of (2 x 2)-covariance matrices, a data frame with either the variables \code{x}, \code{y} or \code{point.x}, \code{point.y}, a 2-vector with eigenvalues.}
}
\details{
The parameters \code{q} and \code{omega} derive from the eigenvalues ev1, ev2 of the covariance matrix of the bivariate normal distribution as follows: \code{q = 1 / sqrt(((ev1+ev2)/ev2) - 1)} and \code{omega = ev1 + ev2}.

If \code{x} is a data frame, its sample covariance matrix is used to estimate the eigenvalues. Note that the Hoyt distribution is only approximately valid for large samples if estimated parameters are used.
}
\value{
A list with the following components:
\item{q}{A vector with values of the shape parameter \code{q}.}
\item{omega}{A vector with values of the scale parameter \code{omega}.}
}
\references{
Hoyt, R. S. (1947). Probability functions for the modulus and angle of the normal complex variate.
Bell System Technical Journal, 26(2), 318-359.

\url{http://reference.wolfram.com/language/ref/HoytDistribution.html}
}
\seealso{
\code{\link[shotGroups]{Hoyt}}
}
\examples{
## q and omega based on coordinates in a data frame
getHoytParam(DFscar17)

## q and omega based on a covariance matrix
cm1 <- cbind(c(8, 0), c(0, 2))
getHoytParam(cm1)

## q and omega based on a list of covariance matrices
cm2 <- cbind(c(6, 0), c(0, 4))
cmL <- list(cm1, cm2)
getHoytParam(cmL)

## q and omega based on eigenvalues
ev <- eigen(cm1)$values
getHoytParam(cm1)
}
