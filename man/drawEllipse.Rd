\name{drawEllipse}
\alias{drawEllipse}
\alias{drawEllipse.list}
\alias{drawEllipse.default}
\title{Draw an ellipse}
\description{Adds an ellipse to an existing plot.}
\usage{drawEllipse(x, shape, radius, nv = 100, axes = FALSE,
            fg = par('fg'), bg = NA, colCtr = NA, lty = par('lty'),
            lwd = par('lwd'), pch = par('pch'), cex = par('cex'))

\method{drawEllipse}{list}(x, shape, radius, nv = 100, axes = FALSE,
            fg = par('fg'), bg = NA, colCtr = NA, lty = par('lty'),
            lwd = par('lwd'), pch = par('pch'), cex = par('cex'))

\method{drawEllipse}{default}(x, shape, radius, nv = 100, axes = FALSE,
            fg = par('fg'), bg = NA, colCtr = NA, lty = par('lty'),
            lwd = par('lwd'), pch = par('pch'), cex = par('cex'))}
\arguments{
  \item{x}{either a numerical 2-vector giving the (x,y)-coordinates of the center or a list with the components \code{ctr}, \code{cov} and \code{magFac} as returned by \code{\link[shotGroups]{getConfEll}}.}
  \item{shape}{a numerical symmetric (2 x 2)-matrix whose eigen-structure determines the ellipse's shape.}
  \item{radius}{a numerical value giving the ellipse's magnification factor.}
  \item{nv}{number of vertices in the approximating polygon.}
  \item{axes}{logical: should ellipse axes be drawn?}
  \item{fg}{color of the ellipse's rim.}
  \item{bg}{the ellipse's fill color. Set to \code{NA} for a fully transparent ellipse.}
  \item{colCtr}{color of the center point. Set to \code{NA} to omit.}
  \item{lty}{line type of the ellipse.}
  \item{lwd}{line width of the ellipse.}
  \item{pch}{symbol used for the center of the ellipse.}
  \item{cex}{magnification factor for the symbol used for the center of the ellipse.}
}
\details{This function is mainly a wrapper for \code{\link[graphics]{polygon}}.}
\seealso{
\code{\link[graphics]{polygon}},
\code{\link[shotGroups]{getConfEll}}
}
\examples{
## error ellipse for a set of points
xy <- matrix(round(rnorm(100, 0, 8), 2), ncol=2)
ce <- getConfEll(xy)
plot(xy, pch=16, asp=1)
drawEllipse(ce, radius=1, axes=TRUE, fg='blue', colCtr='blue',
            lwd=2, pch=4, cex=2)
}
