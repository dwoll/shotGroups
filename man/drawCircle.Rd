\name{drawCircle}
\alias{drawCircle}
\alias{drawCircle.list}
\alias{drawCircle.default}
\title{Draw a circle}
\description{Adds a circle to an existing plot.}
\usage{drawCircle(x, radius, nv = 100, fg = par('fg'), bg = NA,
           colCtr = NA, lty = par('lty'), lwd = par('lwd'),
           pch = par('pch'), cex = par('cex'))

\method{drawCircle}{list}(x, radius, nv = 100, fg = par('fg'), bg = NA,
           colCtr = NA, lty = par('lty'), lwd = par('lwd'),
           pch = par('pch'), cex = par('cex'))

\method{drawCircle}{default}(x, radius, nv = 100, fg = par('fg'), bg = NA,
           colCtr = NA, lty = par('lty'), lwd = par('lwd'),
           pch = par('pch'), cex = par('cex'))}
\arguments{
  \item{x}{either a numerical vector giving the center's (x,y)-coordinates or a list with the components \code{ctr} and \code{rad} as returned by \code{\link[shotGroups]{getMinCircle}}.}
  \item{radius}{a numerical vector giving the circle's radius.}
  \item{nv}{number of vertices in the approximating polygon.}
  \item{fg}{color of the circle's rim.}
  \item{bg}{the circle's fill color. Set to \code{NA} for a fully transparent circle.}
  \item{colCtr}{color of the center point. Set to \code{NA} to omit.}
  \item{lty}{line type of the circle.}
  \item{lwd}{line width of the circle.}
  \item{pch}{symbol used for the center of the circle.}
  \item{cex}{magnification factor for the symbol used for the center of the circle.}
}
\details{
This function is mainly a wrapper for \code{\link[graphics]{polygon}}. To draw more than a few circles efficiently, use \code{\link[graphics]{symbols}} instead.
}
\seealso{
\code{\link[graphics]{polygon}},
\code{\link[graphics]{symbols}},
\code{\link[shotGroups]{getMinCircle}}
}
\examples{
c1 <- c(1, 2)                 # circle center
c2 <- c(2, 3)                 # another circle center
r1 <- 2                       # circle radius
r2 <- 0.5                     # another circle radius

# determine axis limits so that circles will be visible
xLims <- c1[1] + c(-r1, r1)
yLims <- c1[2] + c(-r1, r1)

plot(c1[1], c1[2], type='n', asp=1, xlim=xLims, ylim=yLims)
drawCircle(c1, r1, fg='blue', colCtr='blue', pch=19)
drawCircle(c2, r2, fg='red', bg='red', colCtr='black', pch=4)
}
