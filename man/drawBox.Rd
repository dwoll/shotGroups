\name{drawBox}
\alias{drawBox}
\alias{drawBox.list}
\alias{drawBox.default}
\title{Draw an axis-aligned box}
\description{Adds an axis-aligned box to an existing plot.}
\usage{drawBox(x, fg = par('fg'), bg = NA,
        colCtr = NA, lty = par('lty'), lwd = par('lwd'),
        pch = par('pch'), cex = par('cex'))

\method{drawBox}{list}(x, fg = par('fg'), bg = NA,
        colCtr = NA, lty = par('lty'), lwd = par('lwd'),
        pch = par('pch'), cex = par('cex'))

\method{drawBox}{default}(x, fg = par('fg'), bg = NA,
        colCtr = NA, lty = par('lty'), lwd = par('lwd'),
        pch = par('pch'), cex = par('cex'))}
\arguments{
  \item{x}{either a list with component \code{pts} as returned by \code{\link[shotGroups]{getBoundingBox}}, or a vector giving coordinates xleft, ybottom, xright, ytop.}
  \item{fg}{color of the box' rim.}
  \item{bg}{the box' fill color. Set to \code{NA} for a fully transparent box.}
  \item{colCtr}{color of the center point. Set to \code{NA} to omit.}
  \item{lty}{line type of the box.}
  \item{lwd}{line width of the box.}
  \item{pch}{symbol used for the center of the box.}
  \item{cex}{magnification factor for the symbol used for the center of the box.}
}
\details{This function is mainly a wrapper for \code{\link[graphics]{rect}}.}
\seealso{
\code{\link[shotGroups]{getBoundingBox}},
\code{\link[graphics]{rect}}
}
\examples{
xy  <- matrix(round(rnorm(20, 100, 15), 1), ncol=2)
(bb <- getBoundingBox(xy))

plot(xy, asp=1, pch=16)
drawBox(bb, fg='blue', colCtr='blue', pch=4, cex=2)
}
