\name{getDistance}
\alias{getDistance}
\title{Get distance based on absolute and angular size}
\description{Calculates the distance to an object based on the object's absolute and angular size.}
\usage{getDistance(x, angular, conversion,
            type = c('deg', 'rad', 'MOA', 'SMOA', 'mrad', 'mil'))}
\arguments{
  \item{x}{a numerical vector of absolute object sizes.}
  \item{angular}{a numerical vector of angular object sizes.}
  \item{conversion}{how to convert the measurement unit for the returned distance to object to that of given absolute object size. Either a character vector indicating the conversion like \code{'m2cm'} for meter to centimeter, \code{'yd2in'} for yards to inches, or \code{'ft2cm'} for feet to cm. Imperial (yd, ft, in) and metric units (m, cm, mm) can be freely mixed. Alternatively, a numerical vector giving the multiplication factor for conversion: 100 for m to cm, 36 for yd to in, and 12 for ft to in.}
  \item{type}{type of angular measure used in \code{angular}: \code{'deg'} for degree, \code{'rad'} for radian, \code{'MOA'} for minute of angle, \code{'SMOA'} for Shooter's MOA, \code{'mrad'} for milliradian, \code{'mil'} for NATO mil. See details.}
}
\details{
1 MOA (minute of angle, arcmin) = 1/60 degree. Shooter's MOA = SMOA = Inches Per Hundred Yards IPHY. 1 inch at 100 yards = 1 SMOA. 1 milliradian = 1/1000 radian. 1 mil = 2*pi/6400 radian (NATO definition: the circle circumference is divided into 6400 mils). Details are given in the vignette, see \code{vignette('shotGroups')}
}
\value{A numerical vector with the distance values.}
\seealso{
\code{\link[shotGroups]{getMOA}},
\code{\link[shotGroups]{fromMOA}}
}
\examples{
size <- seq(1, 20, by=5)   # inch
dst  <- 100                # yard

# get angular size in MOA from absolute size
angular <- getMOA(size, dst=dst, conversion='yd2in', type='MOA')

# this should return dst throughout
getDistance(size, angular=angular, conversion='yd2in', type='MOA')
}
