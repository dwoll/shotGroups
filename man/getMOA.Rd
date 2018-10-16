\name{getMOA}
\alias{getMOA}
\title{Conversion of absolute size to angular diameter}
\description{Converts object size to angular diameter (degree, radian, minute of angle MOA = arcminute, Shooter's MOA SMOA, milliradian mrad, NATO mil).}
\usage{getMOA(x, dst, conversion,
       type = c('deg', 'rad', 'MOA', 'SMOA', 'mrad', 'mil'))}
\arguments{
  \item{x}{a numerical vector of object sizes.}
  \item{dst}{a numerical vector of viewing distances.}
  \item{conversion}{how to convert the measurement unit for distance to target to that of the (x,y)-coordinates. Either a character vector indicating the conversion like \code{'m2cm'} for meter to centimeter, \code{'yd2in'} for yards to inches, or \code{'ft2cm'} for feet to cm. Imperial (yd, ft, in) and metric units (m, cm, mm) can be freely mixed. Alternatively, a numerical vector giving the multiplication factor for conversion: 100 for m to cm, 36 for yd to in, and 12 for ft to in.}
  \item{type}{type of angular diameter: \code{'deg'} for degree, \code{'rad'} for radian, \code{'MOA'} for minute of angle, \code{'SMOA'} for Shooter's MOA, \code{'mrad'} for milliradian, \code{'mil'} for NATO mil. See details.}
}
\details{
1 MOA (minute of angle, arcmin) = 1/60 degree. Shooter's MOA = SMOA = Inches Per Hundred Yards IPHY. 1 inch at 100 yards = 1 SMOA. 1 milliradian = 1/1000 radian. 1 mil = 2*pi/6400 radian (NATO definition: the circle circumference is divided into 6400 mils). Details are given in the vignette, see \code{vignette('shotGroups') .}

If package \code{shiny} is installed, an interactive web app for this functionality can be run with \code{runGUI("angular")}.
}
\value{A numerical vector with the angular diameter values.}
\seealso{
\code{\link[shotGroups]{fromMOA}},
\code{\link[shotGroups]{getDistance}}
}
\examples{
size <- seq(1, 20, by=5)   # inch
dst  <- 100                # yard
getMOA(size, dst=dst, conversion='yd2in', type='MOA')

# this should return objSize
MOA <- getMOA(size, dst=dst, conversion='yd2in', type='MOA')
fromMOA(MOA, dst=dst, conversion='yd2in', type='MOA')

# SMOA
getMOA(c(1, 2, 5), dst=100, conversion='yd2in', type='SMOA')

# milliradian
getMOA(c(10, 20, 50), dst=100, conversion='m2mm', type='mrad')
}
