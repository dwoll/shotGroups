\name{getCEP}
\alias{getCEP}
\alias{getCEP.data.frame}
\alias{getCEP.default}
\title{Circular Error Probable (CEP) and Spherical Error Probable (SEP)}
\description{Estimates the Circular Error Probable (CEP) or the Spherical Error Probable (SEP). CEP/SEP is defined as the radius of the circle/sphere around the point of aim such that it contains a certain fraction of points of impact, e.g., 50\% or 90\%.}
\usage{getCEP(xy, CEPlevel=0.5, dstTarget, conversion,
       center = FALSE, accuracy = FALSE, type = 'CorrNormal', doRob = FALSE)

\method{getCEP}{data.frame}(xy, CEPlevel=0.5, dstTarget, conversion,
       center = FALSE, accuracy = FALSE, type = 'CorrNormal', doRob = FALSE)

\method{getCEP}{default}(xy, CEPlevel=0.5, dstTarget, conversion,
       center = FALSE, accuracy = FALSE, type = 'CorrNormal', doRob = FALSE)}
\arguments{
  \item{xy}{either a numerical (n x p)-matrix with the coordinates of n points (1 row of p coordinates per point), or a data frame with either the variables \code{x}, \code{y} or \code{point.x}, \code{point.y} (optionally \code{z} or \code{point.z}).}
  \item{CEPlevel}{a numerical vector with the coverage values for the CEP/SEP.}
  \item{dstTarget}{a numerical value giving the distance to the target - used in MOA calculation. Acts as override if variable \code{distance} is already included in \code{xy}. See \code{\link[shotGroups]{getMOA}}.}
  \item{conversion}{how to convert the measurement unit for distance to target to that of the (x,y)-coordinates in MOA calculation. Acts as override if variables \code{dist.unit} and \code{point.unit} are already included in \code{xy}. Example \code{'m2cm'}. See \code{\link[shotGroups]{getMOA}}.}
  \item{center}{logical: center groups to mean (0,0) first? If variable \code{series} does not exist, all shots are treated as belonging to the same group. Only available in method \code{getCEP.data.frame()}.}
  \item{accuracy}{logical: take systematic location bias into account? See details.}
  \item{type}{string vector indicating which CEP/SEP estimate to report. Possible values are \code{'CorrNormal', 'GrubbsPearson', 'GrubbsPatnaik', 'GrubbsLiu', 'Rayleigh', 'Krempasky', 'Ignani', 'RMSE', 'Ethridge', 'RAND', 'Valstar'}. See details.}
  \item{doRob}{logical: use robust estimation of center and covariance matrix as basis for estimators?}
}
\details{
For \code{accuracy=FALSE} (default), the reported CEP/SEP estimates do not take into account accuracy, i.e., any systematic location bias. The data is then first centered on the empirical group mean, assumed to coincide with the point of aim. The resulting CEP/SEP-MPI (around Mean Point of Impact) is a pure precision (spread) measure. Set \code{accuracy=TRUE} to incorporate systematic accuracy bias such that the point of aim is in the origin 0, possibly offset from the true group center.
\itemize{
\item \code{CorrNormal}: For \code{accuracy=FALSE} and two-dimensional data, this estimate is based on the correlated bivariate normal distribution re-written in polar coordinates (radius and angle) (see \code{\link[shotGroups]{Hoyt}}). For \code{accuracy=TRUE} or three-dimensional data, it is based on the (offset) circle/sphere probabilities for the correlated multivariate normal distribution (DiDonato & Jarnagin, 1961; DiDonato, 1981, see \code{\link[shotGroups]{qmvnEll}}). This estimate is available for all probability levels.
\item \code{GrubbsPearson}: The Grubbs-Pearson estimate (Grubbs, 1964) is based on the Pearson three-moment central chi^2-approximation of the true cumulative distribution function of radial error. The eigenvalues of the covariance matrix of shot-coordinates are used as variance estimates since they are the variances of the principal components (the PCA-rotated = decorrelated data). This estimate is available for all probability levels, and generalizes to three dimensions.
\item \code{GrubbsPatnaik}: The Grubbs-Patnaik estimate (Grubbs, 1964) differs from the Grubbs-Pearson estimate insofar as it is based on the Patnaik two-moment central chi^2-approximation of the true cumulative distribution function of radial error.
\item \code{GrubbsLiu}: The Grubbs-Liu estimate was not proposed by Grubbs but follows the same principle as his original estimates. It differs from them insofar as it is based on the Liu-Tang-Zhang four-moment non-central chi^2-approximation of the true cumulative distribution function of radial error. For \code{accuracy=FALSE}, it is identical to \code{GrubbsPearson}.
\item \code{Rayleigh}: For \code{accuracy=FALSE} and two-dimensional data, this estimate uses the Rayleigh distribution (see \code{\link[shotGroups]{getRayParam}}). It is valid for uncorrelated bivariate normal coordinates with equal variances. This estimate is available for all probability levels. For \code{accuracy=FALSE} and three-dimensional data, the Maxwell-Boltzmann distribution is used (see \code{\link[shotGroups]{getRayParam}}). For \code{accuracy=TRUE} and two-dimensional data, the estimate uses the Rice distribution (see \code{\link[shotGroups]{getRiceParam}}). For \code{accuracy=TRUE} and three-dimensional data, it is based on the offset sphere probabilities for the multivariate normal distribution set to have equal variances (see \code{\link[shotGroups]{qmvnEll}}).
\item \code{Krempasky}: The Krempasky estimate (Krempasky, 2003) is based on a nearly exact closed-form solution for the 50\% quantile of the radial error for the correlated bivariate normal distribution with 0 mean. It requires estimating the covariance matrix and can only be reported for probability 0.5 with \code{accuracy=FALSE}. It does not generalize to three dimensions.
\item \code{Ignani}: The Ignani estimate (Ignani, 2010) is based on a polynomial approximation for some quantiles of the radial error for the correlated bivariate normal distribution with 0 mean. It requires estimating the covariance matrix and can only be reported for probabilities 0.5, 0.9, 0.95, 0.99 with \code{accuracy=FALSE}. It generalizes to three dimensions.
\item \code{RMSE}: For \code{accuracy=FALSE}, this estimator is the RMSE estimator often described in the GPS literature (van Diggelen, 2007) when using centered data for calculating RMSE (square root of the mean squared error). It is very similar to the \code{Rayleigh} estimator. For \code{accuracy=TRUE}, this the RMSE estimator often described in the GPS literature when using the original, non-centered data for calculating RMSE. It is similar to the \code{Rayleigh} estimator only when bias is small, but becomes seriously wrong otherwise. It is available for all probability levels, and generalizes to three dimensions.
\item \code{Ethridge}: The Ethridge estimate (Ethridge, 1983; Puhek, 1992) is not based on the assumption of multivariate normality of coordinates but uses a robust unbiased estimator for the median radius (Hogg, 1967). It can only be reported for probability 0.5 but generalizes to three dimensions.
\item \code{RAND}: The modified RAND R-234 estimate (RAND, 1952; Pesapane & Irvine, 1977; Puhek 1992) is a weighted sum of the square root of the eigenvalues of the covariance matrix of shot coordinates (the standard deviations of the data that is first de-correlated through rotation). It can only be reported for probability 0.5 and does not generalize to three dimensions.
\item \code{Valstar}: Very similar to the RAND R-234 estimate with \code{accuracy=FALSE} except for very elliptical distributions but with a different bias correction with \code{accuracy=TRUE}. It can only be reported for probability 0.5 and does not generalize to three dimensions.
}

Estimators based on the normal distribution use the plug-in method (Blischke & Halpin, 1966), i.e., they substitute the true covariance matrix and mean vector with those estimated from the data. They are thus strictly valid only for the asymptotic distribution, while the finite sample distribution may differ somewhat.

If package \code{shiny} is installed, an interactive web app for this functionality can be run with \code{runGUI("hitprob")}.
}
\value{
A list with the (chosen) CEP/SEP estimates and supplementary information on the distribution shape.
\item{CEP}{a numerical matrix with the chosen CEP/SEP estimates at the indicated \code{CEPlevel} (in original measurement units, MOA, SMOA, milliradian).}
\item{ellShape}{a numerical vector with the aspect ratio of the ellipse (the square root of condition index \code{\link{kappa}}) and its flattening (1 - inverse aspect ratio).}
\item{ctr}{group center}
}
\references{
Blischke W. R. and Halpin, A. H. (1966). Asymptotic properties of some estimators of quantiles of circular error. Journal of the American Statistical Association, 61 (315), 618-632.

DiDonato, A. R. (1988). Integration of the trivariate normal distribution over an offset spehere and an inverse problem (NSWC TR 87-27). Dahlgren, VA: U.S. Naval Surface Weapons Center Dahlgren Division.

DiDonato, A. R., & Jarnagin, M. P. (1961). Integration of the general bivariate Gaussian distribution over an offset circle. Mathematics of Computation, 15 (76), 375-382.

Grubbs, F. E. (1964). Approximate circular and noncircular offset probabilities of hitting. Operations Research, 12(1), 51-62.

Hogg, R. V. (1967). Some observations on robust estimation. Journal of the American Statistical Association, 62 (320), 1179-1186.

Ignani, B. (2010). Determination of Circular and Spherical Position-Error Bounds in System Performance Analysis. Journal of Guidance, Control, and Dynamics, 33 (4), 1301-1304.

Krempasky, J. J. (2003). CEP equation exact to the fourth order. Navigation: Journal of The Institute of Navigation, 50 (3), 143-149.

Liu, H., Tang, Y., & Zhang, H. H. (2009). A new chi-square approximation to the distribution of non-negative definite quadratic forms in non-central normal variables. Computational Statistics & Data Analysis, 53(4), 853-856.

Pesapane, J., & Irvine, R. B. (1977). Derivation of CEP formula to approximate RAND-234 tables. Offut AFB, NE: Ballistic Missile Evaluation, HQ SAC.

Puhek, P. (1992). Sensitivity analysis of circular error probable approximation techniques (AFIT/GOR/ENS/92M-23). Wright-Patterson AFB, OH: Air Force Institute of Technology.

RAND Corporation. (1952). Offset circle probabilities (RAND-234). Santa Monica, CA: RAND Corporation.

Singh, H. P. 1992. Estimation of Circular Probable Error. The Indian Journal of Statistics, Series B 5(3), 289-305.

van Diggelen, F. 2007. Update: GNSS Accuracy: Lies, Damn Lies, and Statistics. GPS World.
}
\seealso{
\code{\link[shotGroups]{Rayleigh}},
\code{\link[shotGroups]{Maxwell}},
\code{\link[shotGroups]{Hoyt}},
\code{\link[shotGroups]{Rice}},
\code{\link[shotGroups]{mvnEll}},
\code{\link[shotGroups]{getHoytParam}},
\code{\link[shotGroups]{getRayParam}},
\code{\link[shotGroups]{getRiceParam}},
\code{\link[shotGroups]{getConfEll}},
\code{\link[shotGroups]{getHitProb}},
\code{\link[robustbase]{covMcd}}
}
\examples{
# coordinates given by a suitable data frame
(cep <- getCEP(DFtalon, CEPlevel=0.5, accuracy=FALSE,
               dstTarget=10, conversion='m2mm',
               type=c('CorrNormal', 'GrubbsPatnaik', 'Rayleigh')))

# plot points, centers, and circles indicating 50\%-CEP estimates
plot(point.y ~ point.x, data=DFtalon, asp=1, pch=16)
drawCircle(cep$ctr, cep$CEP$CEP0.5['unit', 'CorrNormal'],    fg='red')
drawCircle(cep$ctr, cep$CEP$CEP0.5['unit', 'GrubbsPatnaik'], fg='green3')
drawCircle(cep$ctr, cep$CEP$CEP0.5['unit', 'Rayleigh'],      fg='blue')
points(cep$ctr[1], cep$ctr[2], pch=4, col='gray50', cex=2, lwd=2)
legend(x='bottomleft',
       legend=c('Grubbs 50', 'Corr Normal 50', 'Rayleigh 50', 'center'),
       col=c('red', 'green3', 'blue', 'gray50'), lwd=2,
       lty=c(1, 1, 1, NA), pch=c(NA, NA, NA, 4), bg='white')

# calculate actual coverage percentage of 50% CEP estimates
dists <- getDistToCtr(DFtalon)           # distances to center

# extract CEP radius for all estimates
CEPr <- cep$CEP$CEP0.5['unit', ]

# percentage of points in circle with radius = CEP
100 * sapply(CEPr, function(x) sum(dists <= x)) / length(dists)

# coordinates given by a matrix
\dontrun{
xy <- matrix(round(rnorm(100, 0, 5), 2), ncol=2)
getCEP(xy, accuracy=FALSE, dstTarget=25, conversion='m2cm',
       type=c('Grubbs', 'CorrNormal', 'Rayleigh'))
}
}
