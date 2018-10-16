\name{groupShape}
\alias{groupShape}
\alias{groupShape.data.frame}
\alias{groupShape.default}
\title{Shape analysis for a single group of bullet holes}
\description{Assesses shape of a single group of bullet holes: Outlier analysis as well as numerical and graphical normality checks for a set of (x,y)-coordinates.}
\usage{groupShape(xy, center = FALSE, plots = TRUE, bandW = 0.5,
           outlier = c('mcd', 'pca'), dstTarget, conversion, ...)

\method{groupShape}{data.frame}(xy, center = FALSE, plots = TRUE, bandW = 0.5,
           outlier=c('mcd', 'pca'), dstTarget, conversion, ...)

\method{groupShape}{default}(xy, center = FALSE, plots = TRUE, bandW = 0.5,
           outlier=c('mcd', 'pca'), dstTarget, conversion, ...)}
\arguments{
  \item{xy}{either a numerical (n x 2)-matrix with the (x,y)-coordinates of n points (1 row of coordinates per point), or a data frame with either the variables \code{x}, \code{y} or \code{point.x}, \code{point.y} as well as \code{aim.x}, \code{aim.y} giving the point of aim. If missing, point of aim is assumed to be in (0,0).}
  \item{center}{logical: center groups to mean (0,0) first? If variable \code{series} does not exist, all shots are treated as belonging to the same group. Only available in method \code{groupShape.data.frame()}.}
  \item{plots}{logical: show diagrams?}
  \item{bandW}{for argument \code{bandwith} of \code{\link[graphics]{smoothScatter}}.}
  \item{outlier}{method for outlier identification: \code{mcd} uses robust Mahalanobis distances (see \code{\link[mvoutlier]{aq.plot}}), \code{pca} uses robust principal components analysis (see \code{\link[mvoutlier]{pcout}}). Requires installing package \code{mvoutlier}.}
  \item{dstTarget}{a numerical value giving the distance to the target - used in MOA calculation. Acts as override if variable \code{distance} is already included in \code{xy}. See \code{\link[shotGroups]{getMOA}}.}
  \item{conversion}{how to convert the measurement unit for distance to target to that of the (x,y)-coordinates in MOA calculation. Acts as override if variables \code{dist.unit} and \code{point.unit} are already included in \code{xy}. Example \code{'m2cm'}. See \code{\link[shotGroups]{getMOA}}.}
  \item{...}{additional arguments passed to \code{\link[mvoutlier]{pcout}} with \code{outlier='pca'} - final sensitivity can be adjusted with option \code{outbound}, a sensible candidate value seems to be around 0.45.}
}
\details{
In addition to the numerical results listed below, this function produces the following diagrams:
\itemize{
\item a combined plot for multivariate outlier identification as produced by \code{\link[mvoutlier]{aq.plot}} - requires installing package \code{mvoutlier}
\item a chi-square Q-Q-plot for eyeballing multivariate normality as produced by \code{\link[mvoutlier]{chisq.plot}}, including a reference line with intercept 0 and slope 1
\item a heatmap of a non-parametric 2D-kernel density estimate for the (x,y)-coordinates as produced by \code{\link[graphics]{smoothScatter}} together with group center and error ellipses (original and scaled by factor 2) based on a robust estimate for the covariance matrix (from \code{\link[robustbase]{covMcd}} using the MCD algorithm)
\item a Q-Q-plot of x-coordinates for eyeballing normality
\item a Q-Q-plot of y-coordinates for eyeballing normality
\item a histogram of x-coordinates including a fitted normal distribution as well as a non-parametric kernel density estimate
\item a histogram of y-coordinates including a fitted normal distribution as well as a non-parametric kernel density estimate
}

If package \code{shiny} is installed, an interactive web app for this functionality can be run with \code{runGUI("analyze")}.
}
\value{
A list with the results from the numerical analyses and statistical tests.
\item{corXY}{correlation matrix of (x,y)-coordinates.}
\item{corXYrob}{robust estimate of correlation matrix of (x,y)-coordinates.}
\item{Outliers}{a vector of row indices for observations identified as outliers - only if package \code{mvoutlier} is installed.}
\item{ShapiroX}{Shapiro-Wilk-Test result for normality of x-coordinates. Only for at most 5000 points. For more than 5000 points, replaced by Kolmogorov-Smirnov-Test in \code{ksX}.}
\item{ShapiroY}{Shapiro-Wilk-Test result for normality of y-coordinates. Only for at most 5000 points. For more than 5000 points, replaced by Kolmogorov-Smirnov-Test in \code{ksY}.}
\item{multNorm}{E-statistic-Test result for multivariate normality of (x,y)-coordinates - only available if package \code{energy} is installed.}
}
\note{
The chi-square distribution is only strictly valid for squared Mahalanobis distances if the true center and the true covariance matrix are used in calculation. The goodness of approximation for situations where sample estimates are used should be sufficient here.
}
\seealso{
\code{\link[stats]{qqnorm}},
\code{\link[graphics]{smoothScatter}},
\code{\link[graphics]{hist}},
\code{\link[stats]{kernel}},
\code{\link[robustbase]{covMcd}},
\code{\link[stats]{shapiro.test}},
\code{\link[stats]{ks.test}},
\code{\link[energy]{mvnorm.etest}},
\code{\link[mvoutlier]{chisq.plot}},
\code{\link[mvoutlier]{aq.plot}},
\code{\link[mvoutlier]{pcout}}
}
\examples{
# coordinates given by a suitable data frame
res <- groupShape(DFsavage, bandW=4, outlier='mcd',
                  dstTarget=100, conversion='m2mm')
names(res)
res$corXY
res$Outliers
res$multNorm

# coordinates given by a matrix
\dontrun{
xy <- matrix(round(rnorm(200, 0, 5), 2), ncol=2)
groupShape(xy, bandW=1.6)
}
}
