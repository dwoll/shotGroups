\name{compareGroups}
\alias{compareGroups}
\title{Compare bullet hole groups}
\description{Numerically and graphically compare accuracy, precision, and distribution shape of up to 15 groups of bullet holes.}
\usage{compareGroups(DF, plots = TRUE, xyTopLeft = TRUE, center = FALSE,
              ABalt = c('two.sided', 'less', 'greater'),
              Walt = c('two.sided', 'less', 'greater'),
              CEPtype = 'CorrNormal', CEPlevel = 0.5, CIlevel = 0.95,
              dstTarget, conversion)}
\arguments{
  \item{DF}{a data frame containing (at least) these variables: \code{series} (a \code{\link{factor}}), and either \code{point.x}, \code{point.y} or \code{x}, \code{y} defining the bullet holes. Variables \code{distance} (distance to target), \code{aim.x}, \code{aim.y} (point of aim) are useful - if they are missing, a warning is given and a default assumed.}
  \item{plots}{logical: show diagrams?}
  \item{xyTopLeft}{logical: is the origin of the absolute coordinate system in the top-left corner? See details.}
  \item{center}{logical: center groups to mean (0,0) first to compare only with respect to precision?}
  \item{ABalt}{a character string indicating the hypothesis for the Ansari-Bradley-Test for equal variances. Only used when exactly 2 groups are compared.}
  \item{Walt}{a character string indicating the hypothesis for the Wilcoxon-Rank-Sum-Test for equality of average distance to group center (equivalent to the Mann-Whitney-U-Test). Only used when exactly 2 groups are compared.}
  \item{CEPtype}{string indicating which CEP estimate to report from \code{\link[shotGroups]{getCEP}}.}
  \item{CEPlevel}{a numerical value giving the coverage of the confidence ellipse and CEP.}
  \item{CIlevel}{a numerical value giving the level for the confidence intervals (for standard deviations and Rayleigh sigma, MR).}
  \item{dstTarget}{a numerical value giving the distance to the target - used in MOA calculation. Acts as override if variable \code{distance} is already included in \code{DF}. See \code{\link[shotGroups]{getMOA}}.}
  \item{conversion}{how to convert the measurement unit for distance to target to that of the (x,y)-coordinates in MOA calculation. Acts as override if variables \code{dist.unit} and \code{point.unit} are already included in \code{DF}. Example \code{'m2cm'}. See \code{\link[shotGroups]{getMOA}}.}
}
\details{
By default, OnTarget PC/TDS' 'Export Point Data' places the origin of the absolute coordinate system in the top-left corner. In OnTarget TDS, this setting can be changed by checking the box 'Tools -> Options -> Options tab -> Data Export -> Invert Y-Axis on Export'. In that case, use \code{xyTopLeft=FALSE}. If groups appear to be upside-down, \code{xyTopLeft} is the setting to change.

OnTarget PC/TDS' \code{Group} variable identifies groups just within one file, whereas factor \code{series} is taken to number groups also across different original files. If your data was read with \code{\link[shotGroups]{readDataOT1}}, \code{\link[shotGroups]{readDataOT2}} or \code{\link[shotGroups]{readDataMisc}}, \code{series} is added automatically. For data from just one file, you can otherwise copy variable \code{group} to \code{series} in a data frame called \code{shots} with \code{shots$series} \code{<-} \code{shots$group}.\cr
If the data is missing information about the point of aim, (0,0) is assumed. If distance to target is missing, 100 is assumed.

In addition to the numerical results listed below, this function produces the following diagrams:
\itemize{
\item a scatterplot showing all groups as well as their respective center and confidence ellipse
\item a scatterplot showing all groups as well as their respective (minimum) bounding box and maximum group spread
\item a scatterplot showing all groups as well as their respective minimum enclosing circle and circle with average distance to center
\item a boxplot for the distances to group center per group
\item a stripchart showing the distances to group center per group together with the estimated Rayleigh mean radius and its confidence interval
}

If package \code{shiny} is installed, an interactive web app for this functionality can be run with \code{runGUI("analyze")}.
}
\value{
A list with the results from numerical comparisons and statistical tests.
\item{ctr}{group center offset from the respective point of aim.}
\item{distPOA}{distances from group centers to point of aim (in original measurement units, MOA, SMOA, milliradian).}
\item{MANOVA}{MANOVA result from testing equality of group center offset from the respective point of aim (test statistic is Wilk's lambda).}
\item{corXY}{group correlation matrices for the (x,y)-coordinates.}
\item{sdXY}{list with group standard deviations of the x- and y-coordinates (in original measurement units, MOA, SMOA, milliradian).}
\item{sdXYci}{list with group parametric (chi^2) confidence intervals for the standard deviations of x- and y coordinates (in original measurement units, MOA, SMOA, milliradian).}
\item{meanDistToCtr}{average distances from points to their respective group center (in original measurement units, MOA, SMOA, milliradian).}
\item{maxPairDist}{maximum pairwise distance between points for each group (center-to-center, = maximum spread, in original measurement units, MOA, SMOA, milliradian).}
\item{bbFoM}{minimum-area bounding box figure of merit (average side length) for each group (in original measurement units, MOA, SMOA, milliradian).}
\item{bbDiag}{minimum-area bounding box diagonal length for each group (in original measurement units, MOA, SMOA, milliradian).}
\item{minCircleRad}{radius of the minimum enclosing circle for each group (in original measurement units, MOA, SMOA, milliradian).}
\item{sigma}{estimated Rayleigh parameter sigma (precision) for each group (in original measurement units, MOA, SMOA, milliradian).}
\item{MR}{estimated Rayleigh mean radius for each group (in original measurement units, MOA, SMOA, milliradian).}
\item{sigmaMRci}{parametric (chi^2) confidence intervals for Rayleigh sigma and MR (in original measurement units, MOA, SMOA, milliradian).}
\item{CEP}{Estimate for the circular error probable (CEP) in each group (in original measurement units, MOA, SMOA, milliradian).}
\item{AnsariX}{Ansari-Bradley-Test result from testing equality of group variances for x-coordinates. When two groups are compared.}
\item{AnsariY}{Ansari-Bradley-Test result from testing equality of group variances for y-coordinates. When two groups are compared.}
\item{Wilcoxon}{Wilcoxon-Rank-Sum-Test result from testing equality of average point distances to their respective group center. When two groups are compared.}
\item{FlignerX}{Fligner-Killeen-Test result from testing equality of group variances for x-coordinates. When more than two groups are compared.}
\item{FlignerY}{Fligner-Killeen-Test result from testing equality of group variances for y-coordinates. When more than two groups are compared.}
\item{Kruskal}{Kruskal-Wallis-Test result from testing equality of average point distances to their respective group center. When more than two groups are compared.}
}
\seealso{
\code{\link[shotGroups]{analyzeGroup}},
\code{\link[shotGroups]{getDistToCtr}},
\code{\link[shotGroups]{getMaxPairDist}},
\code{\link[shotGroups]{getMinBBox}},
\code{\link[shotGroups]{getMinCircle}},
\code{\link[shotGroups]{getCEP}},
\code{\link[shotGroups]{getMOA}},
\code{\link[shotGroups]{getRayParam}},
\code{\link[shotGroups]{drawEllipse}},
\code{\link[stats]{anova.mlm}},
\code{\link[coin]{ansari_test}},
\code{\link[coin]{fligner_test}},
\code{\link[coin]{wilcox_test}},
\code{\link[coin]{kruskal_test}}
}
\examples{
cmp <- compareGroups(DF300BLKhl, dstTarget=100, conversion='yd2in')
names(cmp)
cmp$ctr
cmp$meanDistToCtr
cmp$CEP
cmp$Kruskal
}
