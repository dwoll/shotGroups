shotGroups
==========

## Analyze shot group data with R

The `shotGroups` package adds functionality to the open source [R environment for statistical computing](https://www.r-project.org/). It provides functions to read in, plot, statistically describe, analyze, and compare shooting data with respect to group shape, precision, and accuracy. This includes graphical methods, descriptive statistics, and inference tests using standard, but also non-parametric and robust statistical techniques. The data can be imported from files produced by [OnTarget PC and OnTarget TDS](https://www.ontargetshooting.com/), [TARAN](http://taran.ptosis.ch/), [ShotMarker e-target](https://www.autotrickler.com/shotmarker.html), [Silver Mountain e-target](https://www.silvermountaintargets.com/), or from custom data files in text format with a similar structure.

 * The package includes limited support for the analysis of three-dimensional data.
 * Inference from range statistics like extreme spread is also supported.
 * For a set of interactive, [`shiny`](https://shiny.rstudio.com/)-based web applications that implement different aspects of the functionality of `shotGroups`, see
    * [shiny.imbei.uni-mainz.de:3838/shotGroups_AnalyzeGroups/](http://shiny.imbei.uni-mainz.de:3838/shotGroups_AnalyzeGroups/)
    * [shiny.imbei.uni-mainz.de:3838/shotGroups_AngularSize/](http://shiny.imbei.uni-mainz.de:3838/shotGroups_AngularSize/)
    * [shiny.imbei.uni-mainz.de:3838/shotGroups_HitProb/](http://shiny.imbei.uni-mainz.de:3838/shotGroups_HitProb/)
    * [shiny.imbei.uni-mainz.de:3838/shotGroups_RangeStat/](http://shiny.imbei.uni-mainz.de:3838/shotGroups_RangeStat/)
 * For further explanations and an example walkthrough, see the [package vignette](https://cran.rstudio.com/web/packages/shotGroups/vignettes/shotGroups.pdf).
 * A good source for statistical methods for analyzing shot groups is [Ballistipedia](http://ballistipedia.com/).

Daniel Wollschlaeger
