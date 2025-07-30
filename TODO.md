# TODO

 * function: rescale group to make variances equal, by removing excess variance from x or y, or Mahalanobis + rescaling?
 * function: take group n - return uncertainty of zero estimate as mean (sigma*sqrt(pi/2)) and median (sigma*sqrt(2*log(2))) in terms of multiples of Rayleigh sigma, or supply sigma estimate, or supply data to estimate sigma (p95)
 * `bs4Dash` shiny app: active distance to target should depend on current group selection

## Short / medium term
### Misc

 * `readData()` function that uses `file=Sys.glob("*pat1/*.csv")` and option `type` to select input file type

### Statistics

 * Gammon 2017 Shot Group Statistics p25 ex16
 * `polyCub::polyCub()` for integration of other multivariate distribution functions over elliptical domain like in `pmvnEll()`
 * `polyCub::polyCub()` for integration of multivariate normal distribution over polygons like in `pmvnEll()`
 * Zhang & An 2012 CEP estimator - general case and Rice case
 * CEP CIs: parametric, delta method with (parametric) bootstrap calibration
 * Rayleigh order stats estimators from lstange/mcgs

### Sugar

 * `print.*()` methods for human readable formatted output

### Target definitions

 * DSU targets a2, b2, b5 simulated score -> score function
 * BDMP PP target -> add target and create score function
 * WRABF targets

## Long term

 * Add formulas for calculated statistics to the vignette
 * Allow dates be associated with group data to track accuracy and precision performance over time
