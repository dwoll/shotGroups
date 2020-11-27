# TODO

 * https://cran.r-project.org/web/packages/momentchi2/

## Short / medium term
### Misc

 * Switch to `rrcov::CovMrcd()` from `robustbase::covMcd()`
 * Gammon 2017 Shot Group Statistics p25 ex16
 * `readData()` function that uses `file=Sys.glob("*pat1/*.csv")` and option `type` to select input file type
 * Rayleigh order stats estimators from lstange/mcgs

### Statistics

 * `polyCub::polyCub()` for integration of other multivariate distribution functions over elliptical domain like in `pmvnEll()`
 * Zhang & An 2012 CEP estimator - general case and Rice case
 * CEP CIs: parametric, delta method with (parametric) bootstrap calibration

### Sugar

 * `print.*()` methods for human readable formatted output

### Target definitions

 * DSU targets a2, b2, b5 simulated score -> score function
 * BDMP PP target -> add target and create score function
 * WRABF targets

## Long term

 * Add formulas for calculated statistics to the vignette
 * Allow dates be associated with group data to track accuracy and precision performance over time
