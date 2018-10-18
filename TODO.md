# TODO

## Short / medium term

 * new `readData()` function that uses `file=Sys.glob("*pat1/*.csv")` and option
   `type` to select input file type
 * `pRange()`
 * check Gammon 2017 Shot Group Statistics -> ideas for possible functionality
 * lstange/mcgs: Kuchnost based on four shots and calculated as follows:
    - Find mean point of impact of the four shots
    - Using this point as the center, find minimum radius of circle that encloses all shots
    - Unless there is an outlier, in which case discard the outlier and repeat the procedure with the three remaining shots.
    - Outlier is a shot 2.5 times or more distant from mean point of impact of the other three shots than any of these three shots.
 * Rayleigh order stats estimators from lstange/mcgs
 * Kuonen saddlepoint approximation as an alternative to `CompQuadForm::farebrother()`?
 * `sadists::dsumchisqpow()` as an alternative to `CompQuadForm::farebrother()`?

### Statistics

 * Zhang & An 2012 CEP estimator - general case and Rice case
 * CEP CIs: parametric, delta method with (parametric) bootstrap calibration

### Sugar

 * `print.*()` methods for human readable formatted output

### Target definitions

 * DSU targets a2, b2, b5 simulated score -> score function
 * BDMP PP target -> add and create score function
 * NRA, WRABF targets

## Long term

 * Add formulas for calculated statistics to the vignette
 * Allow dates be associated with group data to track accuracy and precision performance over time
