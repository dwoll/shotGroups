# TODO
## Errata 5th edition

  * Abschn. 2.11.1, S. 109 fac <- factor(fac) -> fac, DV1, DV2 undefiniert
  * `vector(mode="list", length=5)` not `vector(mode="list", n=5)`
  * `ggplot2` `coord_fixed(ratio)` aspect ratio, expressed as y / x
  * `haven::save_spss()` issue with `3L`
  * `ggplot2` `after_stat()` instead of `..density..`, `..count..`
  * `sqIm - as.raster(arrSq)`
  * für die Darstellung Dichte das Argument `freq=FALSE`
  * 12.6.1: "ist gleich dem $n$-fachen der quadrierten Mahalanobisdistanz zwischen dem Zentroid der Daten und dem Zentroid unter $\text{H}_{0}$ bzgl.\ der korrigierten Kovarianzmatrix der Daten ist"
  * 12.7 fitIII <- lm(Ym2 ~ IV1*IV2,
             contrasts=list(IV1=contr.sum, IV2=contr.sum)); Manova(fitIII, type="III")
  * 9.4.4 survfit(), predict() für mittlere Pseudo-Beobachtung seit Version 3.2.9 anders

## General

  * https://github.com/r-lib/rig useful for MacOS users
  * https://shinylive.io/r/examples/

## First steps

## Basics

  * Package `diffdf` \cite{GowerPage2024} to compare data frames?

## Import / export, data wrangling

## Reproducible code

  * Make Quarto main? Quarto Live extension for interactive R console (https://r-wasm.github.io/quarto-live/), static HTML page
  * https://quarto.org/docs/computations/execution-options.html
  * override YAML front-matter option on a per code-block basis
  * format: revealjs
  * Quarto YAML indent 4 spaces, not : false, not =FALSE
  * Use Knitr engine if an {r} code block is discovered within the file
  * Quarto cross-references: Ähnliche Parameter sind auch für Tabellen implementiert, hier ist der Prefix nur tbl
  * #| fig-alt: "A line plot on a polar axis"
  * crossref:
  fig-title: Abb
  tbl-title: Tab
  fig-prefix: Abbildung
  tbl-prefix: Tabelle
  options can be format specific
  format: 
  html:
    fig-width: 8
    fig-height: 6
  pdf:
    fig-width: 7
    fig-height: 5

## Preliminaries

## Regression

## ANOVA

  * 7.6.1 `emmip(catcat, prog ~ gender, CIs=TRUE)` statt `interaction.plot()`, `plot.design()`?
  * As new 7.10: Separate section on marginal models using `emmeans` or `marginaleffets`, see https://www.andrewheiss.com/blog/2022/05/20/marginalia/. Especially useful with interactions, non-linearities. Slopes represent the marginal changes in an outcome. `emmeans`: marginal means, predictions, simple slopes. LS means are essentially the same idea as unweighted means. In LS means, we fit a model to the data and use it (in the two-way factorial case) to predict the μij; then our marginal means are estimated as equally-weighted marginal averages of these predictions, just as in unweighted-means analysis. 
  * `emmeans()` gives predicted values at specific values of one variable, with averages plugged in for other variables. `emtrends()` gives marginal effects = marginal slopes. With present 3-way interaction: `emmeans(noise.lm, pairwise ~ size)` vs. `emm_s.t <- emmeans(noise.lm, pairwise ~ size | type)`

## GLM

## Survival

  * 9.4 neu: Ausgearbeitetes Beispiel für Time-Varying Kovariaten inkl. `survSplit()` + Anpassung des Kovariaten-Status, evtl. `tmerge()`

## Nonparametric

## Resampling

## Multivariate

## Prediction performance

## Graphics

## ggplot2

  * Abbildung `scale_x_continuous(transform="log10")` vs. `coord_transform(x="log10")`, `annotation_logticks()` (only for log10?)
  * Format plot title -> `theme(...)` `element_text()` ..., `geom_text(..., family)`
  * `geom_histogram(position="identity", alpha=0.4)`
  * `geom_boxplot()` combine with `geom_violin()` using `width=0.1`
  * 15.2.3 Verweis auf `geom_density_2d_filled()`
  * 15.5.1 `geom_density_2d_filled()` statt `geom_hex()`
  * bubble plot using `aes(size=<<Variable>>)` maps to circle radius, using `scale_size_area(max_size = 15)` maps to circle area with `geom_point()`
  * Abb. 15.1 mit größeren Punkten
  * check diagrams, especially last, barplot
  * dotplot as alternative to barplot
  * `geom_area()` as extension of `geom_line()`, multiple: use with `alpha=0.4`, `colour="black"`, `linewidth=0.2`, ggf. without `colour="black"` but with separate `geom_line()` after `geom_area()` `fill=<<Faktor>>`, `position=position_fill()` vs. stacked (default)
  * `geom_ribbon()` example
  * `geom_abline()`
  * `geom_segment(..., arrow=arrow())` example
  * `scale_shape_discrete()`, `scale_linetype()` not explained

## Numerical methods

## Programming

## RExRepos

  * rdocumentation -> rrdio? format: https://rdrr.io/cran/shotGroups/man/readDataOT1.html
  * link to Posit RStudio documentation / introduction
  * knitr / rmarkdown documents, reproducibility https://CRAN.R-project.org/view=ReproducibleResearch

# DONE

  * Zeilenlänge 70 Zeichen innerhalb von lstlisting kontrollieren
  * R-Befehle: Cross-check gegen LaTeX-Quellen
  * R-Befehle: Fehlerfreiheit prüfen
  * Abbildungen: Helligkeit Graustufen
  * Aktueller Stand
    * Literatur
    * Pakete (OpenMx, EBImage -> nicht auf CRAN)
    * R-Publikationen
  * Abb.\ Abschn.\ bzgl.\ bzw.\ ca.\ d.\,h.\ engl.\ etc.\ evtl.\ ggf.\ i.\,d.\,R.\ inkl.\ i.\,S.\ o.\,g.\ s.\ sog.\ s.\,o.\ s.\,u.\ Tab.\ u.\,a.\ usw.\ u.\,U.\ vgl.\ z.\,B.\ z.\,T.\ ... GP\@.
  * \lstinline in footnote, caption, section: \%, \", \{, \}
  * `spelling::spell_check_files("...", lang="de_DE")`
  * Word spell check - vorher entfernen    
    \\chapter\*?    
    \\section\*?    
    \\subsection\*?    
    \\subsubsection\*?    
    \\index    
    \\emph    
    \\textit    
    \\tiny    
    \\item    
    \\footnote    
    \\label\{.+?\}    
    \\index\[.+\]\{.+?\}    
    \|textbf\}    
    \\lstinline!.+?!    
    (?s)\\begin\{lstlisting\}.+?\\end\{lstlisting\}    
    \\begin\{.+?\}    
    \\end\{.+?\}    
    \\ref\{.+?\}    
    \\texttt\{.+?\}    
    \\citeA?\{.+?\}    
    \\citeNP\{.+?\}    
    \$.+?\$    
    \{\\quotedblbase\}    
    \{\\textquotedblleft\}    
    ggf\.\\    
    vgl\.\\    
    bzw\.\\    
    Abschn\.\\    
    inkl\.\\    
    bzgl\.\\    
    etc\.\\    
    Tab\.\\    
    sog\.\\    
    Kap\.\\    
    Abb\.\\    
    ca\.\\    
    z\.\\,B\.\\    
    i\.\\,S\.\\    
    i\.\\,a\.\\    
    s\.\\,o\.    
    s\.\\,u\.    
    o\.\\,g\.\\    
    d\.\\,h\.\\    
    u\.\\,U\.\\    
    u\.\\,a\.\\    
    i\.\\,d\.\\,R\.\\    
    \\R\\    
    \\R    
    \\textsf    
    \\my.+?\{    
    \\url    
    {    
    }    
    (    
    )    
    @    
    |    
    [    
    ]    
    %    
    -    
     see    

# PERHAPS

  * package `waldo` or `versus` to compare data frames?
  * replace `pheatmap` with `ComplexHeatmap` from BioConductor
  * experimental: `summarise(..., .groups="drop")`
  * `.by` experimental inline alternative to `group_by()` for temporary grouping -> no need to `ungroup()`
  * `pak::pak("tidyverse/dplyr")` instead of `remotes::install_github()`?
  * https://smithjd.github.io/sql-pet/
  * `scale_x_discrete(guide=guide_axis(n.dodge=2))` -> dodge wide labels such that no overlap is there
  * überall: kein "> " bzw. "+ " mehr bei Befehlen, sondern "# " am Beginn der Ausgabe
  * Bayes mit `rstanarm` / `brms`
  * The Theil median slope is a generalization of the Hodges–Lehmann median difference to the case of a non-binary X-variable, the Theil median slope is usually defined as the median value of the slope (Y1 − Y2)/(X1 − X2), or at least as its conditional median, assuming that X1 != X2, the intercept is defined as the median of intercepts when the line goes through each point
  * Kreuzvalidierung für Cox Regression
  * 12.9.6.4 vs. 11.9.6.5: einmal univariat, einmal multivariat
  * 12.9.5, 12.9.6, 12.9.7 auch multivariat, vgl. Fox Appendix-Multivariate
  * `poisson.test()`
  * `max.col()`
  * 7.5.1 `model.tables(..., se=TRUE)` unbalancierte Designs: `se.contrast()` für Standardfehler
  * multiple Imputation mit `mice`
  * \\chapter{Skalenanalyse von Tests und Fragebögen}    
    \\label{sec:quest}    
    \\index{Skalenanalyse}    
    Kennwerte zur Konstrukton von Tests und Fragebögen nach KTT    
    Anwendungsfall verschiedener statistischer Verfahren    
    Paket \\lstinline!psych!\\index{psych@\\lstinline{psych}},    
    Item Response Theory \\myURL{Psychometric Models} der Task Views \\cite{RDevelopmentCoreTeam2009a}.
  * Schwierigkeit: (Mittelwert / Skalenmaximum) * 100    
  * Trennschärfe: concord -> `quest.reliability()` -> item-whole Korrelation    
  * Reliabilität: concord -> `quest.reliability()` -> alpha, item-alpha, check: alpha if item deleted    
  * split-half (random vs. odd-even)    
  * Spearman-Brown: psychometric -> `SBrel()`, `SBlength()`    
  * Skalen-Item Zugehörigkeit -> auf FA verweisen
  * https://www.datanovia.com/en/blog/ggplot-legend-title-position-and-labels/#change-legend-title
 
# DON'T

  * Correspondence analysis of categorical data? and its constrained version, canonical correspondence analysis (CCA) - package vegan: `cca()`
  * https://github.com/dspiegel29/ArtofStatistics
  * Bücher zu Skalenanalyse von Tests und Fragebögen (Rasch, KTT)
  * Eigenes Kapitel "Einfache statistische Auswertungen"? (2.7, 2.12, 2.13.3, 2.13.6)
  * https://rcode.dev/
  * Bio7 IDE
  * class `raw` -> bytes, default `00` (hex)
  * `ls(.BaseNamespaceEnv)`
  * functional programming    
    mergeMany <- function(x) { Reduce(merge, x) }, or: Reduce(intersect, x)    
    Reduce(..., accumulate) -> iterative calculations, e.g., cumulative product    
    weighted moving average:    
    lambda <- 0.95; f <- function(prv, nxt) { lambda\*prv + (1-lambda)\*nxt }    
    ewma <- Reduce(f, prices, accumulate=TRUE)
  * Discrete time Proportional Hazards Modell    
    https://data.princeton.edu/wws509/notes/c7s6    
    `ties="exact"`: exact partial likelihood is equivalent to a conditional logistic model, and is appropriate when the times are a small set of discrete values. computationally intensive    
  * a discrete-time Cox regression is performed with a discrete hazard atom at each observed event time. This is equivalent to a logistic regression with the cloglog link
    http://semsto.userweb.mwn.de/Lehre/GRMWS1112/Blatt10.r
  * komplementärer loglog-Link    
    `modelvglm1b <- vglm(PAIN ~ TH + AGE + AGE2, family=cumulative(link="cloglog", parallel=TRUE))`  
    -> with continuation ratio logits (discrete, cf. F Harrell)? -> `vglm(..., family=sratio(link="cloglog", parallel=TRUE))`   
    Note that in discrete time the hazard is a conditional probability rather than a rate. However, the general result expressing the hazard as a ratio of the density to the survival function is still valid.
  * zufällige, positiv-definite symmetrische Matrix: `clusterGeneration::genPositiveDefMat(5)$Sigma`
  * `do.call("cbind", lapply(ls(pattern="V[[:digit:]]"), get))`
  * `summary(aov(), split=list(IV=list()))`    
    `aov()` Mit dem Argument `split` kann man sich in der Varianzanalyse-Tabelle die Tests für die einzelnen Kontraste oder auch Kombinationen von Kontrasten ausgeben lassen:    
    `summary(r.fit, split=list(factor=list(L1=1, L2=2, L3=3:4)))`
  * The function `cumsum()` also works on matrices in which case the cumulative sums are calculated per column. Use `cumprod()` for cumulative products, `cummin()` for cumulative minimums and `cummax()` for cumulative maximums.
  * `scan(what=list(first="", second=0, third=0))`, `what=NULL` to skip
  * latin squares (treatment = latin letters a, ..., g)
  * two simultaneous blocking factors, each with g levels implement only g^2 experimental units instead of g^3 possible combinations in a balanced way example: treatment = tire, blocking factor 1 = car, blocking factor 2 = tire position generate with Fisher-Yates algorithm, usual ANOVA with main effects model with treatment, row and column effects extension when levels != g -> balanced incomplete block design
  * `list2env(\<Liste\>, .GlobalEnv)`
  * R CMD command --help
  * http://stats.stackexchange.com/questions/240659/qq-plot-reference-line-not-45
  * `download.file(url, "voc_tsvs.zip")`
  * `printCoefmat()`
  * Kendall's tau-a is the covariance between sign(X1 − X2) and sign(Y1 − Y2), whereas Somers' D is the regression coefficient of sign(Y1 − Y2) with respect to sign(X1 − X2). The corresponding correlation coefficient between sign(X1 − X2) and sign(Y1 − Y2) is known as Kendall's tau-b, tau-b(XY) = sign(tau(XY)) * sqrt(Somers_D(XY) * Somers_D(YX)). If both X and Y are binary, then their concordance/discordance ratio is their odds ratio.
  * `DescTools::ScheffeTest()` -> ANCOVA
  * AUC = C = (1/(N1*N2)) * U (N1=n mit event, N2=n ohne event, U=Mann-Whitney U)
  * LaTeX: \\bigl(, \\Bigl(, \\bigr), \\Bigr) instead of \\left( \\right)
  * `asplit()`
  * GLM logistic calibration graph: two histograms, one over the other: x:fitted values in 10 bins, y:frequency, top: histogram of fitted values for failures (0), bottom: for successes (1)
  * inverse normal transformation: phi^{-1}((rank(X)-c)/(n+1-2*c)) - c="offset" in [0, 1/2], Blom: c=3/8 (Beasley 2009 doi 10.1007/s10519-009-9281-0), van der Waerden: c=0
  * An experimental implementation of hash tables is now available
  * To do this for any version of R, one can add: R_LIBS_USER=~/.local/share/R/%p-library/%v to ~/.Renviron or the Renviron.site file. This automatically expands to the platform and R x.y version early on when R starts up, e.g. ~/.local/share/R/x86_64-pc-linux-gnu-library/4.2 (see `help(".Library")`)
  * https://www.fharrell.com/post/po/ - proportional odds, Mann-Whitney-U, c-index, ordinal regression, Somers D_yx
  * [JupyterLite web kernel](https://github.com/r-wasm/jupyterlite-webr-kernel) and [https://jupyter.r-wasm.org/](https://jupyter.r-wasm.org/)
  * negativ Binomial: Mischverteilung aus Poisson-Verteilungen mit $E(X) = \phi_i \cdot \mu$, wobei $\phi_i \sim (1/\theta) \cdot \Gamma(\theta, 1)$, $\theta > 0$, dann $Var(X) = \mu + \mu^2 / \theta$
  * 3.3.10 we can use `varying` argument to rename variables in `reshapeWide()`
  * 3.3.10 `reshape(..., direction="long")` already supports specifying `varying` as a vector, and it does simplify the single variable case. So we decided to be consistent and allow it for `direction="wide"` too
  * \cite{Gelman2013} \cite{Gelman2020}?
  * Heatmap mit `geom_tile()`
  * `coord_polar()` for months / days of the year, radar / spider-chart
  * `geom_dotplot()`
