fluidPage(
    fluidRow(
        bs4Box(
            # inputId = "card_about_1",
            # height = "600px",
            title = "About",
            width = 6,
            # status = NULL, 
            # closable = FALSE,
            # maximizable = FALSE, 
            # collapsible = TRUE,
            p("The", a("shotGroups", href="http://CRAN.R-project.org/package=shotGroups"),
              "package for", a("R", href="http://www.r-project.org/"),
              "provides functions to read in, plot,
                      statistically describe, analyze, and compare shooting data with respect
                      to group shape, precision, and accuracy. This includes graphical methods,
                      descriptive statistics, and inference tests using standard, but also
                      non-parametric and robust statistical methods. The data can be imported
                      from files produced by",
              a("OnTarget PC and OnTarget TDS", href="http://ontargetshooting.com/tds/"), ", ",
              a("TARAN", href="http://taran.ptosis.ch/"), ", ",
              a("ShotMarker e-target", href="https://www.autotrickler.com/shotmarker.html"), ", ",
              a("Silver Mountain e-target", href="https://www.silvermountaintargets.com/"), ", ",
              "or from custom data files in text format with a similar structure.
                      For further explanations and an example walkthrough, see the",
              a("package vignette",
                href="http://cran.rstudio.com/web/packages/shotGroups/vignettes/shotGroups.pdf"),
              ". Many statistical methods are described on",
              a("Ballistipedia", href="http://ballistipedia.com/"), "."),
            p("shotGroups and this web application are written by:", br(),
              "Daniel", HTML("Wollschl&auml;ger"),
              a("<dwoll@kuci.org>", href="mailto:dwoll@kuci.org"), br(),
              a("Source code shotGroups",
                href="http://github.com/dwoll/shotGroups/"), br(),
              a("Source code shiny app",
                href="https://github.com/dwoll/shotGroups/tree/master/inst/shotGroups_AnalyzeGroups")),
            
            h6("More shotGroups web applications"),
            p("Comprehensive shot group analysis:",
              a("http://shiny.imbei.uni-mainz.de:3838/shotGroups_AnalyzeGroups",
                href="http://shiny.imbei.uni-mainz.de:3838/shotGroups_AnalyzeGroups/"), br(),
              "Region", icon("resize-horizontal", lib="glyphicon"),
              "hit probability calculations:",
              a("http://shiny.imbei.uni-mainz.de:3838/shotGroups_HitProb/",
                href="http://shiny.imbei.uni-mainz.de:3838/shotGroups_HitProb/"), br(),
              "Estimate Rayleigh \\(\\sigma\\) from range statistics:",
              a("http://shiny.imbei.uni-mainz.de:3838/shotGroups_RangeStat/",
                href="http://shiny.imbei.uni-mainz.de:3838/shotGroups_RangeStat/")),
            
            h6("Calculations"),
            p("For details of the calculations used in this app, see the documentation for",
              a("getMOA()",
                href="http://www.rdocumentation.org/packages/shotGroups/functions/getMOA"),
              "as well as for",
              a("fromMOA()",
                href="http://www.rdocumentation.org/packages/shotGroups/functions/fromMOA"),
              "and the",
              a("shotGroups vignette",
                href="http://cran.rstudio.com/web/packages/shotGroups/vignettes/shotGroups.pdf"),
              "section 3.5.")
        ),
        bs4Box(
            title="References",
            width=6,
            p("This web application is built with R, shiny, and bs4Dash.
              The shotGroups package
              uses functionality provided by the R packages boot, coin, CompQuadForm,
              energy, mvoutlier, and robustbase:"),
            
            p("Canty, A., & Ripley, B. D. (2020). boot: Bootstrap R (S-Plus) Functions.", br(),
              a("http://CRAN.R-project.org/package=boot",
                href="http://CRAN.R-project.org/package=boot")),
            p("Duchesne, P., & Lafaye de Micheaux, P. (2010). Computing the distribution
                       of quadratic forms: Further comparisons between the Liu-Tang-Zhang
                       approximation and exact methods. Computational Statistics and Data
                       Analysis, 54 (4), 858-862.", br(),
              a("http://CRAN.R-project.org/package=CompQuadForm",
                href="http://CRAN.R-project.org/package=CompQuadForm")),
            p("Filzmoser, P., & Gschwandtner, M. (2018). mvoutlier: Multivariate
                       outlier detection based on robust methods.", br(),
              a("http://CRAN.R-project.org/package=mvoutlier",
                href="http://CRAN.R-project.org/package=mvoutlier")),
            p("Granjon, D. (2019). bs4Dash: A 'Bootstrap 4' Version of 'shinydashboard'.", br(),
              a("https://CRAN.R-project.org/package=bs4Dash",
                href="https://CRAN.R-project.org/package=bs4Dash")),
            p("Hothorn, T., Hornik, K., van de Wiel, M. A., & Zeileis, A. (2008).
                       Implementing a Class of Permutation Tests: The coin Package. Journal of
                       Statistical Software, 28 (8), 1-23.", br(),
              a("http://www.jstatsoft.org/v28/i08/",
                href="http://www.jstatsoft.org/v28/i08/"), br(),
              a("http://CRAN.R-project.org/package=coin",
                href="http://CRAN.R-project.org/package=coin")),
            p("R Core Team (2020). R: A language and environment for statistical computing.
                       R Foundation for Statistical Computing, Vienna, Austria.", br(),
              a("http://www.R-project.org/", br(), href="http://www.R-project.org/")),
            p("Rizzo, M. L., & Szekely, G. J. (2019). energy: E-statistics
                      (energy statistics).", br(),
              a("http://CRAN.R-project.org/package=energy",
                href="http://CRAN.R-project.org/package=energy")),
            p("Rousseeuw, P. J., Croux, C., Todorov, V., Ruckstuhl, A., Salibian-Barrera, M.,
                       Verbeke, T., & Maechler, M. (2020). robustbase: Basic Robust Statistics.", br(),
              a("http://CRAN.R-project.org/package=robustbase",
                href="http://CRAN.R-project.org/package=robustbase")),
            p("RStudio Inc. (2020). shiny: Web Application Framework for R.", br(),
              a("http://CRAN.R-project.org/package=shiny", href="http://CRAN.R-project.org/package=shiny"), br(),
              a("http://shiny.rstudio.com/", href="http://shiny.rstudio.com/")),
            p("Wollschlaeger, D. (2020). shotGroups: Analyze shot group data.", br(),
              a("http://CRAN.R-project.org/package=shotGroups",
                href="http://CRAN.R-project.org/package=shotGroups"), br(),
              a("http://github.com/dwoll/shotGroups/",
                href="http://github.com/dwoll/shotGroups/"), "- development version")
        )
    )
)
