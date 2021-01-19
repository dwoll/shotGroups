source("helper.R")

shinyUI(fluidPage(
    #theme="bootstrap.css",
    titlePanel("Analyze shooting results using shotGroups"),
    sidebarLayout(
        #####-------------------------------------------------------------------
        ## sidebar
        #####-------------------------------------------------------------------
        sidebarPanel(width=3,
           #####----------------------------------------------------------------
           ## data input
           conditionalPanel(condition="input.task == 'Data'",
                h4("Enter data"),
                radioButtons("datIn", "",
                             list("Use built-in data"=1,
                                  "Upload file"=2,
                                  "Paste data"=3)),
                conditionalPanel(condition="input.datIn == '1'",
                                 radioButtons("builtInData", h5("Built-in data:"),
                                              dataBuiltInInv, selected="2")),
                conditionalPanel(condition="(input.datIn == '2') || (input.datIn == '3')",
                                 radioButtons("fileType", "File format:",
                                              list("OnTarget 1.*"=1,
                                                   "OnTarget 2.*, 3.*"=2,
                                                   "Silver Mountain"=3,
                                                   "ShotMarker"=4,
                                                   "Other"=5), selected="2")),
                conditionalPanel(condition="input.datIn == '2'",
                                 h5("Upload file: "),
                                 fileInput("fileUpload", "Select file:", multiple=TRUE)),
                conditionalPanel(condition="input.datIn == '3'",
                                 h5("Paste data:"),
                                 tags$textarea(id="datPaste", rows=4, cols=10, "")),
                actionButton("applyData", "Apply")
            ),

            #####---------------------------------------------------------------
            ## group shape
            conditionalPanel(condition="input.task == 'Shape'",
                h4("Group shape"),
                uiOutput("shapeGroups"),
                sliderInput("shapeBW", label=h5("2D kernel bandwith"),
                            min=0, max=5, value=0.4, step=0.1),
                radioButtons("shapeOutlier", h5("Outlier ID method"),
                             list("MCD"=1,
                                  "PCA"=2)),
                checkboxInput("shapeCenter", "Center groups", FALSE)
            ),

            #####---------------------------------------------------------------
            ## group spread / precision
            conditionalPanel(condition="input.task == 'Precision'",
                h4("Precision"),
                uiOutput("spreadGroups"),
                sliderInput("spreadCIlevel", label=h5("Confidence interval width"),
                            min=0.5, max=1, value=0.95, step=0.01),
                sliderInput("spreadCEPlevel", label=h5("CEP / conf. ellipse coverage"),
                            min=0.5, max=1, value=0.5, step=0.01),
                checkboxGroupInput("spreadCEPtype", label=h5("CEP type (default: CorrNormal)"),
                                   choices=CEPtypes, selected=c(1, 5)),
                checkboxGroupInput("spreadCItype", label=h5("Bootstrap CI type"),
                                   choices=CItypes, selected=NULL),
                checkboxInput("spreadCenter", "Center groups", FALSE)
            ),

            #####---------------------------------------------------------------
            ## group location / accuracy
            conditionalPanel(condition="input.task == 'Accuracy'",
                h4("Accuracy"),
                uiOutput("locGroups"),
                sliderInput("locLevel", label=h5("Confidence interval width"),
                            min=0.5, max=1, value=0.95, step=0.01),
                checkboxGroupInput("locCItype", label=h5("Bootstrap CI type"),
                                   choices=CItypes, selected=NULL)
            ),

            #####---------------------------------------------------------------
            ## compare groups
            conditionalPanel(condition="input.task == 'Compare groups'",
                h4("Compare groups"),
                uiOutput("compGroups"),
                sliderInput("compareCIlevel", label=h5("Confidence interval width"),
                            min=0.5, max=1, value=0.95, step=0.01),
                sliderInput("compareCEPlevel", label=h5("CEP / conf. ellipse coverage"),
                            min=0.5, max=1, value=0.5, step=0.01),
                selectInput("cmpCEPtype", label=h5("CEP type"),
                            choices=CEPtypes, selected=1),
                checkboxInput("cmpXYTL", "XY-origin top-left", TRUE),
                checkboxInput("cmpCenter", "Center groups", FALSE)
            ),

            #####---------------------------------------------------------------
            ## target plot
            conditionalPanel(condition="input.task == 'Target plot'",
                h4("Target plot"),
                uiOutput("trgtGroups"),
                checkboxInput("trgtCenter",    "Center groups", FALSE),
                checkboxInput("trgtXYTL",      "XY-origin top-left", TRUE),
                checkboxInput("trgtBB",        "Bounding box", TRUE),
                checkboxInput("trgtBBmin",     "Min-area bounding box", FALSE),
                checkboxInput("trgtBBdiag",    "Bounding-box diagonal", TRUE),
                checkboxInput("trgtMinCirc",   "Minimum-area circle", FALSE),
                checkboxInput("trgtMaxSpread", "Maximum spread", TRUE),
                checkboxInput("trgtMeanDist",  "Mean dist to center", FALSE),
                checkboxInput("trgtConfEll",   "Confidence ellipse", FALSE),
                checkboxInput("trgtCEP",       "CEP", FALSE),
                sliderInput("trgtCEPlevel",    "CEP level",
                            min=0.5, max=1, value=0.5, step=0.01),
                checkboxInput("trgtScaled",    "Scaled bullet holes", TRUE),
                sliderInput("trgtAlpha",       "Visibility bullet holes",
                            min=0, max=1, value=0.5, step=0.1),
                uiOutput("trgtTargetSel"),
                checkboxInput("trgtRingID",    "Show ring count", FALSE),
                checkboxInput("trgtValueID",   "Show calculated values", TRUE),
                uiOutput("trgtCaliberSel"),
                selectInput("trgtUnitPlot",    "Measurement unit plot",
                            choices=unitsPlot, selected=3)
            ),

            #####---------------------------------------------------------------
            ## about
            conditionalPanel(condition="input.task == 'About'",
                h4("Background information")
            ),

            #####---------------------------------------------------------------
            ## file information on bottom of sidebar
            conditionalPanel(condition="(input.task != 'About') && (input.task != 'Data')",
                h5("Loaded data"),
                uiOutput("fileInfoShort")
            )
        ),

        #####-------------------------------------------------------------------
        ## main output area
        #####-------------------------------------------------------------------
        mainPanel(
            #tags$head(tags$style(type="text/css", ".container-fluid { max-width: 12600px; }")),
            #####---------------------------------------------------------------
            ## distance to target, unit distance, unit xy-coords
            conditionalPanel(condition="(input.task != 'About') & (input.task != 'Data')",
                             uiOutput("unitDstXY")),
            tabsetPanel(
                #####-----------------------------------------------------------
                ## data input
                tabPanel("Data",
                    h6("Information from imported file(s)"),
                    uiOutput("fileInfo"),
                    # p("Set/override information on distance to target and measurement units?"),
                    # uiOutput("unitDstXY"),
                    p("For details on how to read in data, see the documentation for",
                      a("readDataOT1()",
                        href="https://www.rdocumentation.org/packages/shotGroups/functions/readDataOT1"),
                      ",",
                      a("readDataOT2()",
                        href="https://www.rdocumentation.org/packages/shotGroups/functions/readDataOT2"),
                      ",",
                      a("readDataSMT()",
                        href="https://www.rdocumentation.org/packages/shotGroups/functions/readDataSMT"),
                      ",",
                      a("readDataShotMarker()",
                        href="https://www.rdocumentation.org/packages/shotGroups/functions/readDataShotMarker"),
                      ",",
                      a("readDataMisc()",
                        href="https://www.rdocumentation.org/packages/shotGroups/functions/readDataMisc"),
                      "and the",
                      a("shotGroups vignette",
                        href="https://cran.rstudio.com/web/packages/shotGroups/vignettes/shotGroups.pdf"),
                      "section 2.1")
                ),

                #####-----------------------------------------------------------
                ## group shape
                tabPanel("Shape",
                    h6("Group shape"),
                    p("For details, see the documentation for",
                      a("groupShape()",
                        href="https://www.rdocumentation.org/packages/shotGroups/functions/groupShape"),
                      "and the",
                      a("shotGroups vignette",
                        href="https://cran.rstudio.com/web/packages/shotGroups/vignettes/shotGroups.pdf"),
                      "section 2.3"),
                    selectizeInput("shapeOut", label=h5("Select the output elements you want to see"),
                                   choices=shapeOut, multiple=TRUE,
                                   selected=c("1", "3", "4", "5", "6"), width="100%"),
                    downloadButton("saveShape", "Save results as text file"),
                    verbatimTextOutput("shape"),
                    downloadButton("saveShapePDF", "Save diagrams as pdf"),
                    uiOutput("shapePlot")
                ),

                #####-----------------------------------------------------------
                ## group spread / precision
                tabPanel("Precision",
                    h6("Precision"),
                    p("For details, see the documentation for",
                      a("groupSpread()",
                        href="https://www.rdocumentation.org/packages/shotGroups/functions/groupSpread"),
                      "and the",
                      a("shotGroups vignette",
                        href="https://cran.rstudio.com/web/packages/shotGroups/vignettes/shotGroups.pdf"),
                      "section 2.4"),
                    selectizeInput("precisionOut", label=h5("Select the output elements you want to see"),
                                   choices=spreadOut, multiple=TRUE,
                                   selected=c("19", "7", "8", "11", "12", "5", "17"), width="100%"),
                    downloadButton("saveSpread", "Save results as text file"),
                    verbatimTextOutput("spread"),
                    downloadButton("saveSpreadPDF", "Save diagrams as pdf"),
                    uiOutput("spreadPlot")
                ),

                #####-----------------------------------------------------------
                ## group location / accuracy
                tabPanel("Accuracy",
                    h6("Accuracy"),
                    p("For details, see the documentation for",
                      a("groupLocation()",
                        href="https://www.rdocumentation.org/packages/shotGroups/functions/groupLocation"),
                      "and the",
                      a("shotGroups vignette",
                        href="https://cran.rstudio.com/web/packages/shotGroups/vignettes/shotGroups.pdf"),
                      "section 2.5"),
                    selectizeInput("locationOut", label=h5("Select the output elements you want to see"),
                                   choices=locationOut, multiple=TRUE,
                                   selected=c("1", "3", "5", "6", "7"), width="100%"),
                    downloadButton("saveLocation", "Save results as text file"),
                    verbatimTextOutput("location"),
                    downloadButton("saveLocationPDF", "Save diagram as pdf"),
                    plotOutput("locationPlot", height="500px")
                ),

                #####-----------------------------------------------------------
                ## compare groups
                tabPanel("Compare groups",
                    h6("Compare groups"),
                    p("For details, see the documentation for",
                      a("compareGroups()",
                        href="https://www.rdocumentation.org/packages/shotGroups/functions/compareGroups"),
                      "and the",
                      a("shotGroups vignette",
                        href="https://cran.rstudio.com/web/packages/shotGroups/vignettes/shotGroups.pdf"),
                      "section 2.6"),
                    uiOutput("compOut"),
                    downloadButton("saveCompare", "Save results as text file"),
                    verbatimTextOutput("compare"),# style="width:5500px;"),
                    downloadButton("saveComparePDF", "Save diagrams as pdf"),
                    uiOutput("comparePlot")
                ),

                #####------------------------------------------------------------
                ## target plot
                tabPanel("Target plot",
                    h6("Target plot"),
                    p("For details, see the documentation for",
                      a("drawGroup()",
                        href="https://www.rdocumentation.org/packages/shotGroups/functions/drawGroup"),
                      "and the",
                      a("shotGroups vignette",
                        href="https://cran.rstudio.com/web/packages/shotGroups/vignettes/shotGroups.pdf"),
                      "section 3.3"),
                    downloadButton("saveTargetPDF", "Save diagram as pdf"),
                    plotOutput("targetPlot", height="600px"),
                    h6("Simulated ring count (requires a selected target)"),
                    p("For details, see the documentation for",
                      a("simRingCount()",
                        href="https://www.rdocumentation.org/packages/shotGroups/functions/simRingCount"),
                      "and the",
                      a("shotGroups vignette",
                        href="https://cran.rstudio.com/web/packages/shotGroups/vignettes/shotGroups.pdf"),
                      "section 3.4"),
                    verbatimTextOutput("simRingCount")
                ),

                #####-----------------------------------------------------------
                ## about
                tabPanel("About",
                    h6("About shotGroups"),
                    p("The", a("shotGroups", href="https://CRAN.R-project.org/package=shotGroups"),
                      "package for", a("R", href="https://www.r-project.org/"),
                      "provides functions to read in, plot,
                      statistically describe, analyze, and compare shooting data with respect
                      to group shape, precision, and accuracy. This includes graphical methods,
                      descriptive statistics, and inference tests using standard, but also
                      non-parametric and robust statistical methods. The data can be imported
                      from files produced by",
                      a("OnTarget PC and OnTarget TDS", href="https://ontargetshooting.com/tds/"), ", ",
                      a("TARAN", href="http://taran.ptosis.ch/"), ", ",
                      a("ShotMarker e-target", href="https://www.autotrickler.com/shotmarker.html"), ", ",
                      a("Silver Mountain e-target", href="https://www.silvermountaintargets.com/"), ", ",
                      "or from custom data files in text format with a similar structure.
                      For further explanations and an example walkthrough, see the",
                      a("package vignette",
                        href="https://cran.rstudio.com/web/packages/shotGroups/vignettes/shotGroups.pdf"),
                      ". Many statistical methods are described on",
                       a("Ballistipedia", href="http://ballistipedia.com/"), "."),
                    p("shotGroups and this web application are written by:", br(),
                      "Daniel", HTML("Wollschl&auml;ger"),
                      a("<dwoll@kuci.org>", href="mailto:dwoll@kuci.org"), br(),
                      "Source code shotGroups:",
                      a("https://github.com/dwoll/shotGroups/",
                        href="https://github.com/dwoll/shotGroups/"), br(),
                      "Source code shiny app:",
                      a("https://github.com/dwoll/shotGroupsApp/",
                        href="https://github.com/dwoll/shotGroupsApp/")),

                    h6("More shotGroups web applications"),
                    p("Absolute", icon("resize-horizontal", lib="glyphicon"),
                      "angular size conversions:",
                      a("http://shiny.imbei.uni-mainz.de:3838/shotGroups_AngularSize/",
                        href="shiny.imbei.uni-mainz.de:3838/shotGroups_AngularSize/"), br(),
                      "Region", icon("resize-horizontal", lib="glyphicon"),
                      "hit probability calculations:",
                      a("http://shiny.imbei.uni-mainz.de:3838/shotGroups_HitProb/",
                        href="http://shiny.imbei.uni-mainz.de:3838/shotGroups_HitProb/"), br(),
                      "Estimate Rayleigh sigma from range statistics:",
                      a("http://shiny.imbei.uni-mainz.de:3838/shotGroups_RangeStat/",
                        href="http://shiny.imbei.uni-mainz.de:3838/shotGroups_RangeStat/")),

                    h6("Acknowledgements"),
                    p("Thanks to David Bookstaber for testing, feedback and data."),

                    h6("References"),
                    p("This web application is built with R and shiny. The shotGroups package
                      uses functionality provided by the R packages boot, coin, CompQuadForm,
                      energy, mvoutlier, and robustbase:"),

                    p("Canty, A., & Ripley, B. D. (2020). boot: Bootstrap R (S-Plus) Functions.", br(),
                      a("https://cran.R-project.org/package=boot",
                        href="https://cran.R-project.org/package=boot")),
                    p("Duchesne, P., & Lafaye de Micheaux, P. (2010). Computing the distribution
                       of quadratic forms: Further comparisons between the Liu-Tang-Zhang
                       approximation and exact methods. Computational Statistics and Data
                       Analysis, 54 (4), 858-862.", br(),
                      a("https://cran.R-project.org/package=CompQuadForm",
                        href="https://cran.R-project.org/package=CompQuadForm")),
                    p("Filzmoser, P., & Gschwandtner, M. (2018). mvoutlier: Multivariate
                       outlier detection based on robust methods.", br(),
                      a("https://cran.R-project.org/package=mvoutlier",
                        href="https://cran.R-project.org/package=mvoutlier")),
                    p("Hothorn, T., Hornik, K., van de Wiel, M. A., & Zeileis, A. (2008).
                       Implementing a Class of Permutation Tests: The coin Package. Journal of
                       Statistical Software, 28 (8), 1-23.", br(),
                      a("https://www.jstatsoft.org/v28/i08/",
                        href="https://www.jstatsoft.org/v28/i08/"), br(),
                      a("https://cran.R-project.org/package=coin",
                         href="https://cran.R-project.org/package=coin")),
                    p("R Core Team (2018). R: A language and environment for statistical computing.
                       R Foundation for Statistical Computing, Vienna, Austria.", br(),
                       a("https://www.R-project.org/", br(), href="https://www.R-project.org/")),
                    p("Rizzo, M. L., & Szekely, G. J. (2019). energy: E-statistics
                      (energy statistics).", br(),
                      a("https://cran.R-project.org/package=energy",
                        href="https://cran.R-project.org/package=energy")),
                    p("Rousseeuw, P. J., Croux, C., Todorov, V., Ruckstuhl, A., Salibian-Barrera, M.,
                       Verbeke, T., & Maechler, M. (2020). robustbase: Basic Robust Statistics.", br(),
                      a("https://cran.R-project.org/package=robustbase",
                        href="https://cran.R-project.org/package=robustbase")),
                    p("RStudio Inc. (2020). shiny: Web Application Framework for R.", br(),
                       a("https://cran.R-project.org/package=shiny", href="https://cran.R-project.org/package=shiny"), br(),
                       a("https://shiny.rstudio.com/", href="https://shiny.rstudio.com/")),
                    p("Wollschlaeger, D. (2020). shotGroups: Analyze shot group data.", br(),
                       a("https://cran.R-project.org/package=shotGroups",
                         href="https://cran.R-project.org/package=shotGroups"), br(),
                       a("https://github.com/dwoll/shotGroups/",
                         href="https://github.com/dwoll/shotGroups/"), "- development version")
                ),

                id="task"
            )
        )
    )
))
