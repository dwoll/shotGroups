source("helper.R")

shinyUI(fluidPage(
    #theme="bootstrap.css",
    titlePanel("Determine hit probability from region using shotGroups"),
    sidebarLayout(
        #####-------------------------------------------------------------------
        ## sidebar
        #####-------------------------------------------------------------------
        sidebarPanel(width=3,
           #####----------------------------------------------------------------
           ## data input
           conditionalPanel(condition="input.task == 'Data'",
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
            ## hit probability -> radius
            conditionalPanel(condition="/region/.test(input.task)",
                uiOutput("hitpGroups1"),
                sliderInput("hitpLevel", label=h5("Hit probability"),
                            min=0, max=1, value=0.5, step=0.05),
                checkboxGroupInput("hitpCEPtype1", label=h5("CEP type (default: CorrNormal)"),
                                   choices=CEPtypes, selected=c(1, 5)),
                checkboxInput("hitpCenter1", "Center groups", FALSE),
                checkboxInput("hitpAcc", label="CEP w/ accuracy", FALSE),
                checkboxInput("hitpDoRob1", label="Robust estimate", FALSE)
            ),

            #####---------------------------------------------------------------
            ## radius -> hit probability
            conditionalPanel(condition="/Region/.test(input.task)",
                uiOutput("hitpGroups2"),
                numericInput("hitpR", h5("Radius for circular region"),
                             min=0, step=0.1, value=1),
                selectInput("hitpUnitR", h5("Measurement unit radius"),
                            choices=hitpRUnit, selected=1),
                checkboxGroupInput("hitpCEPtype2", label=h5("CEP type (default: CorrNormal)"),
                                   choices=CEPtypes[1:5], selected=c(1, 5)),
                checkboxInput("hitpCenter2", "Center groups", FALSE),
                checkboxInput("hitpDoRob2", label="Robust estimate", FALSE)
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
            conditionalPanel(condition="(input.task != 'Data') && (input.task != 'About')", uiOutput("unitDstXY")),
            tabsetPanel(
                #####-----------------------------------------------------------
                ## data input
                tabPanel("Data",
                    h6("Information from imported file(s)"),
                    uiOutput("fileInfo"),
                    p("For details on how to read in data, see the documentation for",
                      a("readDataOT1()",
                        href="https://www.rdocumentation.org/packages/shotGroups/functions/readDataOT1"),
                      ",",
                      a("readDataOT2()",
                        href="https://www.rdocumentation.org/packages/shotGroups/functions/readDataOT2"),
                      ",",
                      a("readDataMisc()",
                        href="https://www.rdocumentation.org/packages/shotGroups/functions/readDataMisc"),
                      "and the",
                      a("shotGroups vignette",
                        href="https://cran.rstudio.com/web/packages/shotGroups/vignettes/shotGroups.pdf"),
                      "section 2.1")
                ),

                #####-----------------------------------------------------------
                ## hit probability -> radius
                tabPanel(div("Hit probability", icon("arrow-right", lib="glyphicon"), "region"),
                    h6("Circular Error Probable"),
                    p("For details, see the documentation for",
                      a("getCEP()",
                        href="https://www.rdocumentation.org/packages/shotGroups/functions/getCEP"),
                      "and the",
                      a("shotGroups vignette",
                        href="https://cran.rstudio.com/web/packages/shotGroups/vignettes/shotGroups.pdf"),
                      "sections 3.2.1, 3.2.2"),
                    downloadButton("saveRadius", "Save results as text file"),
                    verbatimTextOutput("CEPRadius"),
                    h6("Confidence Ellipse"),
                        p("For details, see the documentation for",
                          a("getConfEll()",
                            href="https://www.rdocumentation.org/packages/shotGroups/functions/getConfEll"),
                          "and the",
                          a("shotGroups vignette",
                            href="https://cran.r-project.org/web/packages/shotGroups/vignettes/shotGroups.pdf"),
                          "section 3.2.1"),
                    verbatimTextOutput("confEll"),
                    numericInput("hitpExtraDst1", h5("Extrapolate to different distance"),
                                 min=0, step=1, value=100),
                    selectInput("hitpUnitExtraDst1", h5("Measurement unit extrapolation distance"),
                                choices=unitsDst, selected=2),
                    h6("Extrapolated CEP / Confidence Ellipse"),
                    p("For details, see the",
                      a("shotGroups vignette",
                        href="https://cran.rstudio.com/web/packages/shotGroups/vignettes/shotGroups.pdf"),
                      "section 3.2.3."),
                    verbatimTextOutput("extraRadius")
                ),

                #####-----------------------------------------------------------
                ## radius -> hit probability
                tabPanel(div("Region", icon("arrow-right", lib="glyphicon"), "hit probability"),
                    h6("Hit probability"),
                    p("For details, see the documentation for",
                      a("getCEP()",
                        href="https://www.rdocumentation.org/packages/shotGroups/functions/getCEP"),
                      "and the",
                      a("shotGroups vignette",
                        href="https://cran.rstudio.com/web/packages/shotGroups/vignettes/shotGroups.pdf"),
                      "sections 3.2.1, 3.2.2"),
                    downloadButton("saveHitProb", "Save results as text file"),
                    verbatimTextOutput("CEPHitProb"),
                    numericInput("hitpExtraDst2", h5("Extrapolate to different distance"),
                                 min=0, step=1, value=100),
                    selectInput("hitpUnitExtraDst2", h5("Measurement unit extrapolation distance"),
                                choices=unitsDst, selected="2"),
                    p("For details, see the",
                      a("shotGroups vignette",
                        href="https://cran.rstudio.com/web/packages/shotGroups/vignettes/shotGroups.pdf"),
                      "section 3.2.3."),
                    verbatimTextOutput("extraHitProb")
                ),

                #####-----------------------------------------------------------
                ## about
                #####-----------------------------------------------------------
                ## about
                tabPanel("About",
                    h6("About shotGroups"),
                    p("The", a("shotGroups", href="https://cran.R-project.org/package=shotGroups"),
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
                      "."),
                    p("shotGroups and this web application are written by:", br(),
                      "Daniel", HTML("Wollschl&auml;ger"),
                      a("<dwoll@kuci.org>", href="mailto:dwoll@kuci.org"), br(),
                      "Source code shotGroups:",
                      a("https://github.com/dwoll/shotGroups/",
                        href="https://github.com/dwoll/shotGroups/"), br(),
                      "Source code shiny app:",
                      a("https://github.com/dwoll/shotGroupsHitProb/",
                        href="https://github.com/dwoll/shotGroupsHitProb/")),

                    h6("More shotGroups web applications"),
                    p("Comprehensive shot group analysis:",
                      a("http://shiny.imbei.uni-mainz.de:3838/shotGroups_AnalyzeGroups",
                        href="http://shiny.imbei.uni-mainz.de:3838/shotGroups_AnalyzeGroups/"), br(),
                      "Absolute", icon("resize-horizontal", lib="glyphicon"),
                      "angular size conversion:",
                      a("http://shiny.imbei.uni-mainz.de:3838/shotGroups_AngularSize/",
                        href="http://shiny.imbei.uni-mainz.de:3838/shotGroups_AngularSize/"), br(),
                      "Estimate Rayleigh sigma from range statistics:",
                      a("http://shiny.imbei.uni-mainz.de:3838/shotGroups_RangeStat/",
                        href="http://shiny.imbei.uni-mainz.de:3838/shotGroups_RangeStat/")),

                    h6("Acknowledgements"),
                    p("Thanks to David Bookstaber for testing, feedback and data.")
                ),

                id="task"
            )
        )
    )
))
