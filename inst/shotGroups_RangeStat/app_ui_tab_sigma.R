fluidPage(
    fluidRow(
        box(title="Settings",
            width=4,
            radioButtons("rangeStatType1", label=h5("Range statistic"),
                         rangeStat, selected="1"),
            textInput("rangeStatStats", h5("Measured values (average if > 1 group)"),
                      value=c("1 2 3")),
            numericInput("rangeStatN", h5("Shots per group"),
                         min=2, max=100, step=1, value=5),
            numericInput("rangeStatNGroups", h5("Number of groups"),
                         min=1, max=10, step=1, value=1),
            radioButtons("rangeStatSigmaCEP", label=h5("Estimate \\(\\sigma\\) or CEP?"),
                         c("Rayleigh \\(\\sigma\\)"="1", "Rayleigh CEP"="2"), selected="1"),
            conditionalPanel(condition="(input.rangeStatSigmaCEP == '2')",
                             sliderInput("rangeStatCEPLevel", label=h5("CEP level"),
                                         min=0, max=1, value=0.5, step=0.05)),
            sliderInput("rangeStatCILevel", label=h5("CI level"),
                        min=0, max=1, value=0.9, step=0.05),
            numericInput("dstTrgt1", h5("Distance to target"),
                         min=0, step=1, value=100),
            selectInput("unitDstTrgt1", h5("Measurement unit distance"),
                        choices=unitsDst, selected=2),
            selectInput("unitXY1", h5("Measurement unit coordinates"),
                        choices=unitsXY, selected=3)
        ),
        box(title=tagList("Range statistics", icon("arrow-right", lib="font-awesome"), "Rayleigh \\(\\sigma\\)"),
            width=8,
            h6("Background information"),
            p("Assuming a circular bivariate normal shot distribution, this
                      panel estimates the Rayleigh \\(\\sigma\\) parameter from
                      measured range statistics in a given number of shots per
                      group, averaged over a given number of groups.", br(),
              "Distance to target, and information on the measurement
                      unit for distance and range statistic is only used for
                      the conversion to",
              a("angular size", href="http://shiny.imbei.uni-mainz.de:3838/shotGroups_AngularSize/"),
              "."),
            verbatimTextOutput("range2sigma"),
            p("Based on a Monte Carlo simulation with 2 million repetions of
                      2, ..., 100 shots per group in 1, ..., 10 groups.", br(),
              "For more information, see the",
              a("Ballistipedia entry on range statistics",
                href="http://ballistipedia.com/index.php?title=Range_Statistics"),
              ".")
        )
    )
)
