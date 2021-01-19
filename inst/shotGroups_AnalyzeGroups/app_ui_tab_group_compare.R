fluidPage(
    fluidRow(
        box(title="Settings for group comparisons",
            width=4,
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
        box(title="Compare groups",
            width=8,
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
        )
    )
)
