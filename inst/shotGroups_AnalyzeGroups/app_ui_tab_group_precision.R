fluidPage(
    fluidRow(
        box(title="Settings for group precision",
            width=4,
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
        box(title="Group precision",
            width=8,
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
            
        )
    )
)
