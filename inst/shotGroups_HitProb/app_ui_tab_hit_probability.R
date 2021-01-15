fluidPage(
    fluidRow(
        box(title="Settings",
            width=4,
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
        box(title="Hit probability",
            width=8,
            p("For details, see the documentation for",
              a("getCEP()",
                href="http://www.rdocumentation.org/packages/shotGroups/functions/getCEP"),
              "and the",
              a("shotGroups vignette",
                href="http://cran.rstudio.com/web/packages/shotGroups/vignettes/shotGroups.pdf"),
              "sections 3.2.1, 3.2.2"),
            downloadButton("saveHitProb", "Save results as text file"),
            verbatimTextOutput("CEPHitProb"),
            numericInput("hitpExtraDst2", h5("Extrapolate to different distance"),
                         min=0, step=1, value=100),
            selectInput("hitpUnitExtraDst2", h5("Measurement unit extrapolation distance"),
                        choices=unitsDst, selected="2"),
            p("For details, see the",
              a("shotGroups vignette",
                href="http://cran.rstudio.com/web/packages/shotGroups/vignettes/shotGroups.pdf"),
              "section 3.2.3."),
            verbatimTextOutput("extraHitProb")
        )
    )
)
