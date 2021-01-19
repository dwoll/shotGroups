fluidPage(
    fluidRow(
        bs4Box(
            title="Settings",
            width=4,
            uiOutput("hitpGroups1"),
            sliderInput("hitpLevel", label=h5("Hit probability"),
                        min=0, max=1, value=0.5, step=0.05),
            checkboxGroupInput("hitpCEPtype1", label=h5("CEP type (default: CorrNormal)"),
                               choices=CEPtypes, selected=c(1, 5)),
            checkboxInput("hitpCenter1", "Center groups", FALSE),
            checkboxInput("hitpAcc", label="CEP w/ accuracy", FALSE),
            checkboxInput("hitpDoRob1", label="Robust estimate", FALSE)
        ),
        bs4Box(
            title="Circular Error Probable",
            width=8,
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
        )
    )
)
