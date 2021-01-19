fluidPage(
    fluidRow(
        box(title="Settings for group shape",
            width=4,
            uiOutput("shapeGroups"),
            sliderInput("shapeBW", label=h5("2D kernel bandwith"),
                        min=0, max=5, value=0.4, step=0.1),
            radioButtons("shapeOutlier", h5("Outlier ID method"),
                         list("MCD"=1,
                              "PCA"=2)),
            checkboxInput("shapeCenter", "Center groups", FALSE)
        ),
        box(title="Group shape",
            width=8,
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
        )
    )
)
