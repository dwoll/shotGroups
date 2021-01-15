fluidPage(
    fluidRow(
        box(title="Settings for group accuracy",
            width=4,
            uiOutput("locGroups"),
            sliderInput("locLevel", label=h5("Confidence interval width"),
                        min=0.5, max=1, value=0.95, step=0.01),
            checkboxGroupInput("locCItype", label=h5("Bootstrap CI type"),
                               choices=CItypes, selected=NULL)
        ),
        box(title="Group accuracy",
            width=8,
            p("For details, see the documentation for",
              a("groupLocation()",
                href="http://www.rdocumentation.org/packages/shotGroups/functions/groupLocation"),
              "and the",
              a("shotGroups vignette",
                href="http://cran.rstudio.com/web/packages/shotGroups/vignettes/shotGroups.pdf"),
              "section 2.5"),
            selectizeInput("locationOut", label=h5("Select the output elements you want to see"),
                           choices=locationOut, multiple=TRUE,
                           selected=c("1", "3", "5", "6", "7"), width="100%"),
            downloadButton("saveLocation", "Save results as text file"),
            verbatimTextOutput("location"),
            downloadButton("saveLocationPDF", "Save diagram as pdf"),
            plotOutput("locationPlot", height="500px")
        )
    )
)
