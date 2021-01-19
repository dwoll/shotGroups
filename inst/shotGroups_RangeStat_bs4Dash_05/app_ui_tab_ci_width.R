fluidPage(
    fluidRow(
        bs4Box(
            title="Settings",
            width=4,
            radioButtons("effStatType2", label=h5("Range statistic"),
                         rangeStatSig, selected="1"),
            #numericInput("effN2", h5("Shots per group"),
            #             min=0, max=100, step=1, value=5),
            textInput("effN2", h5("Shots per group"),
                      value=c("3 5 10 20")),
            numericInput("effNGroups2", h5("Number of groups"),
                         min=1, step=1, value=1),
            sliderInput("effCILevel2", label=h5("CI level"),
                        min=0, max=1, value=0.9, step=0.05)
        ),
        bs4Box(
            title="Efficiency: CI width",
            width=8,
            h6("Background information"),
            p("Assuming a circular bivariate normal shot distribution, this
                      panel calculates the width of the confidence interval (CI)
                      for the Rayleigh \U03C3 parameter, given the number of
                      shots per group, the number of groups, the desired coverage
                      (level) for the CI, and the type of measured group statistic.", br(),
              "The CI width is 2*E, where E is the width as a fraction of
                      the mean on either side."),
            verbatimTextOutput("effCIWidth"),
            HTML("<ul>
                          <li>n - given number of shots per group</li>
                          <li>nGroups - given number of groups with n shots each</li>
                          <li>CIlevel - desired CI level (coverage probability)</li>
                          <li>CIwidth - achievable CI width (as a fraction of the mean)</li>
                          </ul>"),
            p("Based on a Monte Carlo simulation with 2 million repetions of
                      2, ..., 100 shots per group in 1, ..., 10 groups.", br(),
              "For more information, see the",
              a("Ballistipedia entry on range statistics",
                href="http://ballistipedia.com/index.php?title=Range_Statistics"),
              ".")
        )
    )
)
