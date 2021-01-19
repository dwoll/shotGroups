fluidPage(
    fluidRow(
        bs4Box(
            title="Settings",
            width=4,
            radioButtons("effStatType1", label=h5("Measured group statistic"),
                         rangeStatSig, selected="1"),
            textInput("effN1", h5("Shots per group"),
                      value=c("3 5 10 20")),
            sliderInput("effCILevel1", label=h5("CI level"),
                        min=0, max=1, value=0.9, step=0.05),
            sliderInput("effCIWidth1", label=h5("CI width (=2*E)"),
                        min=0, max=1, value=0.2, step=0.05)
        ),
        bs4Box(
            title="Efficiency: Number of groups",
            width=8,
            h6("Background information"),
            p("Assuming a circular bivariate normal shot distribution, this
                      panel calculates the number of groups required to achieve
                      the desired coverage (level) for the confidence interval (CI)
                      for the Rayleigh \U03C3 parameter, given the number
                      of shots per group, the desired CI width, and the type of
                      measured group statistic.", br(),
              "The CI width is 2*E, where E is
                      the width as a fraction of the mean on either side."),
            verbatimTextOutput("effNGroups"),
            HTML("<ul>
                          <li>n - given number of shots per group</li>
                          <li>nGroupsReq - required number of groups with n shots each (including fractional groups)</li>
                          <li>nGroupsReqCeil - required number of groups with n shots each (full groups only)</li>
                          <li>nShotsReq - required total number of shots, assuming we're shooting groups of size n each (including fractional groups)</li>
                          <li>nShotsReqCeil - required total number of shots, assuming we're shooting groups of size n each (full groups only)</li>
                          <li>CIlevel - desired CI level (coverage probability)</li>
                          <li>CIwidth - desired CI width (as a fraction of the mean)</li>
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
