fluidPage(
    fluidRow(
        bs4Box(
            title="Settings",
            width=4,
            textInput("angszeAbs3", h5("Absolute size"),
                      value=c("1 2 3")),
            selectInput("angszeUnitAbs3", h5("Unit absolute size"),
                        choices=unitsAbs, selected=1),
            textInput("angszeAng3", h5("Angular size"), value="1"),
            selectInput("angszeUnitAng3", h5("Unit angular size"),
                        choices=unitsAng, selected=2),
            h4("Output"),
            selectizeInput("angszeUnitDstOut3", h5("Unit distance"),
                           choices=unitsAbs, multiple=TRUE, selected=c(1, 4))
        ),
        bs4Box(
            title="Calculate distance from absolute and angular size",
            width=8,
            verbatimTextOutput("angAbs2Dist"),
            h6("Math"),
            p("Distance to target \\(d\\) can be calculated from absolute
                      object size \\(s\\) and angular size, assuming that the
                      argument for \\(\\tan(\\cdot)\\) and the
                      result from \\(\\arctan(\\cdot)\\) are in radian, and that
                      distance to target \\(d\\) and object size \\(s\\) are measured
                      in the same unit:"),
            HTML("<ul>
                  <li>From angle \\(\\alpha\\) in degree: \\(d = \\frac{s}{2} \\cdot
                      \\frac{1}{\\tan(\\alpha \\cdot \\pi/360)}\\)</li>
                  <li>From angle \\(\\alpha\\) in MOA: \\(d = \\frac{s}{2} \\cdot
                      \\frac{1}{\\tan(\\alpha \\cdot \\pi/21600)}\\)</li>
                  <li>From angle \\(\\alpha\\) in SMOA: \\(d = \\frac{s}{2} \\cdot
                      \\frac{1}{\\tan(\\alpha \\cdot \\arctan(1/7200))}\\)</li>
                  <li>From arc length \\(x\\) in rad:  \\(d = \\frac{s}{2} \\cdot
                      \\frac{1}{\\tan(x / 2)}\\)</li>
                  <li>From arc length \\(x\\) in mrad: \\(d = \\frac{s}{2} \\cdot
                      \\frac{1}{\\tan(x / 2000)}\\)</li>
                  <li>From arc length \\(x\\) in NATO mil: \\(d = \\frac{s}{2} \\cdot
                      \\frac{1}{\\tan(x \\cdot \\pi / 6400)}\\)
                  </ul>")
        )
    )
)
