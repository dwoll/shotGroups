fluidPage(
    fluidRow(
        bs4Box(
            title="Settings",
            width=4,
            numericInput("dstTrgt2", h5("Distance to target"),
                         min=0, step=1, value=100),
            selectInput("unitDst2", h5("Measurement unit distance"),
                        choices=unitsDst, selected=2),
            textInput("angszeAng2", h5("Angular size"),
                      value=c("1 2 3")),
            selectInput("angszeUnitAng2", h5("Unit angular size"),
                        choices=unitsAng, selected=2),
            h4("Output"),
            selectizeInput("angszeUnitAbsOut2", h5("Unit absolute size"),
                           choices=unitsAbs, multiple=TRUE, selected=c(2, 6))
        ),
        bs4Box(
            title="Convert angular diameter to absolute size",
            width=8,
            verbatimTextOutput("ang2Abs"),
            h6("Math"),
            p("Absolute object size \\(s\\) can be calculated from angular
                      diameter and distance to target \\(d\\), assuming that the
                      argument for \\(\\tan(\\cdot)\\) and the
                      result from \\(\\arctan(\\cdot)\\) are in radian, and that
                      distance to target \\(d\\) and object size \\(s\\) are measured
                      in the same unit:"),
            HTML("<ul>
                  <li>From angle \\(\\alpha\\) in degree: \\(s = 2 \\cdot d \\cdot
                      \\tan\\left(\\alpha \\cdot \\frac{\\pi}{360}\\right)\\)</li>
                  <li>From angle \\(\\alpha\\) in MOA: \\(s = 2 \\cdot d \\cdot
                      \\tan\\left(\\alpha \\cdot \\frac{\\pi}{60 \\cdot 360} \\right) = 2 \\cdot d \\cdot
                      \\tan\\left(\\alpha \\cdot \\frac{\\pi}{21600}\\right)\\)</li>
                  <li>From angle \\(\\alpha\\) in SMOA: \\(s = \\frac{21600}{\\pi} \\cdot
                      \\arctan\\left(\\frac{1}{7200}\\right) \\cdot 2 \\cdot d \\cdot
                      \\tan\\left(\\alpha \\cdot \\frac{\\pi}{21600}\\right)\\)</li>
                  <li>From arc length \\(x\\) in rad: \\(s = 2 \\cdot d \\cdot \\tan\\left(x
                      \\cdot \\frac{1}{2}\\right)\\)</li>
                  <li>From arc length \\(x\\) in mrad: \\(s = 2 \\cdot d \\cdot \\tan\\left(x
                      \\cdot \\frac{1}{2000}\\right)\\)</li>
                  <li>From arc length \\(x\\) in NATO mil: \\(s = 2 \\cdot d \\cdot \\tan\\left(x
                      \\cdot \\frac{\\pi}{6400}\\right)\\)</li>
                  </ul>")
        )
    )
)
