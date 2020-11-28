fluidPage(
    fluidRow(
        bs4Box(
            title="Settings",
            width=4,
            numericInput("dstTrgt1", h5("Distance to target"),
                         min=0, step=1, value=100),
            selectInput("unitDst1", h5("Measurement unit distance"),
                        choices=unitsDst, selected=2),
            textInput("angszeAbs1", h5("Absolute size"),
                      value=c("1 2 3")),
            selectInput("angszeUnitAbs1", h5("Unit absolute size"),
                        choices=unitsAbs, selected=6),
            h4("Output"),
            selectizeInput("angszeUnitAngOut1", h5("Unit angular size"),
                           choices=unitsAng, multiple=TRUE, selected=1:6)
        ),
        bs4Box(
            title="Convert absolute size to angular diameter",
            width=8,
            verbatimTextOutput("abs2Ang"),
            h6("Math"),
            p("The angle \\(\\alpha\\) subtended by an object of size
                      \\(s\\) at distance \\(d\\) can be calculated from the
                      right triangle with hypotenuse \\(d\\) and cathetus \\(s/2\\):
                      \\(\\tan\\left(\\frac{\\alpha}{2}\\right) = \\frac{s}{2}
                      \\cdot \\frac{1}{d}\\), therefore \\(\\alpha = 2 \\cdot
                      \\arctan\\left(\\frac{s}{2 d}\\right)\\)."),
            p("Assuming that the argument for \\(\\tan(\\cdot)\\) and the
                     result from \\(\\arctan(\\cdot)\\) are in radian, and that
                     distance to target \\(d\\) and object size \\(s\\) are measured
                     in the same unit, this leads to the following formulas for
                     calculating \\(\\alpha\\) in MOA, SMOA as well as \\(x\\) in
                     mrad and NATO mil based on \\(d\\) and \\(s\\)"),
            
            HTML("<ul>
                  <li>Angle \\(\\alpha\\) in degree: \\(\\alpha = \\frac{360}{2 \\pi} \\cdot 2
                      \\cdot \\arctan\\left(\\frac{s}{2 d}\\right) =
                      \\frac{360}{\\pi} \\cdot \\arctan\\left(\\frac{s}{2 d}\\right)\\)</li>
                  <li>Angle \\(\\alpha\\) in MOA: \\(\\alpha = 60 \\cdot \\frac{360}{\\pi}
                      \\cdot \\arctan\\left(\\frac{s}{2 d}\\right) = \\frac{21600}{\\pi} \\cdot
                      \\arctan\\left(\\frac{s}{2 d}\\right)\\)</li>
                  <li>Angle \\(\\alpha\\) in SMOA: By definition, size \\(s=1\\) inch at
                      \\(d = 100\\) yards (\\(= 3600\\) inch) is 1 SMOA.<br />
                      Conversion factors to/from MOA are \\(\\frac{21600}{\\pi} \\cdot
                      \\arctan\\left(\\frac{1}{7200}\\right) \\approx 0.95493\\),
                      and \\(\\frac{\\pi}{21600} \\cdot
                      \\frac{1}{\\arctan(1/7200)} \\approx 1.04720\\).<br />
                      \\(\\alpha = \\frac{\\pi}{21600} \\cdot \\frac{1}{\\arctan(1/7200)} \\cdot
                      \\frac{21600}{\\pi} \\cdot \\arctan\\left(\\frac{s}{2 d}\\right) =
                      \\frac{1}{\\arctan(1/7200)} \\cdot \\arctan\\left(\\frac{s}{2 d}\\right)\\)</li>
                  <li>Arc length \\(x\\) in rad:  \\(x = 2 \\cdot \\arctan\\left(\\frac{s}{2 d}\\right)\\).</li>
                  <li>Arc length \\(x\\) in mrad: \\(x = 2000 \\cdot \\arctan\\left(\\frac{s}{2 d}\\right)\\).<br />
                      Conversion factors to/from MOA are \\(\\frac{21600}{2000 \\pi} \\approx 3.43775\\)
                      and \\(\\frac{2000 \\pi}{21600} \\approx 0.29089\\).</li>
                  <li>Arc length \\(x\\) in NATO mil: \\(x = \\frac{6400}{\\pi} \\cdot
                      \\arctan\\left(\\frac{s}{2 d}\\right)\\).<br />
                      Conversion factors to/from MOA are \\(\\frac{21600}{6400} = 3.375\\)
                      and \\(\\frac{6400}{21600} \\approx 0.2962963\\).
                  </ul>")
        )
    )
)
